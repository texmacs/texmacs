
/******************************************************************************
* MODULE     : gs_utilities.mm
* DESCRIPTION: Utilities for Ghostscript
* COPYRIGHT  : (C) 2010-2012 David Michel, Joris van der Hoeven, Denis Raux
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_configure.hpp"
#ifdef USE_GS

#include "gs_utilities.hpp"
#include "analyze.hpp"
#include "file.hpp"

static string
gs_executable () {
#if defined (__MINGW__) || defined (__MINGW32__)
  static string cmd; // no need to resolve each time
  if (cmd == "") {
    url gs= resolve_in_path ("gswin32c");
    if (is_none (gs))
      gs= url_system (get_env ("TEXMACS_PATH")) * "bin" * "gswin32c";
    cmd= sys_concretize (gs);
  }
  return copy (cmd);
#else
  return "gs";
#endif
}

string
gs_prefix () {
  return gs_executable () * string (" ");
}

bool
gs_supports (url image) {
  string s= suffix (image);
  if (s == "ps" || s == "eps" || s == "pdf") return true;
  return false;
}

bool
gs_image_size_sub (string buf, int& w_pt, int& h_pt) {
  int pos= search_forwards ("\n%%BoundingBox: ", buf);
  if (pos < 0) pos = search_forwards ("%%BoundingBox: ", buf);
  if (pos < 0) return false;
  if (buf[pos] == '\n') pos++;
  bool ok= read (buf, pos, "%%BoundingBox: ");
  double X1, Y1, X2, Y2;
  int x1, y1, x2, y2;
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, X1) && ok;
  x1= (int) floor (X1);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, Y1) && ok;
  y1= (int) floor (Y1);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, X2) && ok;
  x2= (int) ceil (X2);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, Y2) && ok;
  y2= (int) ceil (Y2);
  if (!ok) return false;
  w_pt= x2-x1;
  h_pt= y2-y1;
  return true;
}

void
gs_image_size (url image, int& w_pt, int& h_pt) {
  string buf;
  bool err= load_string (image, buf, false);
  if (!err) {
    if ((suffix (image) == "pdf") && gs_PDFimage_size (image, w_pt, h_pt)) return; 
    if (gs_image_size_sub (buf, w_pt, h_pt)) return;
  
    string cmd= gs_prefix ();
    cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -dEPSCrop -sDEVICE=bbox ";
	cmd << sys_concretize (image);
    buf= eval_system (cmd);
  
    if (gs_image_size_sub (buf, w_pt, h_pt)) return;
  }
  convert_error << "Cannot read image file '" << image << "'"
                << " in gs_image_size" << LF;
  w_pt= 0; h_pt= 0;
}

bool
gs_PDFimage_size (url image, int& w_pt, int& h_pt) {
  if (DEBUG_CONVERT) debug_convert << "gs PDF image size :"<<LF;
  string buf;
  //bool err= load_string (image, buf, false); //already tested
  //if (!err) {
    string cmd= gs_prefix ();
    cmd << "-dNODISPLAY -q -sFile=";
	cmd << sys_concretize (image);
	cmd <<" "<<sys_concretize ("$TEXMACS_PATH/misc/convert/pdf_info.ps");
    buf= eval_system (cmd);
  /*}
  else {
  convert_error << "Cannot read PDF image file '" << image << "'"
                << " in gs_PDFimage_size" << LF;
  w_pt= 0; h_pt= 0;
  return false;
  }*/
  //if CropBox is defined, then use it, else Mediabox
  string type="CropBox";
  int pos= search_forwards ("CropBox: [", buf);
  if (pos < 0) {
	  if (DEBUG_CONVERT) debug_convert << "CropBox not found"<<LF;
	  type="MediaBox";
	  pos= search_forwards ("MediaBox: [", buf);
      if (pos < 0) {
	    if (DEBUG_CONVERT) debug_convert << "MediaBox not found"<<LF;
        return false;
	  }	
  }
  bool ok= read (buf, pos, type*": [");
  double X1, Y1, X2, Y2;
  int x1, y1, x2, y2;
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, X1) && ok;
  x1= (int) floor (X1);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, Y1) && ok;
  y1= (int) floor (Y1);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, X2) && ok;
  x2= (int) ceil (X2);
  skip_spaces (buf, pos);
  ok= read_double (buf, pos, Y2) && ok;
  y2= (int) ceil (Y2);
  if (!ok) {
	  if (DEBUG_CONVERT) debug_convert << "no box dims not found"<<LF;
	  return false;
  }
  w_pt= x2-x1;
  h_pt= y2-y1;
  pos= search_forwards ("Rotate =", buf);
  //if (pos < 0) return false;
  ok= read (buf, pos, "Rotate =");
  int rot;
  if (ok) {
     skip_spaces (buf, pos);
     ok= read_int  (buf, pos, rot) ;
     if (ok && ((rot % 180) == 90 )) {//the image is rotated : swap axes lengths
	   if (DEBUG_CONVERT) debug_convert << "Rotate ="<<rot<<LF;
	   h_pt= x2-x1;
       w_pt= y2-y1;
     } else {
	  if (DEBUG_CONVERT) debug_convert << "Rotate not found"<<LF;
	  return false;
  }
  }
  if (DEBUG_CONVERT) debug_convert << type<< "Box size ="<<w_pt<<" x "<< h_pt <<LF;
  return true;
}

void ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2);

static bool
use_converts (url image) {
#if defined(__MINGW__) || defined(__MINGW32__)
  //(void) image; return false;
  
  // the native pdf renderer now assumes convert is available...
  static bool has_image_magick = exists_in_path("conjure"); 
  // testing for "convert" would be ambiguous because it is also a WINDOWS filesystem utility
  // better test for "conjure" for the presence of imagemagick

#else
  // NOTE: determine whether we should use image magick.
  // Indeed, EPSCrop unfortunately does not correctly handle
  // non trivial offsets of bounding boxes
  static bool has_image_magick= exists_in_path ("convert");
#endif
  int bx1, by1, bx2, by2;
  ps_bounding_box (image, bx1, by1, bx2, by2);
  return has_image_magick && (bx1 != 0 || by1 != 0);
}

void
gs_to_png (url image, url png, int w, int h) {
  if (use_converts (image)) {
	if (DEBUG_CONVERT) debug_convert << "gs_to_png using convert"<<LF;
	string cmd= "convert";
	#if (defined (__MINGW__) || defined (__MINGW32__))
       cmd = sys_concretize(resolve_in_path(cmd));
    #endif  
    cmd << " -density 300x300 -geometry " << as_string (w) << "x" << as_string (h) << "! ";  
    if (suffix (image) == "pdf") cmd << "-define pdf:use-cropbox=true ";
	cmd << sys_concretize (image) << " ";
    cmd << sys_concretize (png);
    system (cmd);
  }
  else {
    if (DEBUG_CONVERT) debug_convert << "gs_to_png using gs"<<LF;
    string cmd= gs_prefix ();
    cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
    cmd << "-sDEVICE=png16m -dGraphicsAlphaBits=4 -dTextAlphaBits=4 ";
    if (suffix (image) == "pdf") {
		cmd << "-dUseCropBox ";
	} else { 
		cmd << "-dEPSCrop ";
	}
    cmd << "-g" << as_string (w) << "x" << as_string (h) << " ";
    int bbw, bbh;
    int rw, rh;
    gs_image_size (image, bbw, bbh);
    rw= (w*72-1)/bbw+1;
    rh= (h*72-1)/bbh+1;
	if (DEBUG_CONVERT) {
	debug_convert << "w="<<w<<" h="<<h<<LF;
	debug_convert << "bbw="<<bbw<<" bbh="<<bbh<<LF;
	debug_convert <<" res ="<<rw<<" * "<<rh <<LF;
    }
	cmd << "-r" << as_string (rw) << "x" << as_string (rh) << " ";  
    cmd << "-sOutputFile=" << sys_concretize (png) << " ";
    cmd << sys_concretize (image);
    system (cmd);
  }
}

void
gs_to_eps (url image, url eps) {
  if (use_converts (image)) {
    string cmd= "convert ";
	#if (defined (__MINGW__) || defined (__MINGW32__))
       cmd = sys_concretize(resolve_in_path(cmd));
    #endif  
    cmd << sys_concretize (image) << " ";
    cmd << sys_concretize (eps);
    system (cmd);
  }
  else {
    string cmd= gs_prefix ();
    cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
    cmd << "-sDEVICE=epswrite -dEPSCrop ";
    cmd << "-sOutputFile=" << sys_concretize (eps) << " ";
    cmd << sys_concretize (image);
    system (cmd);
  }
}

void
gs_to_pdf (url doc, url pdf, bool landscape, double paper_h, double paper_w) {
  string cmd= gs_prefix ();
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite ";
  if (landscape)
    cmd << "-dDEVICEWIDTHPOINTS=" << as_string ((int) (28.36*paper_h+ 0.5))
      << " -dDEVICEHEIGHTPOINTS=" << as_string ((int) (28.36*paper_w+ 0.5));
  else
    cmd << "-dDEVICEWIDTHPOINTS=" << as_string ((int) (28.36*paper_w+ 0.5))
      << " -dDEVICEHEIGHTPOINTS=" << as_string ((int) (28.36*paper_h+ 0.5));

  cmd << " -sOutputFile=" << sys_concretize (pdf) << " ";
  cmd << sys_concretize (doc);
  cmd << " -c \"[ /Title (" << as_string (tail(pdf)) << ") /DOCINFO pdfmark\" ";

  // NOTE: when converting from ps to pdf the title of the document is 
  // incorrectly referring to the name of the temporary file
  // so we add some PS code to override the PDF document title with
  // the name of the PDF file.

  system (cmd);
}

void
gs_to_ps (url doc, url ps, bool landscape, double paper_h, double paper_w) {
  string cmd= gs_prefix ();
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=ps2write ";
  if (landscape)
    cmd << "-dDEVICEWIDTHPOINTS=" << as_string ((int) (28.36*paper_h+ 0.5))
      << " -dDEVICEHEIGHTPOINTS=" << as_string ((int) (28.36*paper_w+ 0.5));
  else
    cmd << "-dDEVICEWIDTHPOINTS=" << as_string ((int) (28.36*paper_w+ 0.5))
      << " -dDEVICEHEIGHTPOINTS=" << as_string ((int) (28.36*paper_h+ 0.5));

  cmd << " -sOutputFile=" << sys_concretize (ps) << " ";
  cmd << sys_concretize (doc);
  cmd << " -c \"[ /Title (" << as_string (tail(ps)) << ") /DOCINFO pdfmark\" ";

  // NOTE: when converting from pdf to ps the title of the document is 
  // incorrectly referring to the name of the temporary file
  // so we add some PS code to override the PS document title with
  // the name of the PS file.

  system (cmd);
}

void
tm_gs (url image) {
  string cmd= gs_prefix ();
  cmd << "-q -sDEVICE=x11alpha -dBATCH -dNOPAUSE -dSAFER -dNOEPS ";
  cmd << sys_concretize (image);
  system (cmd);
}

bool
gs_check (url doc) {
  if (!exists_in_path (gs_executable ())) return true;
  array<string> cmd;
  cmd << gs_executable ();
  cmd << string ("-dNOPAUSE"); cmd << string ("-dBATCH");
  cmd << string ("-dDEBUG"); cmd << string ("-sDEVICE=nullpage");
  cmd << sys_concretize (doc);
  array<int> out; out << 1; out << 2;
  //cout << "cmd= " << cmd << LF;
  array<string> ret= evaluate_system (cmd, array<int> (), array<string> (), out);
  //cout << "ret= " << ret << LF;
  if (ret [0] != "0" || ret[2] != "") {
    //convert_error << ret[1] << LF;
    convert_error << "for file " << doc << LF;
    convert_error << ret[2] << LF;
    return false;
  }
  return true;
}
#endif
