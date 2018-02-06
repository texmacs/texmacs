
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
#include "image_files.hpp"
#include "scheme.hpp" 

string
gs_system () {
#ifdef OS_MINGW
	url gs= url_system ("C:\\") * url_wildcard ("Program Files*") * url_system ("gs") * url_wildcard ("gs*")* url_system ("bin") * url_wildcard ("gswin*c.exe");
	return materialize (gs);
#else
   return "gs";
#endif
}

#ifdef GS_EXE
string
gs_embedded () {
  string cmd; // no need to resolve each time
  
  url tmp= url_system (get_env ("TEXMACS_PATH"));
  url gs= tmp * url_system (GS_EXE);
   
  if (exists (gs)) {
    cmd= concretize (gs);
  } else {
		cmd= gs_system ();
  }
  return cmd;
}
#endif

static string
gs_executable () {
#ifdef GS_EXE
  static string cmd;
	if(N (cmd) == 0) cmd= gs_embedded (); // init had to be postponed because of TEXMACS_PATH initialization
#else
  static string cmd= gs_system ();
#endif
  return cmd;
}

string
gs_prefix () {
  return string ("\"") * gs_executable () * string ("\"") * string (" ");
}

 // eps2write available starting with gs  9.14 (2014-03-26)
 // epswrite removed in gs 9.16 (2015-03-30)
string
eps_device () {
  static string dev; // no need to resolve each time
  if (dev == "") {
    string cmd= gs_prefix ()*" --version";
    string buf= var_eval_system (cmd);
    double ver;
    int pos=0;
    if (read_double (buf, pos, ver)) {
      if (DEBUG_CONVERT) debug_convert << "gs version :"<<buf<<LF;
      if (ver >= 9.14) dev="eps2write";
      else dev="epswrite";
    }
    else  convert_error << "Cannot determine gs version"<<LF;
  }
  return copy(dev);
}


bool
gs_supports (url image) {
  string s= suffix (image);
  if (s == "ps" || s == "eps" || s == "pdf") return true;
  return false;
}

void
gs_image_size (url image, int& w_pt, int& h_pt) {
  bool ok;
  if (suffix (image) == "pdf") 
    ok= gs_PDFimage_size (image, w_pt, h_pt);
  else {
    if (DEBUG_CONVERT) debug_convert << "gs eps image size :"<<LF;
    int x1,y1,x2,y2;
    string buf;
    ok= !load_string (image, buf, false);
    if (ok) {
      //try finding Bounding box in file:
      ok= ps_read_bbox (buf, x1, y1, x2, y2);
      if (!ok) {
        // bbox not found ask gs to compute one :
        string cmd= gs_prefix ();
        cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=bbox ";
        //Note: bbox device does a "smart" job of finding the cropbox on its own removing blank margins (*even* on eps files)
        //this is ok if we are reading a ps page
        // real eps pages with proper bounding boxes have been recognized before this and will have their BoundingBox respected
        cmd << sys_concretize (image);
        buf= eval_system (cmd);
        if (DEBUG_CONVERT) debug_convert << "gs cmd :"<<cmd<<LF
          <<"answer :"<< buf ;
        ok= ps_read_bbox (buf, x1, y1, x2, y2);
      }
      if (ok) {
        w_pt= x2-x1;
        h_pt= y2-y1;
        set_imgbox_cache(image->t, w_pt, h_pt, x1, y1);
      }
    }
  }
  if (!ok) { 
  convert_error << "Cannot read image file '" << image << "'"
                << " in gs_image_size" << LF;
  w_pt= 0; h_pt= 0;
  }
}

void 
gs_fix_bbox (url eps, int x1, int y1, int x2, int y2) {
// used to restore appropriate bounding box of an eps file in case epswrite 
// spuriously changes it (see gs_to_eps)
  string outbuf, buf;
  int inx1, iny1, inx2, iny2;
  bool err = load_string (eps, buf, false);
  if (!err) {
    if (DEBUG_CONVERT) debug_convert<< "fix_bbox input bbox : ";
    if ( !ps_read_bbox (buf, inx1, iny1, inx2, iny2 ) ) 
      return; //bbox not found... should not occur
    if (inx1!=x1 || iny1!=y1 || inx2!=x2 || iny2!=y2) {
      int pos= search_forwards ("%%BoundingBox:", buf);
      pos += 14;
      outbuf << buf(0, pos)
        << " " << as_string(x1) << " " << as_string(y1) 
        << " " << as_string(x2) << " " << as_string(y2) << "\n";
      skip_line (buf, pos);
      if (read (buf, pos, "%%HiResBoundingBox:")) skip_line (buf, pos);
      outbuf << buf(pos, N(buf));
      save_string (eps, outbuf, true);
      if (DEBUG_CONVERT) 
        debug_convert<< "restored bbox : " << ps_read_bbox (outbuf, x1, y1, x2, y2 )<<LF;
    }  
    set_imgbox_cache(eps->t, x2-x1, y2-y1, x1, y1);
  } 
}

string
default_pdf_version () {
  return "1.4";
}

string
pdf_version () {
  string version= get_preference ("texmacs->pdf:version");
  if (version == "1.4" || version == "1.5" ||
      version == "1.6" || version == "1.7") return version;
  return default_pdf_version ();
}

string
pdf_version (url image) {
  string buf;
  bool ok= !load_string (image, buf, false);
  int n= N(buf);
  if (!ok || n < 5 || buf(0,5) != "%PDF-") {
    std_error << "Cannot determine PDF version of \"" << image << "\"." << LF;
    return default_pdf_version ();
  }
  int pos= 5;
  while (n > pos && is_numeric (buf[pos])) pos++;
  string v= buf(5,pos);
  if (!is_double(v)) {
    std_error << "Cannot determine PDF version of \"" << image << "\"." << LF;
    return default_pdf_version ();;
  }
  //cout << "PDF version found for \"" << image << "\": " << v << LF;
  return v;
}

bool
gs_PDFimage_size (url image, int& w_pt, int& h_pt) {
  if (DEBUG_CONVERT) debug_convert << "gs PDF image size :"<<LF;
  string buf;
  string cmd= gs_prefix ();
  cmd << "-dNODISPLAY -q -sFile=";
  cmd << sys_concretize (image);
  cmd <<" "<<sys_concretize ("$TEXMACS_PATH/misc/convert/pdf_info.ps");
  buf= eval_system (cmd);
  if (DEBUG_CONVERT) debug_convert << "gs cmd :"<<cmd<<LF
    <<"answer :"<< buf ;
  //if CropBox is defined, then use it, else Mediabox
  string type="CropBox";
  int pos= search_forwards ("CropBox: [", buf);
  if (pos < 0) {
    type="MediaBox";
    pos= search_forwards ("MediaBox: [", buf);
      if (pos < 0) {
      if (DEBUG_CONVERT) debug_convert << "CropBox|MediaBox not found"<<LF;
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
    if (DEBUG_CONVERT) debug_convert << "box dims not found"<<LF;
    return false;
  }
  w_pt= x2-x1;
  h_pt= y2-y1;
  pos= search_forwards ("Rotate =", buf);
  ok= read (buf, pos, "Rotate =");
  int rot;
  if (ok) {
    skip_spaces (buf, pos);
    ok = read_int  (buf, pos, rot) ;
    if (ok) {
      rot = rot%360;
      if (rot < 0) rot +=360;
      if ((rot % 180) == 90 ) {//the image is rotated : swap axes lengths
        if (DEBUG_CONVERT) debug_convert << "Rotate ="<<rot<<LF;
        h_pt= x2-x1;
        w_pt= y2-y1;
      }
    } 
    else {
      if (DEBUG_CONVERT) debug_convert << "Rotate not found"<<LF;
      return false;
    }
  }
  if (DEBUG_CONVERT) debug_convert << type<< " size ="<<w_pt<<" x "<< h_pt <<LF;
  return true;
}

bool
gs_PDF_EmbedAllFonts (url image, url pdf) {
  if (DEBUG_CONVERT) debug_convert << "gs_PDF_EmbedAllFonts" << LF;
  if (!exists (image)) return false;
  array<string> cmd;
  cmd << gs_executable ();
  cmd << string ("-dNOPAUSE"); cmd << string ("-dBATCH");
  cmd << string ("-dQUIET"); cmd << string ("-dSAFER");
  cmd << string ("-sDEVICE=pdfwrite");
  cmd << string ("-dPDFSETTINGS=/prepress");
  cmd << string ("-dEmbedAllFonts=true");
  cmd << string ("-dCompatibilityLevel=") * pdf_version ();
  cmd << string ("-sOutputFile=") * concretize (pdf);
  cmd << concretize (image);
  // cout << cmd << LF;
  array<int> out; out << 1; out << 2;
  array<string> ret= evaluate_system (cmd, array<int> (), array<string> (), out);
  // cout << "ret= " << ret << LF;
  if (ret [0] != "0" || ret[2] != "") {
    convert_warning << "cannot embed all fonts for file " << image << LF;
    convert_warning << ret[1] << LF;
    convert_warning << ret[2] << LF;
    return false;
  }
  return true;
}

bool
gs_to_png (url image, url png, int w, int h) { //Achtung! w,h in pixels
  string cmd;
  if (DEBUG_CONVERT) debug_convert << "gs_to_png using gs"<<LF;
  cmd= gs_prefix ();
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
  cmd << "-sDEVICE=png16m -dGraphicsAlphaBits=4 -dTextAlphaBits=4 ";
  cmd << "-g" << as_string (w) << "x" << as_string (h) << " ";
  cmd << "-sOutputFile=" << sys_concretize (png) << " ";
  int bbw, bbh;
  int rw, rh;
  int bx1, by1, bx2, by2;
  if (suffix(image) == "pdf") 
    image_size (image, bbw, bbh);
    //don't call gs_PDFimage_size 
    //in order to benefit from caching
  else {
    ps_bounding_box (image, bx1, by1, bx2, by2); //same comment
    bbw=bx2-bx1;
    bbh=by2-by1;
  }
  rw=(w*72)/bbw;
  rh=(h*72)/bbh;
  cmd << "-r" << as_string (rw) << "x" << as_string (rh) << " ";  
  
  if (DEBUG_CONVERT) debug_convert << "w="<<w<<" h="<<h<<LF
      << "bbw="<<bbw<<" bbh="<<bbh<<LF
      <<" res ="<<rw<<" * "<<rh <<LF;
  
  if (suffix(image) == "pdf") {
    cmd << "-dUseCropBox "; // old gs versions (<9.0 ?) fail if CropBox not explicitly defined
    cmd << sys_concretize (image);
  }
  else {
    //don't use -dEPSCrop which works incorrectly if (bx1 != 0 || by1 != 0)
    cmd << "-c \" "<< as_string (-bx1) << " "<< as_string (-by1) <<" translate gsave \"  -f "
            << sys_concretize (image) << " -c \" grestore \"";    
  }
  string ans= eval_system (cmd);
  if (DEBUG_CONVERT) debug_convert << cmd <<LF
    <<"answer :"<<ans <<LF;
  if (! exists(png)) {
    convert_error << "gs_to_png failed for "<< image <<LF;
    return false;
    }
  return true;
}

void
gs_to_eps (url image, url eps) { //this should be used mostly for pdf->eps conversion.
  string cmd;
  int bx1, by1, bx2, by2; // bounding box
  if (DEBUG_CONVERT) debug_convert << "gs_to_eps"<<LF;
  cmd= gs_prefix ();
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
  cmd << "-sDEVICE="<<eps_device ();
  cmd << " -sOutputFile=" << sys_concretize (eps) << " ";
  if (suffix(image) == "pdf") {
    image_size (image, bx2, by2);
    bx1=by1=0;
    cmd << "-dUseCropBox "
      << " -dDEVICEWIDTHPOINTS=" << as_string (bx2)
      << " -dDEVICEHEIGHTPOINTS=" << as_string (by2)<<" "
      << sys_concretize (image);
  }  
  else {
    ps_bounding_box (image, bx1, by1, bx2, by2);
    cmd << " -dDEVICEWIDTHPOINTS=" << as_string (bx2-bx1)
      << " -dDEVICEHEIGHTPOINTS=" << as_string (by2-by1)<<" ";
    //don't use -dEPSCrop which works incorrectly if (bx1 != 0 || by1 != 0)
    cmd << "-c \" "<< as_string (-bx1) << " " << as_string (-by1) 
      << " translate gsave \" "
      << sys_concretize (image)
      << " -c \" grestore \"";     
  }
  string ans= eval_system (cmd);
  if (DEBUG_CONVERT) debug_convert << cmd <<LF
    <<"answer :"<<ans <<LF
    <<"eps generated? "<< exists(eps)<<LF;
  // eps(2)write and bbox devices do a "smart" job of finding the boundingbox on their own,
  // possibly changing the original margins/aspect ratio defined by the pdf CropBox|MediaBox
  // here were restore the original size.
  gs_fix_bbox (eps, 0, 0, bx2-bx1, by2-by1);
}

// This conversion is appropriate for eps images
// (originally implemented in pdf_image_rep::flush)
void  
gs_to_pdf (url image, url pdf, int w, int h) {
  string cmd;
  if (DEBUG_CONVERT) debug_convert << "(eps) gs_to_pdf"<<LF;
  string s= suffix (image);    
  // take care of properly handling the bounding box
  // the resulting pdf image will always start at 0,0.

  int bx1, by1, bx2, by2; // bounding box
  ps_bounding_box(image, bx1, by1, bx2, by2);
  double scale_x = w/((double)(bx2-bx1));
  double scale_y = h/((double)(by2-by1));

  cmd= gs_prefix();
  cmd << " -dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite ";
  cmd << "-dCompatibilityLevel=" << pdf_version () << " ";
  cmd << " -sOutputFile=" << sys_concretize(pdf) << " ";
  cmd << " -c \" << /PageSize [ " << as_string(bx2-bx1) << " " << as_string(by2-by1)
    << " ] >> setpagedevice gsave  "
    << as_string(-bx1) << " " << as_string(-by1) << " translate "
    << as_string(scale_x) << " " << as_string(scale_y) << " scale \"";
  cmd << " -f " << sys_concretize (image);
  cmd << " -c \" grestore \"  ";
  // debug_convert << cmd << LF;
  system(cmd);
  if (DEBUG_CONVERT) debug_convert << cmd <<LF
    <<"pdf generated? "<< exists(pdf)<<LF;
}

// This conversion is appropriate for printed pages
void  
gs_to_pdf (url doc, url pdf, bool landscape, double paper_h, double paper_w) {
  if (DEBUG_CONVERT) debug_convert << "(ps page) gs_to_pdf"<<LF;
  string cmd= gs_prefix ();
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite ";
  cmd << "-dCompatibilityLevel=" << pdf_version () << " ";
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
  if (DEBUG_CONVERT) debug_convert << cmd <<LF
    <<"pdf generated? "<< exists(pdf)<<LF;
}

void
gs_to_ps (url doc, url ps, bool landscape, double paper_h, double paper_w) {
  if (DEBUG_CONVERT) debug_convert << "gs_to_ps"<<LF;
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
  if (DEBUG_CONVERT) debug_convert << cmd <<LF
    <<"ps generated? "<< exists(ps)<<LF;
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
  if (!exists (gs_executable ()) && !exists_in_path (gs_executable ())) return true;
  array<string> cmd;
  cmd << gs_executable ();
  cmd << string ("-dNOPAUSE"); cmd << string ("-dBATCH");
  cmd << string ("-dDEBUG"); cmd << string ("-sDEVICE=nullpage");
  cmd << concretize (doc);
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
