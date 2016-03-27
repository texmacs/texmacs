
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

 // eps2write available starting with gs  9.14 (2014-03-26)
 // epswrite removed in gs 9.16 (2015-03-30)
string
eps_device () {
  static string dev; // no need to resolve each time
  if (dev == "") {
    string cmd= gs_prefix ()*" -v";
    string buf= eval_system (cmd);
    int pos= search_forwards ("Ghostscript", buf);
    bool ok=(pos < 0) ?false:true;
    pos+=11;
    skip_spaces (buf, pos);
    double ver;
    ok= ok&&read_double (buf, pos, ver);
    if (ok) {
      if (DEBUG_CONVERT) debug_convert << "gs version :"<<as_string(ver)<<LF;
      if (ver*100>=914) dev="eps2write";
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

bool
gs_image_size_sub (string buf, int& w_pt, int& h_pt) {
  int x1,y1,x2,y2; 
  if  (!ps_read_bbox (buf, x1, y1, x2,y2 )) return false;
  w_pt= x2-x1;
  h_pt= y2-y1;
  return true;
}

void
gs_image_size (url image, int& w_pt, int& h_pt) {
  if ((suffix (image) == "pdf") && gs_PDFimage_size (image, w_pt, h_pt) ) return;
  else {
    string buf;
    bool err= load_string (image, buf, false);
    if (!err) {
      if (DEBUG_CONVERT) debug_convert << "gs eps image size :"<<LF;
      //try finding Bounding box in file:
      if (gs_image_size_sub (buf, w_pt, h_pt)) return;
      //if not found ask gs to compute one :
      string cmd= gs_prefix ();
      cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=bbox ";
      //Note: bbox device does a "smart" job of finding the cropbox on its own removing blank margins (*even* on eps files)
      //this is ok if we are reading a ps page
      // real eps pages with proper bounding boxes have been recognized before this and will have their BoundingBox respected
      cmd << sys_concretize (image);
      buf= eval_system (cmd);
      if (DEBUG_CONVERT) debug_convert << "gs cmd :"<<cmd<<LF
        <<"answer :"<< buf ;
      if (gs_image_size_sub (buf, w_pt, h_pt)) return;
    }
  } 
  convert_error << "Cannot read image file '" << image << "'"
                << " in gs_image_size" << LF;
  w_pt= 0; h_pt= 0;
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
    ok= read_int  (buf, pos, rot) ;
    if (ok && ((rot % 180) == 90 )) {//the image is rotated : swap axes lengths
      if (DEBUG_CONVERT) debug_convert << "Rotate ="<<rot<<LF;
      h_pt= x2-x1;
      w_pt= y2-y1;
    } 
    else {
      if (DEBUG_CONVERT) debug_convert << "Rotate not found"<<LF;
      return false;
    }
  }
  if (DEBUG_CONVERT) debug_convert << type<< " size ="<<w_pt<<" x "<< h_pt <<LF;
  return true;
}

void
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
    rw=(int) ceil((double) (w*72)/bbw);
    rh=(int) ceil((double) (h*72)/bbh);
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
      <<"answer :"<<ans <<LF
      <<"png generated? "<< exists(png)<<LF;
}

void
gs_to_eps (url image, url eps) { //this should be used mostly for pdf->eps conversion.
  string cmd;
  if (DEBUG_CONVERT) debug_convert << "gs_to_eps"<<LF;
    cmd= gs_prefix ();
    cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
    cmd << "-sDEVICE=ps2write"; 
    //Note: eps(2)write and bbox devices do a "smart" job of finding the cropbox on their own, generally changing
    // the margins/aspect ratio of the image defined by the pdf CropBox|MediaBox
    //in spite of its name ps2write does writes the Bounding box. ps2write has been available since 2010.
    cmd << " -sOutputFile=" << sys_concretize (eps) << " ";
    if (suffix(image) == "pdf") {
      cmd << "-dUseCropBox ";
      cmd << sys_concretize (image);
    }  
    else {
      int bx1, by1, bx2, by2; // bounding box
      ps_bounding_box (image, bx1, by1, bx2, by2);
      cmd << "-dDEVICEWIDTHPOINTS=" << as_string (bx2-bx1)
        << " -dDEVICEHEIGHTPOINTS=" << as_string (by2-by1)<<" ";
     //don't use -dEPSCrop which works incorrectly if (bx1 != 0 || by1 != 0)
      cmd << "-c \" "<< as_string (-bx1) << " "<< as_string (-by1) <<" translate gsave \" "
           << sys_concretize (image)<< " -c \" grestore \"";       
  }
    string ans= eval_system (cmd);
  if (DEBUG_CONVERT) debug_convert << cmd <<LF
      <<"answer :"<<ans <<LF
      <<"eps generated? "<< exists(eps)<<LF;
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
