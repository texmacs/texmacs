
/******************************************************************************
* MODULE     : gs_utilities.mm
* DESCRIPTION: Utilities for Ghostscript
* COPYRIGHT  : (C) 2010 David MICHEL
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

bool
gs_supports (url image) {
  string s= suffix (image);
  if (s == "ps" || s == "eps" || s == "pdf") return true;
  return false;
}

void
gs_image_size (url image, int& w_pt, int& h_pt) {
  string cmd;
  cmd= "grep -m 1 '^%%BoundingBox: ' ";
  cmd << sys_concretize (image);
  string buf= eval_system (cmd);
  int pos= 0;
  int ok= read (buf, pos, "%%BoundingBox: ");
  if (!ok) {
#if defined (__MINGW__) || defined (__MINGW32__)
    cmd= "\"";
    cmd << get_env ("TEXMACS_PATH") << string ("\\bin\\gswin32c\" ");
#else
    cmd= "gs ";
#endif
    cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -dEPSCrop -sDEVICE=bbox ";
    cmd << sys_concretize (image);
    string buf= eval_system (cmd);
    int pos= 0;
    ok= read (buf, pos, "%%BoundingBox: ");
  }
  if (ok) {
    int x1, y1, x2, y2;
    skip_spaces (buf, pos);
    ok= ok && read_int (buf, pos, x1);
    skip_spaces (buf, pos);
    ok= ok && read_int (buf, pos, y1);
    skip_spaces (buf, pos);
    ok= ok && read_int (buf, pos, x2);
    skip_spaces (buf, pos);
    ok= ok && read_int (buf, pos, y2);
    if (ok) {
      w_pt= x2-x1;
      h_pt= y2-y1;
      cout << "Size= " << w_pt << ", " << h_pt << "\n";
      return;
    }
  }
  cerr << "TeXmacs Cannot read image file '" << image << "'"
       << " in gs_image_size" << LF;
  w_pt= 35; h_pt= 35;
}

void
gs_to_png (url image, url png, int w, int h) {
#if defined (__MINGW__) || defined (__MINGW32__)
  string cmd= "\"";
  cmd << get_env ("TEXMACS_PATH") << string ("\\bin\\gswin32c\" ");
#else
  string cmd= "gs ";
#endif
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
  cmd << "-sDEVICE=png16m -dGraphicsAlphaBits=4 -dEPSCrop ";
  cmd << "-g" << as_string (w) << "x" << as_string (h) << " ";
  int bbw, bbh;
  int rw, rh;
  gs_image_size (image, bbw, bbh);
  rw= (w*72-1)/bbw+1;
  rh= (h*72-1)/bbh+1;
  cmd << "-r" << as_string (rw) << "x" << as_string (rh) << " ";  
  cmd << "-sOutputFile=" << sys_concretize (png) << " ";
  cmd << sys_concretize (image);
  system (cmd);
}

void
gs_to_eps (url image, url eps) {
#if defined (__MINGW__) || defined (__MINGW32__)
  string cmd= "\"";
  cmd << get_env ("TEXMACS_PATH") << string ("\\bin\\gswin32c\" ");
#else
  string cmd= "gs ";
#endif
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
  cmd << "-sDEVICE=epswrite -dEPSCrop ";
  cmd << "-sOutputFile=" << sys_concretize (eps) << " ";
  cmd << sys_concretize (image);
  system (cmd);
}

void
gs_to_pdf (url doc, url pdf) {
#if defined (__MINGW__) || defined (__MINGW32__)
  string cmd= "\"";
  cmd << get_env ("TEXMACS_PATH") << string ("\\bin\\gswin32c\" ");
#else
  string cmd= "gs ";
#endif
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite ";
  cmd << "-sOutputFile=" << sys_concretize (pdf) << " ";
  cmd << sys_concretize (doc);
  cmd << " -c '[ /Title (" << as_string (tail(pdf)) << ") /DOCINFO pdfmark' ";

  // NOTE: when converting from ps to pdf the title of the document is 
  // incorrectly referring to the name of the temporary file
  // so we add some PS code to override the PDF document title with
  // the name of the PDF file.
  
  system (cmd);
}

void
tm_gs (url image) {
#if defined (__MINGW__) || defined (__MINGW32__)
  string cmd= "\"";
  cmd << get_env ("TEXMACS_PATH") << string ("\\bin\\gswin32c\" ");
#else
  string cmd= "gs ";
#endif
  cmd << "-q -sDEVICE=x11alpha -dBATCH -dNOPAUSE -dSAFER -dNOEPS ";
  cmd << sys_concretize (image);
  system (cmd);
}

#endif

