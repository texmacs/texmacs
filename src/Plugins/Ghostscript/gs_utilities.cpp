
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

string
gs_prefix () {
#if defined (__MINGW__) || defined (__MINGW32__)
  static string cmd; // no need to resolve each time
  if (cmd == "") {
    url gs= resolve_in_path ("gswin32c");
    if(is_none(gs))
      gs= url_system (get_env ("TEXMACS_PATH")) * "bin" * "gswin32c";
    cmd= sys_concretize (gs) * " ";
  }
  return cmd;
#else
  return "gs ";
#endif
}

bool
gs_supports (url image) {
  string s= suffix (image);
  if (s == "ps" || s == "eps" || s == "pdf") return true;
  return false;
}

bool
gs_image_size_sub (string buf, int& w_pt, int& h_pt) {
  int pos= search_forwards (buf, "\n%%BoundingBox: ");
  if (pos < 0) search_forwards (buf, "%%BoundingBox: ");
  if (pos < 0) return false;
  if (buf[pos] == '\n') pos++;
  bool ok= read (buf, pos, "%%BoundingBox: ");
  int x1, y1, x2, y2;
  skip_spaces (buf, pos);
  ok= ok && read_int (buf, pos, x1);
  skip_spaces (buf, pos);
  ok= ok && read_int (buf, pos, y1);
  skip_spaces (buf, pos);
  ok= ok && read_int (buf, pos, x2);
  skip_spaces (buf, pos);
  ok= ok && read_int (buf, pos, y2);
  if (!ok) return false;
  w_pt= x2-x1;
  h_pt= y2-y1;
  return true;
}

void
gs_image_size (url image, int& w_pt, int& h_pt) {
  string buf;
  bool err= load_string (image, buf, false);
  if (!err && gs_image_size_sub (buf, w_pt, h_pt)) return;
  if (!err) {
    string cmd= gs_prefix ();
    cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -dEPSCrop -sDEVICE=bbox ";
    cmd << sys_concretize (image);
    buf= eval_system (cmd);
  }
  if (!err && gs_image_size_sub (buf, w_pt, h_pt)) return;
  cerr << "TeXmacs Cannot read image file '" << image << "'"
       << " in gs_image_size" << LF;
  w_pt= 0; h_pt= 0;
}

void ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2);

static bool
use_converts (url image) {
#ifdef __MINGW__
  (void) image; return false;
#else
  // NOTE: determine whether we should use image magick.
  // Indeed, EPSCrop unfortunately does not correctly handle
  // non trivial offsets of bounding boxes
  static bool has_image_magick= exists_in_path ("convert");
  int bx1, by1, bx2, by2;
  ps_bounding_box (image, bx1, by1, bx2, by2);
  return has_image_magick && (bx1 != 0 || by1 != 0);
#endif
}

void
gs_to_png (url image, url png, int w, int h) {
  if (use_converts (image)) {
    string cmd= "convert ";
    cmd << "-geometry " << as_string (w) << "x" << as_string (h) << "! ";  
    cmd << sys_concretize (image) << " ";
    cmd << sys_concretize (png);
    system (cmd);
  }
  else {
    string cmd= gs_prefix ();
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
}

void
gs_to_eps (url image, url eps) {
  if (use_converts (image)) {
    string cmd= "convert ";
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
gs_to_pdf (url doc, url pdf) {
  string cmd= gs_prefix ();
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -sDEVICE=pdfwrite ";
  cmd << "-sOutputFile=" << sys_concretize (pdf) << " ";
  cmd << sys_concretize (doc);
  cmd << " -c \"[ /Title (" << as_string (tail(pdf)) << ") /DOCINFO pdfmark\" ";

  // NOTE: when converting from ps to pdf the title of the document is 
  // incorrectly referring to the name of the temporary file
  // so we add some PS code to override the PDF document title with
  // the name of the PDF file.

  system (cmd);
}

void
tm_gs (url image) {
  string cmd= gs_prefix ();
  cmd << "-q -sDEVICE=x11alpha -dBATCH -dNOPAUSE -dSAFER -dNOEPS ";
  cmd << sys_concretize (image);
  system (cmd);
}

#endif
