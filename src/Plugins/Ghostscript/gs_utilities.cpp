
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

void
gs_image_size (url image, int& w_pt, int& h_pt) {
#if defined (__MINGW__) || defined (__MINGW32__)
  string cmd= get_env ("TEXMACS_PATH") * string ("\\bin\\gswin32c ");
#else
  string cmd= "gs ";
#endif
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER -dEPSCrop -sDEVICE=bbox ";
  cmd << sys_concretize (image);
  string buf= eval_system (cmd);
  int pos= 0;
  int ok= read (buf, pos, "%%BoundingBox: ");
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
      return;
    }
  }
  cerr << "Cannot read image file '" << image << "'" << LF;
  w_pt= 35; h_pt= 35;
}

void
gs_to_png (url image, url png, int w, int h) {
#if defined (__MINGW__) || defined (__MINGW32__)
  string cmd= get_env ("TEXMACS_PATH") * string ("\\bin\\gswin32c ");
#else
  string cmd= "gs ";
#endif
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
  cmd << "-sDEVICE=png16m -dGraphicsAlphaBits=4 -dEPSCrop ";
  cmd << "-g" << as_string (w) << "x" << as_string (h) << " ";
  int bbw, bbh;
  double rw, rh;
  gs_image_size (image, bbw, bbh);
  rw= (double)w*72/bbw;
  rh= (double)h*72/bbh;
  if ((int)rw < rw) rw= (int)rw+1; else rw= (int)rw;
  if ((int)rh < rh) rh= (int)rh+1; else rh= (int)rh;
  cmd << "-r" << as_string (rw) << "x" << as_string (rh) << " ";  
  cmd << "-sOutputFile=" << sys_concretize (png) << " ";
  cmd << sys_concretize (image);
  system (cmd);
}

void
gs_to_eps (url image, url eps) {
#if defined (__MINGW__) || defined (__MINGW32__)
  string cmd= get_env ("TEXMACS_PATH") * string ("\\bin\\gswin32c ");
#else
  string cmd= "gs ";
#endif
  cmd << "-dQUIET -dNOPAUSE -dBATCH -dSAFER ";
  cmd << "-sDEVICE=epswrite -dEPSCrop ";
  cmd << "-sOutputFile=" << sys_concretize (eps) << " ";
  cmd << sys_concretize (image);
  system (cmd);
}

#endif

