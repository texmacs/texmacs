
/******************************************************************************
* MODULE     : image_files.hpp
* DESCRIPTION: image file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef IMAGE_FILES_H
#define IMAGE_FILES_H
#include "url.hpp"

tree          xpm_load (url file_name);
void          xpm_size (url file_name, int& w, int& h);
array<string> xpm_colors (tree t);
array<SI>     xpm_hotspot (tree t);
bool          ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2, bool set_default=true);
bool          ps_read_bbox (string buf, int& x1, int& y1, int& x2, int& y2 );
void          set_imgbox_cache(tree t, int w,  int h, int xmin=0, int ymin=0);
void          clear_imgbox_cache(tree t);
string 	      ps_load (url image, bool conv=true);
void          image_size (url image, int& w, int& h);
void          pdf_image_size (url image, int& w, int& h);
void          svg_image_size (url image, int& w, int& h);
void          image_to_eps (url image, url eps, int w_pt= 0, int h_pt= 0, int dpi= 0);
void          image_to_pdf (url image, url eps, int w_pt= 0, int h_pt= 0, int dpi= 0);
string        image_to_psdoc (url image);
void          image_to_png (url image, url png, int w= 0, int h= 0);
bool          call_scm_converter(url image, url dest);
void          call_imagemagick_convert(url image, url dest, int w_pt=0, int h_pt=0, int dpi=72);
bool          imagemagick_image_size(url image, int& w, int& h, bool pt_units=true);
bool          has_image_magick();
string        imagemagick_cmd();
void          native_image_size (url image, int& w, int& h);
void          apply_effect (tree eff, array<url> src, url dest, int w, int h);

#endif // defined IMAGE_FILES_H
