
/******************************************************************************
* MODULE     : image_files.hpp
* DESCRIPTION: image file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef IMAGE_FILES_H
#define IMAGE_FILES_H
#include "url.hpp"

void   xpm_size (url file_name, int& w, int& h);
tree   xpm_load (url file_name);
void   ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2);
string ps_load (url image);
void   image_size (url image, int& w, int& h);

#endif // defined IMAGE_FILES_H
