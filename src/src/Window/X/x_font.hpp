
/******************************************************************************
* MODULE     : x_font.hpp
* DESCRIPTION: Abstract display class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef X_FONT_H
#define X_FONT_H
struct Bitmap_rep {
  Pixmap bm;
  int width, height;
  SI xoff, yoff;
};
typedef Bitmap_rep* Bitmap;

#endif // defined X_FONT_H
