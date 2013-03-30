
/******************************************************************************
* MODULE     : picture.cpp
* DESCRIPTION: Abstract graphical pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "picture.hpp"

void
picture_rep::translate_origin (int dx, int dy) {
  set_origin (get_origin_x () + dx, get_origin_y () + dy);
}

void
picture_rep::copy_from (picture src) {
  copy_from (0, 0, src, 0, 0, src->get_width (), src->get_height ());
}

void
picture_rep::copy_to (picture dest) {
  copy_to (0, 0, dest, 0, 0, get_width (), get_height ());
}

void
picture_rep::copy_from (int x, int y, picture src,
                        int x1, int y1, int x2, int y2)
{
  for (int yy= y1; yy < y2; yy++)
    for (int xx= x1; xx < x2; xx++)
      set_pixel (x + xx, y + yy, src->get_pixel (xx, yy));
}

void
picture_rep::copy_to (int x, int y, picture dest,
                      int x1, int y1, int x2, int y2)
{
  for (int yy= y1; yy < y2; yy++)
    for (int xx= x1; xx < x2; xx++)
      dest->set_pixel (x + xx, y + yy, get_pixel (xx, yy));
}
