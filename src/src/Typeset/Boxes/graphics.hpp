
/******************************************************************************
* MODULE     : graphics.hpp
* DESCRIPTION: construction routines for graphical boxes
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef GRAPHICS_H
#define GRAPHICS_H
#include "boxes.hpp"
#include "Graphics/frame.hpp"
#include "Graphics/curve.hpp"

box graphics_box (path ip, array<box> bs, SI x1, SI y1, SI x2, SI y2);
box point_box (path ip, frame f, point p, color col);
box curve_box (path ip, frame f, curve c, double width, color col);

#endif // defined GRAPHICS_H
