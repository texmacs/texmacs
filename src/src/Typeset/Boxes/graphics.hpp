
/******************************************************************************
* MODULE     : graphics.hpp
* DESCRIPTION: construction routines for graphical boxes
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GRAPHICS_H
#define GRAPHICS_H
#include "boxes.hpp"
#include "frame.hpp"
#include "curve.hpp"
#include "grid.hpp"
#include "gui.hpp"

box graphics_box (path ip, array<box> bs, frame f, grid g,
		  point lim1, point lim2);
box graphics_group_box (path ip, array<box> bs);
box text_at_box (path ip, box b, SI x, SI y, SI hx, SI hy, SI axis, SI pad);
box point_box (path ip, point p, SI r, pencil pen, brush br, string style);
box curve_box (path ip, curve c, double portion, pencil pen,
	       array<bool> style, array<point> motif, SI style_unit,
	       brush fill_br, array<box> arrows);
box spacial_box (path ip, spacial obj);
box grid_box (path ip, grid g, frame f, SI un, point lim1, point lim2);

#endif // defined GRAPHICS_H
