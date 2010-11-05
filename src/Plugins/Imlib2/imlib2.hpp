
/******************************************************************************
* MODULE     : imlib2.hpp
* DESCRIPTION: interface with Imlib2
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef IMLIB2_H
#define IMLIB2_H
#include "url.hpp"

bool imlib2_present ();
bool imlib2_supports (url u);
void imlib2_image_size (url u, int& w, int& h);

#ifdef X11TEXMACS
#include <X11/Xlib.h>
void imlib2_display (Display* dpy, Pixmap pm, url image, int w, int h,
		     double cx1, double cy1, double cx2, double cy2);
#endif

#ifdef USE_IMLIB2
#include <Imlib2.h>
#include <stdio.h>
#include <string.h>
#endif // USE_IMLIB2

#endif // IMLIB2_H
