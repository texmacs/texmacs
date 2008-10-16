
/******************************************************************************
* MODULE     : imlib2.hpp
* DESCRIPTION: interface with Imlib2
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef IMLIB2_H
#define IMLIB2_H
#include "url.hpp"

bool imlib2_present ();
bool imlib2_supports (url u);
void imlib2_image_size (url u, int& w, int& h);

#ifndef QTTEXMACS
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
