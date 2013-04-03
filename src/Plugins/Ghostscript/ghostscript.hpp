
/******************************************************************************
* MODULE     : ghostscript.hpp
* DESCRIPTION: interface with ghostscript
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GHOSTSCRIPT_H
#define GHOSTSCRIPT_H

#ifdef X11TEXMACS

#include "url.hpp"
#include <X11/Xlib.h>
#include <X11/Xatom.h>

bool ghostscript_bugged ();
void ghostscript_run (Display* dpy, Window gs_win, Pixmap pm,
		      url image, SI w, SI h);

#endif // X11TEXMACS

#endif // GHOSTSCRIPT_H
