
/******************************************************************************
* MODULE     : ghostscript.hpp
* DESCRIPTION: interface with ghostscript
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef GHOSTSCRIPT_H
#define GHOSTSCRIPT_H
#include "url.hpp"
#ifdef OS_WIN32
#include "X11/Xlib.hpp"
#include "X11/Xatom.hpp"
#else
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#endif

bool ghostscript_bugged ();
void ghostscript_run (Display* dpy, Window gs_win, Pixmap pm,
		      url image, SI w, SI h, int x1, int y1, int x2, int y2);

#endif // GHOSTSCRIPT_H
