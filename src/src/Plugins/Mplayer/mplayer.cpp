
/******************************************************************************
* MODULE     : mplayer.cpp
* DESCRIPTION: interface with mplayer
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Mplayer/mplayer.hpp"
#include "file.hpp"
#include <X11/Xlib.h>
#include <X11/Xatom.h>

bool
supports_mplayer () {
  return exists_in_path ("mplayer");
}

void
mplayer_play_sound (url u) {
  system ("mplayer", u, "> /dev/null 2> /dev/null &");
}

void
mplayer_play_video (url u) {
  system ("mplayer", u, "> /dev/null 2> /dev/null &");
}
