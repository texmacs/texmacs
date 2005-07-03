
/******************************************************************************
* MODULE     : mplayer.cpp
* DESCRIPTION: interface with mplayer
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
