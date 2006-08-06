
/******************************************************************************
* MODULE     : mplayer.hpp
* DESCRIPTION: interface with mplayer
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef MPLAYER_H
#define MPLAYER_H
#include "url.hpp"

bool supports_mplayer ();
void mplayer_play_sound (url u);
void mplayer_play_video (url u);

#endif // MPLAYER_H
