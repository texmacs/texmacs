
/******************************************************************************
* MODULE     : mplayer.hpp
* DESCRIPTION: interface with mplayer
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MPLAYER_H
#define MPLAYER_H
#include "url.hpp"

bool supports_mplayer ();
void mplayer_play_sound (url u);
void mplayer_play_video (url u);

#endif // MPLAYER_H
