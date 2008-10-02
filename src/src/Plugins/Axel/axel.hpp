
/******************************************************************************
* MODULE     : axel.hpp
* DESCRIPTION: interface with Axel
* COPYRIGHT  : (C) 2008  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef AXEL_H
#define AXEL_H

bool axel_present ();
void axel_test ();

#ifdef USE_AXEL
#include <Axel.h>
#endif // USE_AXEL

#endif // AXEL_H
