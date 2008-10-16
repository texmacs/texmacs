
/******************************************************************************
* MODULE     : mac_cocoa.hpp
* DESCRIPTION: Header for Mac UI
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef MAC_COCOA_H
#define MAC_COCOA_H

// hacks to prevent symbol clash with Carbon and Cocoa headers
#define ID COCOA_ID
#define outline COCOA_outline
#undef EVENT_H 


#include <Cocoa/Cocoa.h>

// end of hacks
#define EVENT_H
#undef ID
#undef outline


#endif