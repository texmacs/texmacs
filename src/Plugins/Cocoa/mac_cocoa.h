
/******************************************************************************
* MODULE     : mac_cocoa.hpp
* DESCRIPTION: Header for Mac UI
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MAC_COCOA_H
#define MAC_COCOA_H

// hacks to prevent symbol clash with Carbon and Cocoa headers
#define ID COCOA_ID
#define outline COCOA_outline
#undef EVENT_H 
#define extend CARBON_extends 
#define object COCOA_object
#define OS_object OS_COCOA_object

#include <Cocoa/Cocoa.h>

// end of hacks
#define EVENT_H
#undef ID
#undef outline
#undef extend
#undef object
#undef OS_object

#endif
