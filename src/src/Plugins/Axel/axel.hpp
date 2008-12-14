
/******************************************************************************
* MODULE     : axel.hpp
* DESCRIPTION: interface with Axel
* COPYRIGHT  : (C) 2008  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef AXEL_H
#define AXEL_H

bool axel_present ();
void axel_test ();

#ifdef USE_AXEL
#include <Axel.h>
#endif // USE_AXEL

#endif // AXEL_H
