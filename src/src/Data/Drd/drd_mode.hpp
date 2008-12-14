
/******************************************************************************
* MODULE     : drd_mode.hpp
* DESCRIPTION: global variables that affect drd-based routines
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef DRD_MODE_H
#define DRD_MODE_H

#define DRD_ACCESS_NORMAL    0
#define DRD_ACCESS_HIDDEN    1
#define DRD_ACCESS_SOURCE    2

#define DRD_WRITABLE_NORMAL  0
#define DRD_WRITABLE_INPUT   1
#define DRD_WRITABLE_ANY     2

int set_access_mode (int mode);
int get_access_mode ();
int set_writable_mode (int mode);
int get_writable_mode ();

#endif // defined DRD_MODE_H
