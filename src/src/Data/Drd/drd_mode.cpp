
/******************************************************************************
* MODULE     : drd_mode.cpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "drd_mode.hpp"

static int drd_access_mode  = DRD_ACCESS_NORMAL;
static int drd_writable_mode= DRD_WRITABLE_NORMAL;

int
set_access_mode (int mode) {
  int old_mode= drd_access_mode;
  drd_access_mode= mode;
  return old_mode;
}

int
get_access_mode () {
  return drd_access_mode;
}

int
set_writable_mode (int mode) {
  int old_mode= drd_writable_mode;
  drd_writable_mode= mode;
  return old_mode;
}

int
get_writable_mode () {
  return drd_writable_mode;
}
