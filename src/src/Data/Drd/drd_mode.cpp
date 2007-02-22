
/******************************************************************************
* MODULE     : drd_mode.cpp
* DESCRIPTION: data relation descriptions
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
