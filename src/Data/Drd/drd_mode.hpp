
/******************************************************************************
* MODULE     : drd_mode.hpp
* DESCRIPTION: global variables that affect drd-based routines
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef DRD_MODE_H
#define DRD_MODE_H

#define DRD_ACCESS_NORMAL    0
#define DRD_ACCESS_HIDDEN    1
#define DRD_ACCESS_SOURCE    2

#define DRD_WRITABLE_NORMAL  0
#define DRD_WRITABLE_INPUT   1

int set_access_mode (int mode);
int get_access_mode ();
int set_writable_mode (int mode);
int get_writable_mode ();

#endif // defined DRD_MODE_H
