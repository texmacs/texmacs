
/******************************************************************************
* MODULE     : boot.hpp
* DESCRIPTION: manipulation of TeX font files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef BOOT_H
#define BOOT_H
#include "url.hpp"

extern tree texmacs_settings;
extern int  install_status;
extern bool use_which;
extern bool use_locate;

string get_setting (string var, string def= "");
void   set_setting (string var, string val);
void   get_old_settings (string s);
void   init_upgrade ();
void   init_texmacs ();
void   init_plugins ();
void   setup_texmacs ();

scheme_tree plugin_list ();

#endif // defined BOOT_H
