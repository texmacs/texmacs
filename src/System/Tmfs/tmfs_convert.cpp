
/******************************************************************************
* MODULE     : tmfs_convert.cpp
* DESCRIPTION: conversions from/into conventional file systems
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"

/*
void
tmfs_import (string prj, url base, url u) {
  if (is_or (u)) {
    tmfs_import (prj, base, u[1]);
    tmfs_import (prj, base, u[2]);
  }
  else if (is_directory (u)) {
    bool flag;
    array<string> a= read_directory (u, flag);
    if (flag) return;
    for (int i=0; i<N(a); i++)
      if (a[i] != "CVS" && !ends (a[i], "~"))
	tmfs_import (prj, base, u * a[i]);
  }
  else if (is_regular (u)) {
    string name= as_string (u);
    string val1= tmfs_load (id);
    collection ids= tmfs_query ("mirror", "?id", name);
    if (N (ids) != 1) {
      string id= tmfs_new_identifier ();
      tmfs_save (id, val1);
      tmfs_set_attribute ("mirror", id, name);
      tmfs_set_attribute ("project", id, prj);
      tmfs_set_attribute ("placement", id, prj, as_string (delta (base, u)));
    }
    else {
      string id  = first (ids);
      string val2= load_string (u);
      if (val1 == val2) return id;
      tmfs_update (id, val1);
    }
  }
}

void
tmfs_export (string prj, url base, url u) {
  collection s= tmfs_get_attribute ("project", "?id", prj);
  iterator it= iterate (s);
  if (it->busy ()) {
    string id= it->next ();
    collection pl= tmfs_get_attribute ("placement", id, prj, "?delta");
    if (N(pl) == 1) {
      url u= base * first (pl);
      if (descends (u, base)) {
	string val= tmfs_load (id);
	save_string (dest_dir * delta (src_dir, u), id);
      }
    }
}
*/
