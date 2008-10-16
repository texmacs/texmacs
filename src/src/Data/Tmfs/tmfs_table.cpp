
/******************************************************************************
* MODULE     : tmfs_basis.cpp
* DESCRIPTION: low-level disk functions for the TeXmacs file system
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tmfs.hpp"
#include "file.hpp"

/******************************************************************************
* The TeXmacs file system and the two major operations
******************************************************************************/

disk_table tmfs;

void
tmfs_initialize () {
  if (is_nil (tmfs)) tmfs= disk_table ("$TEXMACS_HOME_PATH/tmfs");
}

void
tmfs_write (transaction t) {
  tmfs_initialize ();
  tmfs->write (t);
}

transaction
tmfs_read (collection c) {
  tmfs_initialize ();
  return tmfs->read (c);
}

/******************************************************************************
* Set, reset and get
******************************************************************************/

void
tmfs_set (string key, collection val) {
  tmfs_write (atom (key, val));
}

void
tmfs_set (string key, string val) {
  tmfs_write (atom (key, singleton (val, 1)));
}

void
tmfs_reset (string key, collection val) {
  tmfs_write (atom (key, invert (val)));
}

void
tmfs_reset (string key, string val) {
  tmfs_write (atom (key, singleton (val, -1)));
}

collection
tmfs_get (string key) {
  transaction t= tmfs_read (singleton (key));
  return simplify (t[key]);
}

/******************************************************************************
* Saving, removing and loading plain files
******************************************************************************/

void
tmfs_save (string key, string val) {
  tmfs_write (atom (key, singleton (val, 3)));
}

void
tmfs_remove (string key) {
  tmfs_write (atom (key, singleton ("*", -3)));
}

string
tmfs_load (string key) {
  transaction t= tmfs_read (singleton (key, 3));
  collection  c= simplify (t[key]);
  if (N(c) != 1) return "";
  return first (c);
}
