
/******************************************************************************
* MODULE     : data_cache.hpp
* DESCRIPTION: utilities for caching data
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef DATA_CACHE_H
#define DATA_CACHE_H
#include "tree.hpp"

void cache_set (string buffer, tree key, tree im);
void cache_reset (string buffer, tree key);
bool is_cached (string buffer, tree key);
tree cache_get (string buffer, tree key);

bool do_cache_dir (string name);
bool do_cache_stat_fail (string name);
bool do_cache_stat (string name);
bool do_cache_file (string name);
bool do_cache_doc (string name);

void cache_save (string buffer);
void cache_load (string buffer);
void cache_memorize ();
void cache_refresh ();
void cache_initialize ();

#endif // defined DATA_CACHE_H
