
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

void cache_set (string key, tree im);
void cache_reset (string key);
bool is_cached (string key);
tree cache_get (string key);

void cache_save ();
void cache_load ();

#endif // defined DATA_CACHE_H
