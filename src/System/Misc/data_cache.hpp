
/******************************************************************************
* MODULE     : data_cache.hpp
* DESCRIPTION: utilities for caching data
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef DATA_CACHE_H
#define DATA_CACHE_H
#include "url.hpp"

void cache_set (string buffer, tree key, tree im);
void cache_reset (string buffer, tree key);
bool is_cached (string buffer, tree key);
tree cache_get (string buffer, tree key);
bool is_up_to_date (url dir, bool reset= false);
bool is_recursively_up_to_date (url dir);

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
