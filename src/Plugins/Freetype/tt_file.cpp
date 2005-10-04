
/******************************************************************************
* MODULE     : tt_file.cpp
* DESCRIPTION: Finding a True Type font
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tt_file.hpp"
#include "file.hpp"
#include "boot.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "Metafont/tex_files.hpp"
#include "timer.hpp"
#include "data_cache.hpp"

static hashmap<string,string> tt_fonts ("no");

static url
tt_locate (string name) {
  if (ends (name, ".pfb")) {
    url u= resolve_tex (name);
    //cout << "tt_locate: " << name << " -> " << u << "\n";
    if (!is_none (u)) return u;
  }
  else if (use_locate) {
    string s= eval_system ("locate", "/" * name);
    int start, i, n= N(s);
    for (start=0, i=0; i<n; i++)
      if (s[i]=='\n') {
	if (ends (s (start, i), name))
	  return url (s (start, i));
	start= i+1;
      }
  }
  return url_none ();
}

url
tt_font_find (string name) {
  url u= tt_locate (name * ".pfb");
  //if (!is_none (u)) cout << name << " -> " << u << "\n";
  if (!is_none (u)) return u;
  u= tt_locate (name * ".ttf");
  //if (!is_none (u)) cout << name << " -> " << u << "\n";
  //else cout << name << " -> ???\n";
  return u;
}

bool
tt_font_exists (string name) {
  // cout << "tt_font_exists? " << name << "\n";
  if (tt_fonts->contains (name)) return tt_fonts[name] == "yes";
  bool yes= !is_none (tt_font_find (name));
  tt_fonts (name)= yes? string ("yes"): string ("no");
  return yes;
}

string
tt_find_name_sub (string name, int size) {
  if (size == 0) {
    if (tt_font_exists (name)) return name;
    else return "";
  }
  if (tt_font_exists (name * as_string (size)))
    return name * as_string (size);
  if (size > 333) size= (size+50)/100;
  if (tt_font_exists (name * as_string (size)))
    return name * as_string (size);

  if ((size >= 15) && tt_font_exists (name * "17")) return name * "17";
  if ((size >  12) && tt_font_exists (name * "12")) return name * "12";
  if ((size <  5 ) && tt_font_exists (name * "5" )) return name * "5" ;
  if ((size <  6 ) && tt_font_exists (name * "6" )) return name * "6" ;
  if ((size <  7 ) && tt_font_exists (name * "7" )) return name * "7" ;
  if ((size <  8 ) && tt_font_exists (name * "8" )) return name * "8" ;
  if ((size <  9 ) && tt_font_exists (name * "9" )) return name * "9" ;
  if ((size <  9 ) && tt_font_exists (name * "7" )) return name * "7" ;
  if (tt_font_exists (name * "10")) return name * "10";
  if ((size <  9 ) && tt_font_exists (name * "700" )) return name * "700" ;
  if ((size >= 15) && tt_font_exists (name * "1700")) return name * "1700";
  if (tt_font_exists (name * "1000")) return name * "1000";
  if (tt_font_exists (name)) return name;
  return "";
}

string
tt_find_name (string name, int size) {
  cache_load ();
  string s= "tt:" * name * as_string (size);
  if (is_cached (s)) {
    string r= cache_get (s) -> label;
    if (tt_font_exists (r)) return r;
    cache_reset (s);
  }

  bench_start ("tt find name");
  string r= tt_find_name_sub (name, size);
  // cout << name << size << " -> " << r << "\n";
  bench_cumul ("tt find name");

  if (r != "") cache_set (s, r);
  return r;
}
