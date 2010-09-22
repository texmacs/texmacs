
/******************************************************************************
* MODULE     : tt_file.cpp
* DESCRIPTION: Finding a True Type font
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
search_sub_dirs (url root) {
  url dirs= complete (root * url_wildcard (), "dr");
  return expand (dirs);
}

static url
tt_locate (string name) {
  if (ends (name, ".pfb")) {
    /*
    if (starts (name, "rpag")) name= "uag" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rpbk")) name= "ubk" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rpcr")) name= "ucr" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rphv")) name= "uhv" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rpnc")) name= "unc" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rppl")) name= "upl" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rpsy")) name= "usy" * name (4, N (name));
    if (starts (name, "rptm")) name= "utm" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rpzc")) name= "uzc" * name (4, N (name) - 4) * "8a.pfb";
    if (starts (name, "rpzd")) name= "uzd" * name (4, N (name));
    */
    url u= resolve_tex (name);
    //cout << "tt_locate: " << name << " -> " << u << "\n";
    if (!is_none (u)) return u;
  }
  else if (use_locate &&
	   // NOTE: avoiding unnecessary locates can greatly improve timings
	   !starts (name, "ec") &&
	   !starts (name, "la") &&
	   !starts (name, "cm") &&
	   !starts (name, "msam") &&
	   !starts (name, "msbm") &&
	   !starts (name, "bbm") &&
	   !starts (name, "stmary") &&
	   !starts (name, "rsfs") &&
	   !starts (name, "grmn") &&
	   !starts (name, "mac-")
	   // FIXME: better caching of missed tt_locates would be better
	   )
    {
      string s= eval_system ("locate", "/" * name);
      //cout << "locate " << name << " -> " << s << "\n";
      int start, i, n= N(s);
      for (start=0, i=0; i<n; i++)
	if (s[i]=='\n') {
	  if (ends (s (start, i), name))
	    return url (s (start, i));
	  start= i+1;
	}
    }

  url tt_path=
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/truetype") |
#if defined __MINGW32__
    search_sub_dirs ("$windir/Fonts");
#elif defined OS_MACOS
    search_sub_dirs ("$HOME/Library/Fonts") |
    search_sub_dirs ("/Library/Fonts") |
    search_sub_dirs ("/System/Library/Fonts");
#else
    search_sub_dirs ("/usr/share/fonts/truetype") |
    search_sub_dirs ("/usr/local/share/fonts/truetype");
#endif
  return resolve (tt_path * name);
}

url
tt_font_find (string name) {
  //cout << "tt_font_find " << name << "\n";
  url u= tt_locate (name * ".pfb");
  //if (!is_none (u)) cout << name << " -> " << u << "\n";
  if (!is_none (u)) return u;
  u= tt_locate (name * ".ttf");
  //if (!is_none (u)) cout << name << " -> " << u << "\n";
  //else cout << name << " -> ???\n";
  if (!is_none (u)) return u;
  u= tt_locate (name * ".ttc");
  return u;
}

bool
tt_font_exists (string name) {
  //cout << "tt_font_exists? " << name << "\n";
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
  string s= "tt:" * name * as_string (size);
  if (is_cached ("font_cache.scm", s)) {
    string r= cache_get ("font_cache.scm", s) -> label;
    if (tt_font_exists (r)) return r;
    cache_reset ("font_cache.scm", s);
  }

  bench_start ("tt find name");
  string r= tt_find_name_sub (name, size);
  //cout << name << size << " -> " << r << "\n";
  bench_cumul ("tt find name");

  if (r != "") cache_set ("font_cache.scm", s, r);
  return r;
}
