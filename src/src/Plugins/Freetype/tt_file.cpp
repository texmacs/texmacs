
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

#include "Freetype/tt_file.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"

static hashmap<string,string> tt_fonts ("no");

url
tt_font_find (string name) {
  string suffix= "/" * name * ".ttf";
  string s= eval_system ("locate", suffix);
  int start, i, n= N(s);
  for (start=0, i=0; i<n; i++)
    if (s[i]=='\n') {
      if (ends (s (start, i), suffix))
	return url (s (start, i));
      start= i+1;
    }
  return url_none ();
}

bool
tt_font_exists (string name) {
  if (tt_fonts->contains (name)) return tt_fonts[name] == "yes";
  bool yes= !is_none (tt_font_find (name));
  tt_fonts (name)= yes? string ("yes"): string ("no");
  return yes;
}
