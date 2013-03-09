
/******************************************************************************
* MODULE     : tt_analyze.cpp
* DESCRIPTION: analysis of global properties of true type fonts
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tt_face.hpp"

#ifdef USE_FREETYPE

bool
range_exists (font_metric fm, int start, int end) {
  for (int i= start; i <= end; i++)
    if (!fm->exists (i)) return false;
  return true;
}

array<string>
tt_analyze (string family) {
  array<string> r;
  font_metric fm= tt_font_metric (family, 10, 1200);
  cout << "Analyzing " << family << "\n";

  bool ok1= range_exists (fm, 0x41, 0x5a);
  if (ok1) cout << "Support ASCII uppercase\n";

  bool ok2= range_exists (fm, 0x61, 0x7a);
  if (ok2) cout << "Support ASCII lowercase\n";
 
  bool ok3= range_exists (fm, 0x30, 0x39);
  if (ok3) cout << "Support ASCII digits\n";

  bool ok4= range_exists (fm, 0x20, 0x7e);
  if (ok4) cout << "Support ASCII\n";

  bool ok5= range_exists (fm, 0xc0, 0xff);
  if (ok5) cout << "Support Latin-1 accented\n";

  bool ok6= range_exists (fm, 0xa1, 0xac) && range_exists (fm, 0xae, 0xbf);
  if (ok6) cout << "Support Latin-1 symbols\n";

  bool ok7= range_exists (fm, 0x100, 0x17f);
  if (ok7) cout << "Support Latin-A symbols\n";

  bool ok8= range_exists (fm, 0x180, 0x1ff);
  if (ok8) cout << "Support Latin-B symbols\n";

  return r;
}

#else

array<string>
tt_analyze (string family) {
  (void) family;
  array<string> r;
  return r;
}

#endif // USE_FREETYPE
