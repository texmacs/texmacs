
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
#include "font.hpp"

#ifdef USE_FREETYPE

/******************************************************************************
* Check which character ranges are supported in a font
******************************************************************************/

bool
range_exists (font_metric fnm, int start, int end) {
  for (int i= start; i <= end; i++)
    if (!fnm->exists (i)) return false;
  return true;
}

void
analyze_range (font_metric fnm, array<string>& r) {
  if (range_exists (fnm, 0x41, 0x5a))
    r << string ("+Upper");
  if (range_exists (fnm, 0x61, 0x7a))
    r << string ("+Lower");
  if (range_exists (fnm, 0x30, 0x39))
    r << string ("+Digits");
  if (range_exists (fnm, 0x20, 0x7e))
    r << string ("+Ascii");
  if (range_exists (fnm, 0xc0, 0xff))
    r << string ("+Latin1Basic");
  if (range_exists (fnm, 0xa1, 0xac) && range_exists (fnm, 0xae, 0xbf))
    r << string ("+Latin1Symbols");
  if (range_exists (fnm, 0x100, 0x17f))
    r << string ("+LatinA");
  if (range_exists (fnm, 0x180, 0x1ff))
    r << string ("+LatinB");
  if (range_exists (fnm, 0x391, 0x3a1) && range_exists (fnm, 0x3a3, 0x3ce))
    r << string ("+GreekBasic");
  if (range_exists (fnm, 0x410, 0x44f))
    r << string ("+CyrillicBasic");
}

/******************************************************************************
* Check for special font properties (mono, sans serif)
******************************************************************************/

void
analyze_special (font_metric fnm, array<string>& r) {
  if (range_exists (fnm, 0x41, 0x5a) && range_exists (fnm, 0x61, 0x7a)) {
    bool mono= true;
    metric_struct* x= fnm->get (0x41);
    for (int i= 0x42; i<=0x7a; i++)
      if (i <= 0x5a || i >= 0x61) {
        metric_struct* y= fnm->get (i);
        if (y->x2 != x->x2) {
          mono= false;
          break;
        }
      }
    if (mono) r << string ("mono=yes");
    else r << string ("mono=no");
  }
}

/******************************************************************************
* Check for major glyph characteristics
******************************************************************************/

void
analyze_major (font_metric fnm, array<string>& r) {
  if (range_exists (fnm, 0x41, 0x5a) && range_exists (fnm, 0x61, 0x7a)) {
    metric_struct* x= fnm->get (0x78);
    int ex= x->y2 / 256;
    r << (string ("ex=") * as_string (ex));
    metric_struct* M= fnm->get (0x4d);
    int em_rat= (100 * (M->x2 / 256)) / ex;
    r << (string ("em=") * as_string (em_rat));
  }
}

/******************************************************************************
* Check for major glyph characteristics
******************************************************************************/

array<int>
build_range (int start, int end) {
  array<int> a;
  for (int i=start; i<=end; i++) a << i;
  return a;
}

string
array_trace (array<int> a) {
  for (int i=0; i<N(a); i++)
    if (a[i] < 0) a[i]= -a[i];
  int ma= 0, mi= 1000000000;
  for (int i=0; i<N(a); i++) {
    ma= max (ma, a[i]);
    mi= min (mi, a[i]);
  }
  mi--; ma++;
  string s;
  for (int i=0; i<N(a); i++) {
    int x= (10*(a[i] - mi)) / (ma - mi);
    s << ((char) (((int) '0') + x));
  }
  return s;
}

/******************************************************************************
* Compute vector trace for various font characteristics
******************************************************************************/

string
width_trace (font_metric fnm, array<int> cs) {
  array<int> a;
  for (int i= 0; i < N(cs); i++) {
    metric_struct* m= fnm->get (cs[i]);
    a << (m->x4 - m->x3);
  }
  return array_trace (a);
}

string
height_trace (font_metric fnm, array<int> cs) {
  array<int> a;
  for (int i= 0; i < N(cs); i++) {
    metric_struct* m= fnm->get (cs[i]);
    a << (m->y4 - m->y3);
  }
  return array_trace (a);
}

void
analyze_trace (font_metric fnm, array<string>& r) {
  if (range_exists (fnm, 0x41, 0x5a)) {
    array<int> wa;
    wa << build_range (0x41, 0x46);
    wa << 0x49 << 0x4a << 0x4d << 0x52 << 0x53 << 0x57;
    string wt= width_trace (fnm, wa);
    r << (string ("upw=") * wt);
    array<int> ha;
    ha << 0x41 << 0x42 << 0x43 << 0x44 << 0x47 << 0x48
       << 0x4a << 0x4d << 0x4e << 0x51;
    string ht= height_trace (fnm, ha);
    r << (string ("uph=") * ht);
  }
  if (range_exists (fnm, 0x61, 0x7a)) {
    array<int> wa;
    wa << build_range (0x61, 0x66);
    wa << 0x69 << 0x6a << 0x6c << 0x6d << 0x74 << 0x77;
    string wt= width_trace (fnm, wa);
    r << (string ("low=") * wt);
    array<int> ha;
    ha << 0x61 << 0x62 << 0x63 << 0x64 << 0x66 << 0x67
       << 0x69 << 0x6a << 0x70 << 0x71 << 0x74 << 0x7a;
    string ht= height_trace (fnm, ha);
    r << (string ("loh=") * ht);
  }
}

/******************************************************************************
* Master routine
******************************************************************************/

array<string>
tt_analyze (string family) {
  array<string> r;
  font_metric fnm= tt_font_metric (family, 10, 1200);
  font fn= tt_font (family, 10, 1200);
  cout << "Analyzing " << family << "\n";

  analyze_range (fnm, r);
  analyze_special (fnm, r);
  analyze_major (fnm, r);
  analyze_trace (fnm, r);

  //cout << "  -> " << r << "\n";
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
