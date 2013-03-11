
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

array<int> build_range (int start, int end);
string height_trace (font_metric fnm, array<int> cs);
array<int> decode_trace (string s);

/******************************************************************************
* Check which character ranges are supported in a font
******************************************************************************/

bool
range_exists (font_metric fnm, int start, int end) {
  for (int i= start; i <= end; i++)
    if (!fnm->exists (i)) return false;
  return true;
}

double
range_percentage (font_metric fnm, int start, int end) {
  int count= 0;
  for (int i= start; i <= end; i++)
    if (fnm->exists (i)) count++;
  return ((double) count) / ((double) max (end+1-start, 1));
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

  if (range_percentage (fnm, 0x4e00, 0x4eff) > 0.0) {
    double perc= range_percentage (fnm, 0x4e00, 0x9fcc);
    //cout << "percentage -> " << perc << "\n";
    if (perc > 0.2) r << string ("+CJK");
  }
  if (range_percentage (fnm, 0xac00, 0xacff) > 0.0) {
    double perc= range_percentage (fnm, 0xac00, 0xd7af);
    //cout << "percentage -> " << perc << "\n";
    if (perc > 0.2) r << string ("+Hangul");
  }

  if (range_percentage (fnm, 0x2100, 0x21ff) > 0.0) {
    double perc= range_percentage (fnm, 0x2000, 0x23ff);
    //cout << "percentage -> " << perc << "\n";
    if (perc > 0.2) r << string ("+Math");
  }
  if (range_percentage (fnm, 0x2900, 0x29ff) > 0.0) {
    double perc= range_percentage (fnm, 0x2900, 0x2e7f);
    //cout << "percentage -> " << perc << "\n";
    if (perc > 0.2) r << string ("+MathExtra");
  }
  if (range_percentage (fnm, 0x1d400, 0x1d4ff) > 0.0) {
    double perc= range_percentage (fnm, 0x1d400, 0x1d7ff);
    //cout << "percentage -> " << perc << "\n";
    if (perc > 0.2) r << string ("+MathLetters");
  }
}

/******************************************************************************
* Check for special font properties (mono, sans serif)
******************************************************************************/

int
int_abs (int i) {
  return max (i, -i);
}

int
l1_distance (array<int> a1, array<int> a2) {
  int r= 0;
  for (int i=0; i<min (N(a1), N(a2)); i++)
    r += int_abs (a1[i] - a2[i]);
  return r;
}

int
irregularity (font_metric fnm) {
  metric_struct* x= fnm->get (0x78);
  int ex= x->y2 / 256;
  array<int> xlike;
  xlike << 0x61 << 0x63 << 0x65 << 0x6d << 0x6e << 0x6f
        << 0x75 << 0x76 << 0x77;
  int totdy= 0;
  for (int i= 0; i<N(xlike); i++) {
    metric_struct* y= fnm->get (xlike[i]);
    totdy += (int_abs (y->y3 - x->y3) + int_abs (y->y4 - x->y4)) / 256;
  }
  return (100 * totdy) / (N(xlike) * ex);
}

void
analyze_special (font fn, font_metric fnm, array<string>& r) {
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

  if (range_exists (fnm, 0x41, 0x5a)) {
    glyph glL= fn->get_glyph ("L");
    if (!is_nil (glL)) {
      bool sans= is_sans_serif (glL);
      if (sans) r << string ("sans=yes");
      else r << string ("sans=no");
    }
  }

  if (range_exists (fnm, 0x5b, 0x5b)) {
    glyph gl= fn->get_glyph ("[");
    if (!is_nil (gl)) {
      int sl= (int) floor (100.0 * get_slant (gl) + 0.5);
      r << (string ("slant=") * as_string (sl));
    }
  }

  if (range_exists (fnm, 0x61, 0x7a)) {
    bool italic= true;
    glyph ga= fn->get_glyph ("a");
    if (is_nil (ga)) italic= false;
    else {
      int status= italic_a_status (ga);
      //r << (string ("italic-a=") * as_string (status));
      italic= (status == 0);
    }
    metric_struct* f= fnm->get (0x66);
    metric_struct* x= fnm->get (0x78);
    italic= italic && (f->y3 < -(x->y2/5));
    if (italic) r << string ("italic=yes");
    else r << string ("italic=no");    
  }

  if (range_exists (fnm, 0x41, 0x5a) && range_exists (fnm, 0x61, 0x7a)) {
    array<int> upr= build_range (0x41, 0x5a);
    array<int> lor= build_range (0x61, 0x7a);
    array<int> uph= decode_trace (height_trace (fnm, upr));
    array<int> loh= decode_trace (height_trace (fnm, lor));
    if (l1_distance (loh, uph) <= N(upr)) {
      metric_struct* A= fnm->get (0x41);
      metric_struct* a= fnm->get (0x61);
      int Ah= A->y4 - A->y3;
      int ah= a->y4 - a->y3;
      if (ah < ((95 * Ah) / 100)) r << string ("case=smallcaps");
      else r << string ("case=caps");
    }
    else r << string ("case=mixed");
  }

  if (range_exists (fnm, 0x61, 0x7a)) {
    bool regular= (irregularity (fnm) <= 6);
    if (regular) r << string ("regular=yes");
    else r << string ("regular=no");
  }
}

/******************************************************************************
* Check for major glyph characteristics
******************************************************************************/

void
analyze_major (font fn, font_metric fnm, array<string>& r) {
  if (range_exists (fnm, 0x41, 0x5a) && range_exists (fnm, 0x61, 0x7a)) {
    metric_struct* x= fnm->get (0x78);
    int ex= x->y2 / 256;
    r << (string ("ex=") * as_string (ex));
    metric_struct* M= fnm->get (0x4d);
    int em_rat= (100 * (M->x2 / 256)) / ex;
    r << (string ("em=") * as_string (em_rat));
    
    glyph glo= fn->get_glyph ("o");
    if (!is_nil (glo)) {
      int lvw= (100 * vertical_stroke_width (glo)) / ex;
      int lhw= (100 * horizontal_stroke_width (glo)) / ex;
      r << (string ("lvw=") * as_string (lvw));
      r << (string ("lhw=") * as_string (lhw));
    }
    
    glyph glO= fn->get_glyph ("O");
    if (!is_nil (glO)) {
      int uvw= (100 * vertical_stroke_width (glO)) / ex;
      int uhw= (100 * horizontal_stroke_width (glO)) / ex;
      r << (string ("uvw=") * as_string (uvw));
      r << (string ("uhw=") * as_string (uhw));
    }

    int cnt= 0;
    int totlw= 0;
    int totw= 0;
    double fill= 0.0;
    for (int i= 0x42; i<=0x7a; i++)
      if (i <= 0x5a || i >= 0x61) {
        string s; s << ((char) i);
        glyph g= fn->get_glyph (s);
        if (!is_nil (g)) {
          cnt += pixel_count (g);
          totlw += g->lwidth;
          totw += g->width;
          fill += fill_rate (g);
        }
      }
    int fillp= (int) (100.0 * (fill / 52.0));
    int vcnt= cnt / max (totlw, 1);
    int asprat= (100 * totw) / (52 * ex);
    r << (string ("fillp=") * as_string (fillp));
    r << (string ("vcnt=") * as_string (vcnt));
    r << (string ("asprat=") * as_string (asprat));

    //int irreg= irregularity (fnm);
    //r << (string ("irreg=") * as_string (irreg));
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

array<int>
decode_trace (string s) {
  array<int> r;
  for (int i=0; i<N(s); i++)
    r << (((int) s[i]) - ((int) '0'));
  return r;
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

string
count_trace (font fn, array<int> cs) {
  array<int> a;
  for (int i= 0; i < N(cs); i++) {
    string s; s << ((char) cs[i]);
    glyph g= fn->get_glyph (s);
    if (is_nil (g)) return "";
    a << pixel_count (g);
  }
  return array_trace (a);
}

void
analyze_trace (font fn, font_metric fnm, array<string>& r) {
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
    array<int> ca;
    ca << 0x41 << 0x42 << 0x43 << 0x44 << 0x45 << 0x48
       << 0x49 << 0x4a << 0x4d << 0x4e << 0x53 << 0x57;
    string ct= count_trace (fn, ca);
    if (ct != "") r << (string ("upc=") * ct);
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
    array<int> ca;
    ca << 0x61 << 0x62 << 0x63 << 0x64 << 0x65 << 0x66
       << 0x68 << 0x69 << 0x6d << 0x72 << 0x79 << 0x7a;
    string ct= count_trace (fn, ca);
    if (ct != "") r << (string ("loc=") * ct);
  }
}

/******************************************************************************
* Master routine
******************************************************************************/

extern bool get_glyph_fatal;

array<string>
tt_analyze (string family) {
  array<string> r;
  font fn= tt_font (family, 10, 1200);
  font_metric fnm= tt_font_metric (family, 10, 1200);
  //cout << "Analyzing " << family << "\n";

  get_glyph_fatal= false;
  analyze_range (fnm, r);
  analyze_special (fn, fnm, r);
  analyze_major (fn, fnm, r);
  analyze_trace (fn, fnm, r);
  get_glyph_fatal= true;

  //cout << "  -> " << r << "\n";
  cout << r << " " << family << "\n";
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
