
/******************************************************************************
* MODULE     : smart_font.cpp
* DESCRIPTION: smart merging of several fonts for different unicode ranges
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "Freetype/tt_tools.hpp"

/******************************************************************************
* Efficient computation of the appropriate subfont
******************************************************************************/

RESOURCE(smart_map);

#define SUBFONT_MAIN  0
#define SUBFONT_ERROR 1

struct smart_map_rep: rep<smart_map> {
  int chv[256];
  hashmap<string,int> cht;
  hashmap<tree,int> fn_nr;
  array<tree> fn_spec;

public:
  smart_map_rep (string name, tree fn):
    rep<smart_map> (name), cht (-1), fn_nr (-1), fn_spec (2)
  {
    for (int i=0; i<256; i++) chv[i]= -1;
    fn_nr (tree ("main" ))= SUBFONT_MAIN;
    fn_nr (tree ("error"))= SUBFONT_ERROR;
    fn_spec[SUBFONT_MAIN ]= fn;
    fn_spec[SUBFONT_ERROR]= tree ("error");
  }

  int
  add_char (tree fn, string c) {
    //cout << "Add " << c << " to " << fn << "\n";
    if (!fn_nr->contains (fn)) {
      int sz= N (fn_spec);
      fn_nr (fn)= sz;
      fn_spec << fn;
      //cout << "Create " << sz << " -> " << fn << "\n";
    }
    int nr= fn_nr [fn];
    if (starts (c, "<")) cht (c)= nr;
    else chv [(int) (unsigned char) c[0]]= nr;
    return nr;
  }
};


RESOURCE_CODE(smart_map);

smart_map
get_smart_map (tree fn) {
  string name= recompose (tuple_as_array (fn), "-");
  if (smart_map::instances -> contains (name))
    return smart_map (name);
  return make (smart_map, name, tm_new<smart_map_rep> (name, fn));
}

/******************************************************************************
* The smart font class
******************************************************************************/

typedef int int_vector[256];
typedef hashmap<string,int> int_table;

struct smart_font_rep: font_rep {
  string family;
  string variant;
  string series;
  string shape;
  int    sz;
  int    dpi;

  array<font> fn;
  smart_map   sm;

  smart_font_rep (string name, font base_fn, font err_fn,
                  string family, string variant,
                  string series, string shape, int sz, int dpi);

  void   advance (string s, int& pos, string& r, int& nr);
  int    search_subfont (string c);
  int    resolve (string c);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  font   magnify (double zoom);
  glyph  get_glyph (string s);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction (string s);
};

smart_font_rep::smart_font_rep (
  string name, font base_fn, font err_fn, string family2, string variant2,
  string series2, string shape2, int sz2, int dpi2):
    font_rep (name, base_fn), family (family2), variant (variant2),
    series (series2), shape (shape2), sz (sz2), dpi (dpi2),
    fn (2), sm (get_smart_map (tuple (family2, variant2, series2, shape2)))
{
  fn[SUBFONT_MAIN ]= base_fn;
  fn[SUBFONT_ERROR]= err_fn;
}

/******************************************************************************
* Smart font resolution
******************************************************************************/

static int
get_ex (string family, string variant, string series, string shape,
	int attempt) {
  array<string> lfn= logical_font (family, variant, series, shape);
  array<string> pfn= search_font (lfn, attempt);
  //array<string> nfn= logical_font (pfn[0], pfn[1]);
  array<string> chs= font_database_characteristics (pfn[0], pfn[1]);
  string ex= find_attribute_value (chs, "ex");
  if (ex == "") return 0;
  else return as_int (ex);
}

void
smart_font_rep::advance (string s, int& pos, string& r, int& nr) {
  int* chv= sm->chv;
  hashmap<string,int>& cht (sm->cht);
  int start= pos;
  nr= -1;
  while (pos < N(s)) {
    if (s[pos] != '<') {
      int c= (int) (unsigned char) s[pos];
      int next= chv[c];
      if (chv[c] == -1) next= resolve (s (pos, pos+1));
      if (next == nr) pos++;
      else if (nr == -1) { pos++; nr= next; }
      else break;
    }
    else {
      int end= pos;
      tm_char_forwards (s, end);
      int next= cht[s (pos, end)];
      if (next == -1) next= resolve (s (pos, end));
      if (next == nr) pos= end;
      else if (nr == -1) { pos= end; nr= next; }
      else break;
    }
  }
  r= s (start, pos);
  if (nr < 0) return;
  if (N(fn) <= nr) fn->resize (nr+1);
  if (is_nil (fn[nr])) {
    array<string> a= tuple_as_array (sm->fn_spec[nr]);
    int att= as_int (a[4]);
    int ex1= get_ex (family, variant, series, shape, 1);
    int ex2= get_ex (a[0], a[1], a[2], a[3], att);
    double zoom= 1.0;
    if (ex1 != 0 && ex2 != 0) zoom= ((double) ex1) / ((double) ex2);
    if (zoom > 0.975 && zoom < 1.025) zoom= 1;
    int ndpi= (int) tm_round (dpi * zoom);
    fn[nr]= closest_font (a[0], a[1], a[2], a[3], sz, ndpi, att);
    //cout << "Font " << nr << " -> " << fn[nr]->res_name << "\n";
  }
}

int
smart_font_rep::search_subfont (string c) {
  if (fn[SUBFONT_MAIN]->supports (c))
    return sm->add_char (tree ("main"), c);
  string uc= cork_to_utf8 (c);
  int pos= 0;
  int code= decode_from_utf8 (uc, pos);

  string range= "";
  if (code <= 0x7f) range= "+ascii";
  else if (code >= 0x80 && code <= 0xff) range= "+latin1basic";
  else if (code >= 0x100 && code <= 0x17f) range= "+latina";
  else if (code >= 0x380 && code <= 0x3ff) range= "+greekbasic";
  else if (code >= 0x400 && code <= 0x4ff) range= "+cyrillicbasic";
  else if (code >= 0x4e00 && code <= 0x9fcc) range= "+cjk";
  else if (code >= 0xac00 && code <= 0xd7af) range= "+hangul";
  else if (code >= 0x2000 && code <= 0x23ff) range= "+math";
  else if (code >= 0x2900 && code <= 0x2e7f) range= "+mathextra";
  else if (code >= 0x1d400 && code <= 0x1d7ff) range= "+mathletters";

  if (pos == N(uc)) {
    string v= variant;
    int start= 1;
    if (range == "") start= 2;
    else if (v == "rm") v= range;
    else v= v * "-" * range;
    for (int attempt= start; attempt <= 20; attempt++) {
      font cfn= closest_font (family, v, series, shape, sz, dpi, attempt);
      //cout << "Trying " << c << " in " << cfn->res_name << "\n";
      if (cfn->supports (c)) {
	tree key= tuple (family, v, series, shape, as_string (attempt));
	return sm->add_char (key, c);
      }
    }
  }

  return sm->add_char (tree ("error"), c);
}

int
smart_font_rep::resolve (string c) {
  return search_subfont (c);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

static string empty_string ("");

bool
smart_font_rep::supports (string c) {
  (void) c;
  return true;
}

void
smart_font_rep::get_extents (string s, metric& ex) {
  int i=0, n= N(s);
  fn[0]->get_extents (empty_string, ex);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->get_extents (r, ey);
      ex->y1= min (ex->y1, ey->y1);
      ex->y2= max (ex->y2, ey->y2);
      ex->x3= min (ex->x3, ex->x2 + ey->x3);
      ex->y3= min (ex->y3, ey->y3);
      ex->x4= max (ex->x4, ex->x2 + ey->x4);
      ex->y4= max (ex->y4, ey->y4);
      ex->x2 += ey->x2;
    }
  }
}

void
smart_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  int i=0, n= N(s);
  while (i < n) {
    int nr;
    string r= s;
    metric ey;
    advance (s, i, r, nr);
    if (nr >= 0) {
      fn[nr]->draw_fixed (ren, r, x, y);
      if (i < n) {
	fn[nr]->get_extents (r, ey);
	x += ey->x2;
      }
    }
  }
}

font
smart_font_rep::magnify (double zoom) {
  return smart_font (family, variant, series, shape, sz,
                     (int) tm_round (dpi * zoom));
}

/******************************************************************************
* Other routines for fonts
******************************************************************************/

glyph
smart_font_rep::get_glyph (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_glyph (s);
  string r= s;
  advance (s, i, r, nr);
  if (nr<0) return glyph ();
  return fn[nr]->get_glyph (r);
}

double
smart_font_rep::get_left_slope  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_slope (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_slope (r);
}

double
smart_font_rep::get_right_slope (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_right_slope (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_slope (r);
}

SI
smart_font_rep::get_left_correction  (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_left_correction (s);
  string r= s;
  advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_left_correction (r);
}

SI
smart_font_rep::get_right_correction (string s) {
  int i=0, n= N(s), nr;
  if (n == 0) return fn[0]->get_right_correction (s);
  string r= s;
  while (i<n) advance (s, i, r, nr);
  nr= max (nr, 0);
  return fn[nr]->get_right_correction (r);
}

/******************************************************************************
* User interface
******************************************************************************/

font
smart_font (string family, string variant, string series, string shape,
            int sz, int dpi) {
  if (starts (family, "tc"))
    // FIXME: temporary hack for symbols from std-symbol.ts
    return find_font (family, variant, series, shape, sz, dpi);

  string name=
    family * "-" * variant * "-" *
    series * "-" * shape * "-" *
    as_string (sz) * "-" * as_string (dpi) * "-smart";
  if (font::instances->contains (name)) return font (name);
  font base_fn= closest_font (family, variant, series, shape, sz, dpi);
  if (is_nil (base_fn)) return font ();
  font err_fn= error_font (base_fn);
  return make (font, name,
               tm_new<smart_font_rep> (name, base_fn, err_fn, family, variant,
                                       series, shape, sz, dpi));
}
