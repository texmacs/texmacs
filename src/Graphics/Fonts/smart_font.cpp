
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
#include "translator.hpp"

/******************************************************************************
* Efficient computation of the appropriate subfont
******************************************************************************/

RESOURCE(smart_map);

#define SUBFONT_MAIN  0
#define SUBFONT_ERROR 1
#define SUBFONT_MATH  2

#define REWRITE_NONE       0
#define REWRITE_MATH       1
#define REWRITE_CYRILLIC   2
#define REWRITE_LETTERS    3
#define REWRITE_SPECIAL    4

struct smart_map_rep: rep<smart_map> {
  int chv[256];
  hashmap<string,int> cht;
  hashmap<tree,int> fn_nr;
  array<tree> fn_spec;
  array<int> fn_rewr;

public:
  smart_map_rep (string name, tree fn):
    rep<smart_map> (name), cht (-1), fn_nr (-1), fn_spec (2), fn_rewr (2)
  {
    for (int i=0; i<256; i++) chv[i]= -1;
    fn_nr (tuple ("main" ))= SUBFONT_MAIN;
    fn_nr (tuple ("error"))= SUBFONT_ERROR;
    fn_spec[SUBFONT_MAIN ]= tuple ("main");
    fn_spec[SUBFONT_ERROR]= tuple ("error");
    fn_rewr[SUBFONT_MAIN ]= REWRITE_NONE;
    fn_rewr[SUBFONT_ERROR]= REWRITE_NONE;
  }

  int
  add_font (tree fn, int rewr) {
    if (!fn_nr->contains (fn)) {
      int sz= N (fn_spec);
      fn_nr (fn)= sz;
      fn_spec << fn;
      fn_rewr << rewr;
      //cout << "Create " << sz << " -> " << fn << "\n";
    }
    return fn_nr[fn];
  }

  int
  add_char (tree fn, string c) {
    //cout << "Add " << c << " to " << fn << "\n";
    add_font (fn, REWRITE_NONE);
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
* Virtual font handling
******************************************************************************/

static bool virt_initialized= false;
static array<string> std_virt;
static array<translator> std_trl;

static void
initialize_virtual () {
  if (virt_initialized) return;
  std_virt << string ("long") << string ("negate") << string ("misc");
  for (int i=0; i<N(std_virt); i++)
    std_trl << load_translator (std_virt[i]);
  virt_initialized= true;
}

static string
find_in_virtual (string c) {
  initialize_virtual ();
  for (int i=0; i<N(std_virt); i++)
    if (std_trl[i]->dict->contains (c))
      return std_virt[i];
  return "";
}

/******************************************************************************
* Special characters in mathematical fonts
******************************************************************************/

static string rewrite_math (string s);

static bool
is_math_family (string f) {
  return
    f == "roman" ||
    f == "concrete" ||
    f == "Euler" ||
    f == "ENR";
}

inline bool
is_letter (char c) {
  return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

static bool
is_greek (string c) {
  static hashmap<string,bool> t (false);
  if (N(t) == 0) {
    array<int> a;
    //for (int i= 0x391; i<0x3a9; i++) if (i != 0x3a2) a << i;
    for (int i= 0x3b1; i<0x3c9; i++) a << i;
    for (int i= 0; i<N(a); i++) {
      string s= upcase_all ("<#" * as_hexadecimal (a[i]) * ">");
      t (s)= true;
      t (locase_all (s))= true;
      t (rewrite_math (s))= true;
    }
  }
  return t[c];
}

static hashmap<string,string> special_table ("");

static bool
unicode_provides (string s) {
  return cork_to_utf8 (s) != s;
}

static bool
is_special (string s) {
  if (N (special_table) == 0) {
    special_table ("*")= "";
    special_table ("<noplus>")= "";
    special_table ("<nocomma>")= "";
    special_table ("<nospace>")= "";
    special_table ("<nobracket>")= "";
    special_table ("<nosymbol>")= "";
    special_table ("-")= "<minus>";
    special_table ("|")= "<mid>";
    special_table ("'")= "<#2B9>";
    special_table ("`")= "<backprime>";
  }
  if (starts (s, "<big-."))
    special_table (s)= "";
  if (starts (s, "<big-") && (ends (s, "-1>") || ends (s, "-2>"))) {
    string ss= s (0, N(s)-3) * ">";
    //cout << "Search " << ss << "\n";
    if (unicode_provides (ss))
      special_table (s)= ss;
    ss= "<big" * s (5, N(s)-3) * ">";
    //cout << "Search " << ss << "\n";
    if (unicode_provides (ss))
      special_table (s)= ss;
    ss= "<" * s (5, N(s)-3) * ">";
    if (ends (ss, "lim>")) ss= ss (0, N(ss)-4) * ">";
    //cout << "Search " << ss << "\n";
    if (unicode_provides (ss))
      special_table (s)= ss;
  }
  return special_table->contains (s);
}

/******************************************************************************
* Mathematical letters in Unicode
******************************************************************************/

static hashmap<string,string> substitution_char ("");
static hashmap<string,string> substitution_font ("");

static void
unicode_subst (int src, int dest, int nr, string fn) {
  for (int i=0; i<nr; i++) {
    string csrc = upcase_all ("<#" * as_hexadecimal (src  + i) * ">");
    string cdest= upcase_all ("<#" * as_hexadecimal (dest + i) * ">");
    if (dest + i < 128) cdest= string ((char) (dest + i));
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
    csrc= locase_all (csrc);
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
    csrc= rewrite_math (csrc);
    substitution_char (csrc)= cdest;
    substitution_font (csrc)= fn;
  }
}

static void
unicode_letters (int start, string fn) {
  unicode_subst (start, 0x41, 26, fn);
  unicode_subst (start + 26, 0x61, 26, fn);
}

static void
unicode_greek (int start, string fn) {
  unicode_subst (start, 0x391, 25, fn); // FIXME: attention to 0x3a2
  unicode_subst (start + 25, 0x2207, 1, fn);
  unicode_subst (start + 26, 0x3b1, 25, fn);
  unicode_subst (start + 51, 0x2202, 1, fn);
  unicode_subst (start + 52, 0x3f5, 1, fn);
  unicode_subst (start + 53, 0x3d1, 1, fn);
  unicode_subst (start + 54, 0x3f0, 1, fn);
  unicode_subst (start + 55, 0x3d5, 1, fn);
}

static void
unicode_digits (int start, string fn) {
  unicode_subst (start, 0x30, 10, fn);
}

static void
init_unicode_substitution () {
  if (N (substitution_char) != 0) return;
  unicode_letters (0x1d400, "bold");
  unicode_letters (0x1d434, "italic");
  unicode_letters (0x1d468, "bold-italic");
  unicode_letters (0x1d49c, "cal");
  unicode_letters (0x1d4d0, "bold-cal");
  unicode_letters (0x1d504, "frak");
  unicode_letters (0x1d56c, "bold-frak");
  unicode_letters (0x1d538, "bbb");
  unicode_letters (0x1d5a0, "ss");
  unicode_letters (0x1d5d4, "bold-ss");
  unicode_letters (0x1d608, "italic-ss");
  unicode_letters (0x1d63c, "bold-italic-ss");
  unicode_letters (0x1d670, "tt");
  unicode_greek (0x1d6a8, "bold");
  unicode_greek (0x1d6e2, "italic");
  unicode_greek (0x1d71c, "bold-italic");
  unicode_greek (0x1d756, "bold-ss");
  unicode_greek (0x1d790, "bold-italic-ss");
  unicode_digits (0x1d7ce, "bold");
  unicode_digits (0x1d7d8, "bbb");
  unicode_digits (0x1d7e2, "ss");
  unicode_digits (0x1d7ec, "bold-ss");
  unicode_digits (0x1d7f6, "tt");
}

/******************************************************************************
* Font sequences
******************************************************************************/

array<string>
trimmed_tokenize (string s, string sep) {
  return trim_spaces (tokenize (s, sep));
}

string
main_family (string f) {
  array<string> a= trimmed_tokenize (f, ",");
  for (int i=0; i<N(a); i++)
    if (N (trimmed_tokenize (a[i], "=")) <= 1)
      return a[i];
  if (N(a) == 0) return f;
  a= trimmed_tokenize (f, "=");
  return a[1];
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
  string real_shape;
  int    sz;
  int    dpi;
  int    math_nr;
  int    cyrillic_nr;
  int    math_kind;
  int    italic_nr;

  array<font> fn;
  smart_map   sm;

  smart_font_rep (string name, font base_fn, font err_fn,
                  string family, string variant,
                  string series, string shape, int sz, int dpi);
  font   get_math_font ();
  font   get_cyrillic_font ();

  void   advance (string s, int& pos, string& r, int& nr);
  int    resolve (string c, string fam, int attempt);
  int    resolve (string c);
  void   initialize_font (int nr);
  int    adjusted_dpi (string fam, string var, string ser, string sh, int att);

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
    series (series2), shape (shape2), real_shape (shape2),
    sz (sz2), dpi (dpi2),
    math_nr (-1), cyrillic_nr (-1), math_kind (0), italic_nr (-1),
    fn (2), sm (get_smart_map (tuple (family2, variant2, series2, shape2)))
{
  fn[SUBFONT_MAIN ]= base_fn;
  fn[SUBFONT_ERROR]= err_fn;
  if (is_math_family (family))
    math_nr= sm->add_font (tuple ("math"), REWRITE_MATH);
  if (family == "roman")
    cyrillic_nr= sm->add_font (tuple ("cyrillic"), REWRITE_CYRILLIC);
  if (shape == "mathitalic" || shape == "mathupright" || shape == "mathshape") {
    if (is_math_family (family)) {
      real_shape= "right";
      if (shape == "mathupright")
        this->copy_math_pars (base_fn);
      else {
        initialize_font (math_nr);
        this->copy_math_pars (fn[math_nr]);
        fn[SUBFONT_MAIN]= fn[math_nr];
      }
    } 
    else {
      math_kind= 1;
      if (shape == "mathupright") math_kind= 2;
      if (shape == "mathshape") math_kind= 3;
      real_shape= "right";
      if (math_kind == 2)
        this->copy_math_pars (base_fn);
      else {
        italic_nr= sm->add_font (tuple ("fast-italic"), REWRITE_NONE);
        initialize_font (italic_nr);
        this->copy_math_pars (fn[italic_nr]);
      }
      (void) sm->add_font (tuple ("special"), REWRITE_SPECIAL);
      (void) sm->add_font (tuple ("other"), REWRITE_NONE);
      (void) sm->add_font (tuple ("regular"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("italic"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-italic"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("cal"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-cal"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("frak"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-frak"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bbb"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("tt"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("italic-ss"), REWRITE_LETTERS);
      (void) sm->add_font (tuple ("bold-italic-ss"), REWRITE_LETTERS);
    }
  }
}

/******************************************************************************
* Fonts for backward compatibility
******************************************************************************/

font
smart_font_rep::get_math_font () {
  string fam= family;
  string var= variant;
  string ser= series;
  string sh = real_shape;
  if (!is_math_family (fam)) fam= "roman";
  find_closest (fam, var, ser, sh);
  string mvar= "mr";
  if (var == "ss") mvar= "ms";
  if (var == "tt") mvar= "mt";
  return find_font (fam, mvar, ser, "", sz, dpi);
}

font
smart_font_rep::get_cyrillic_font () {
  string fam= family;
  string var= variant;
  string ser= series;
  string sh = real_shape;
  find_closest (fam, var, ser, sh);
  return find_font ("cyrillic", var, ser, sh, sz, dpi);
}

static string
rewrite_math (string s) {
  string r;
  int i= 0, n= N(s);
  while (i < n) {
    int start= i;
    tm_char_forwards (s, i);
    if (s[start] == '<' && start+1 < n && s[start+1] == '#' && s[i-1] == '>')
      r << utf8_to_cork (cork_to_utf8 (s (start, i)));
    else r << s (start, i);
  }
  return r;
}

static string
rewrite_letters (string s) {
  init_unicode_substitution ();
  string r;
  int i= 0, n= N(s);
  while (i < n) {
    int start= i;
    tm_char_forwards (s, i);
    string ss= s (start, i);
    if (substitution_char->contains (ss)) r << substitution_char[ss];
    else r << ss;
  }
  return r;
}

static string
rewrite (string s, int kind) {
  switch (kind) {
  case REWRITE_NONE:
    return s;
  case REWRITE_MATH:
    return rewrite_math (s);
  case REWRITE_CYRILLIC:
    return code_point_to_cyrillic_subset_in_t2a (s);
  case REWRITE_LETTERS:
    return rewrite_letters (s);
  case REWRITE_SPECIAL:
    return special_table [s];
  default:
    return s;
  }
}

/******************************************************************************
* Smart font resolution
******************************************************************************/

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
      if (math_kind != 0 && math_kind != 2 && is_letter (c) &&
          (pos == 0 || !is_letter (s[pos-1])) &&
          (pos+1 == N(s) || !is_letter (s[pos+1])))
        next= italic_nr;
      else if (chv[c] == -1) next= resolve (s (pos, pos+1));
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
  if (N(fn) <= nr || is_nil (fn[nr])) initialize_font (nr);
  if (sm->fn_rewr[nr] != REWRITE_NONE)
    r= rewrite (r, sm->fn_rewr[nr]);
  //cout << "Got " << r << " in " << fn[nr]->res_name << "\n";
}

int
smart_font_rep::resolve (string c, string fam, int attempt) {
  bool ok= true;
  if (fam == "cal" || fam == "cal*" ||
      fam == "Bbb" || fam == "Bbb****")
    ok= ok && is_alpha (c) && upcase_all (c) == c;
  if (fam == "cal**" || fam == "Bbb*")
    ok= ok && is_alpha (c);
  
  if (attempt == 1 && fam == main_family (family)) {
    if (ok && fn[SUBFONT_MAIN]->supports (c))
      return sm->add_char (tuple ("main"), c);
    return -1;
  }

  string uc= cork_to_utf8 (c);
  int pos= 0;
  int code= decode_from_utf8 (uc, pos);
  string range= "";
  if (code <= 0x7f) range= "ascii";
  else if (code >= 0x80 && code <= 0x37f) range= "latin";
  else if (code >= 0x380 && code <= 0x3ff) range= "greek";
  else if (code >= 0x400 && code <= 0x4ff) range= "cyrillic";
  else if (code >= 0x4e00 && code <= 0x9fcc) range= "cjk";
  else if (code >= 0xac00 && code <= 0xd7af) range= "hangul";
  else if (code >= 0x2000 && code <= 0x23ff) range= "mathsymbols";
  else if (code >= 0x2900 && code <= 0x2e7f) range= "mathextra";
  else if (code >= 0x1d400 && code <= 0x1d7ff) range= "mathletters";

  if ((attempt == 1 && ok) || (attempt > 1 && pos == N(uc))) {
    int a= attempt - 1;
    string v= variant;
    if (attempt == 1) a= 1;
    else if (v == "rm") v= range;
    else v= v * "-" * range;
    font cfn= closest_font (fam, v, series, real_shape, sz, dpi, a);
    //cout << "Trying " << c << " in " << cfn->res_name << "\n";
    if (cfn->supports (c)) {
      tree key= tuple (fam, v, series, real_shape, as_string (a));
      return sm->add_char (key, c);
    }
  }
  if (attempt == 1) return -1;

  if (math_nr >= 0) {
    initialize_font (math_nr);
    if (fn[math_nr]->supports (rewrite (c, REWRITE_MATH)))
      return sm->add_char (tuple ("math"), c);
  }
  if (cyrillic_nr >= 0) {
    initialize_font (cyrillic_nr);
    if (fn[cyrillic_nr]->supports (rewrite (c, REWRITE_CYRILLIC)))
      return sm->add_char (tuple ("cyrillic"), c);
  }

  if (math_kind != 0 && range == "mathletters") {
    init_unicode_substitution ();
    string nc= "<#" * as_hexadecimal (code) * ">";
    string sc= substitution_char [nc];
    string sf= substitution_font [nc];
    //cout << c << " (" << nc << ") -> " << sc << ", " << sf << "\n";
    if (sc != "" && sc != c) {
      bool flag= ends (sf, "cal") || ends (sf, "frak") || ends (sf, "bbb");
      if (!flag || math_kind == 2) return sm->add_char (tuple (sf), c);
    }
  }

  return -1;
}

int
smart_font_rep::resolve (string c) {
  if (math_kind != 0) {
    if (is_greek (c))
      return sm->add_char (tuple ("italic"), c);
    if (is_special (c))
      return sm->add_char (tuple ("special"), c);
  }

  for (int attempt= 1; attempt <= 20; attempt++) {
    int nr= resolve (c, family, attempt);
    if (nr >= 0) return nr;
  }

  string virt= find_in_virtual (c);
  if (virt != "") {
    //cout << "Found " << c << " in " << virt << "\n";
    return sm->add_char (tuple ("virtual", virt), c);
  }

  if (math_kind != 0 && !unicode_provides (c))
    return sm->add_char (tuple ("other"), c);

  return sm->add_char (tuple ("error"), c);
}

void
smart_font_rep::initialize_font (int nr) {
  if (N(fn) <= nr) fn->resize (nr+1);
  if (!is_nil (fn[nr])) return;
  array<string> a= tuple_as_array (sm->fn_spec[nr]);
  if (a[0] == "math")
    fn[nr]= get_math_font ();
  else if (a[0] == "cyrillic")
    fn[nr]= get_cyrillic_font ();
  else if (a[0] == "special")
    fn[nr]= smart_font (family, variant, series, "right", sz, dpi);
  else if (a[0] == "other") {
    int ndpi= adjusted_dpi ("roman", variant, series, "mathitalic", 1);
    fn[nr]= smart_font ("roman", variant, series, "mathitalic", sz, ndpi);
  }
  else if (a[0] == "bold")
    fn[nr]= smart_font (family, variant, "bold", "right", sz, dpi);
  else if (a[0] == "fast-italic")
    fn[nr]= smart_font (family, variant, series, "italic", sz, dpi);
  else if (a[0] == "italic")
    fn[nr]= smart_font (family, variant, series, "italic", sz, dpi);
  else if (a[0] == "bold-italic")
    fn[nr]= smart_font (family, variant, "bold", "italic", sz, dpi);
  else if (a[0] == "tt")
    fn[nr]= smart_font (family, "tt", series, "right", sz, dpi);
  else if (a[0] == "ss")
    fn[nr]= smart_font (family, "tt", series, "right", sz, dpi);
  else if (a[0] == "bold-ss")
    fn[nr]= smart_font (family, "tt", "bold", "right", sz, dpi);
  else if (a[0] == "italic-ss")
    fn[nr]= smart_font (family, "tt", series, "italic", sz, dpi);
  else if (a[0] == "bold-italic-ss")
    fn[nr]= smart_font (family, "tt", "bold", "italic", sz, dpi);
  else if (a[0] == "cal" && N(a) == 1)
    fn[nr]= smart_font (family, "calligraphic", series, "italic", sz, dpi);
  else if (a[0] == "bold-cal")
    fn[nr]= smart_font (family, "calligraphic", "bold", "italic", sz, dpi);
  else if (a[0] == "frak")
    fn[nr]= smart_font (family, "gothic", series, "right", sz, dpi);
  else if (a[0] == "bold-frak")
    fn[nr]= smart_font (family, "gothic", "bold", "right", sz, dpi);
  else if (a[0] == "bbb" && N(a) == 1)
    fn[nr]= smart_font (family, "outline", series, "right", sz, dpi);
  else if (a[0] == "virtual")
    fn[nr]= virtual_font (this, a[1], sz, dpi);
  else {
    int ndpi= adjusted_dpi (a[0], a[1], a[2], a[3], as_int (a[4]));
    fn[nr]= closest_font (a[0], a[1], a[2], a[3], sz, ndpi, as_int (a[4]));
  }
  //cout << "Font " << nr << ", " << a << " -> " << fn[nr]->res_name << "\n";
  if (fn[nr]->res_name == res_name) {
    cout << "Font " << nr << ", " << a << " -> " << fn[nr]->res_name << "\n";
    ASSERT (false, "substitution font loop detected");
  }
}

static int
get_ex (string family, string variant, string series, string shape,
	int attempt) {
  array<string> lfn= logical_font (family, variant, series, shape);
  array<string> pfn= search_font (lfn, attempt);
  array<string> chs= font_database_characteristics (pfn[0], pfn[1]);
  string ex= find_attribute_value (chs, "ex");
  if (ex == "") return 0;
  else return as_int (ex);
}

int
smart_font_rep::adjusted_dpi (string fam, string var, string ser, string sh,
                              int attempt) {
  int ex1= get_ex (family, variant, series, real_shape, 1);
  int ex2= get_ex (fam, var, ser, sh, attempt);
  double zoom= 1.0;
  if (ex1 != 0 && ex2 != 0) zoom= ((double) ex1) / ((double) ex2);
  if (zoom > 0.975 && zoom < 1.025) zoom= 1;
  return (int) tm_round (dpi * zoom);
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
  //cout << "Extents of " << s << " for " << res_name << "\n";
  int i=0, n= N(s);
  if (n == 0) fn[0]->get_extents (empty_string, ex);
  else {
    int nr;
    string r= s;
    metric ey;
    while (i < n) {
      advance (s, i, r, nr);
      if (nr >= 0) {
        //cout << "From " << nr << " -> " << sm->fn_spec[nr] << "\n";
        fn[nr]->get_extents (r, ex);
        break;
      }
    }
    while (i < n) {
      advance (s, i, r, nr);
      if (nr >= 0) {
        //cout << "From " << nr << " -> " << sm->fn_spec[nr] << "\n";
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
  if (!new_fonts) return find_font (family, variant, series, shape, sz, dpi);
  if (starts (family, "tc"))
    // FIXME: temporary hack for symbols from std-symbol.ts
    return find_font (family, variant, series, shape, sz, dpi);

  string name=
    family * "-" * variant * "-" *
    series * "-" * shape * "-" *
    as_string (sz) * "-" * as_string (dpi) * "-smart";
  if (font::instances->contains (name)) return font (name);
  string sh= shape;
  if (shape == "mathitalic" || shape == "mathshape") sh= "right";
  string mfam= main_family (family);
  font base_fn= closest_font (mfam, variant, series, sh, sz, dpi);
  if (is_nil (base_fn)) return font ();
  font sec_fn= closest_font ("modern", "ss", "medium", "right", sz, dpi);
  font err_fn= error_font (sec_fn);
  return make (font, name,
               tm_new<smart_font_rep> (name, base_fn, err_fn, family, variant,
                                       series, shape, sz, dpi));
}

font
smart_font (string family, string variant, string series, string shape,
            string tfam, string tvar, string tser, string tsh,
            int sz, int dpi) {
  if (!new_fonts) return find_font (family, variant, series, shape, sz, dpi);
  if (tfam == "roman") tfam= family;
  if (variant != "mr") {
    if (variant == "ms") tvar= "ss";
    if (variant == "mt") tvar= "tt";
  }
  if (shape == "right") tsh= "mathupright";
  return smart_font (tfam, tvar, tser, tsh, sz, dpi);
}
