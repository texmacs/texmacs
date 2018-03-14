
/******************************************************************************
* MODULE     : font.hpp
* DESCRIPTION: fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FONT_H
#define FONT_H
#include "space.hpp"
#include "renderer.hpp"

RESOURCE(font);

struct glyph;
struct font_metric;
struct font_glyphs;

#define FONT_ATTEMPTS        20

#define FONT_TYPE_TEX         0
#define FONT_TYPE_UNICODE     1
#define FONT_TYPE_QT          2
#define FONT_TYPE_OTHER       3

#define MATH_TYPE_NORMAL      0
#define MATH_TYPE_STIX        1
#define MATH_TYPE_TEX_GYRE    2

#define START_OF_LINE         1
#define END_OF_LINE           2
#define PROTRUSION_MASK      60
#define CJK_PROTRUSION_MASK  28
#define QUANJIAO              4
#define BANJIAO               8
#define HANGMOBANJIAO        12
#define KAIMING              16
#define WESTERN_PROTRUSION   32
#define TABLE_CELL           64

/******************************************************************************
* The font structure
******************************************************************************/

struct font_rep: rep<font> {
  int      type;             // font type
  int      math_type;        // For TeX Gyre math fonts and Stix
  SI       size;             // requested size
  SI       design_size;      // design size in points/256
  SI       display_size;     // display size in points/PIXEL
  double   slope;            // italic slope
  space    spc;              // usual space between words
  space    extra;            // extra space at end of words
  SI       sep;              // separation space between close components

  SI       y1;               // bottom y position
  SI       y2;               // top y position
  SI       yx;               // height of the x character
  SI       yfrac;            // vertical position fraction bar
  SI       ysub_lo_base;     // base line for subscripts
  SI       ysub_hi_lim;      // upper limit for subscripts
  SI       ysup_lo_lim;      // lower limit for supscripts
  SI       ysup_lo_base;     // base line for supscripts
  SI       ysup_hi_lim;      // upper limit for supscripts
  SI       yshift;           // vertical script shift inside fractions

  SI       wpt;              // width of one point in font
  SI       hpt;              // height of one point in font (usually wpt)
  SI       wfn;              // wpt * design size in points
  SI       wline;            // width of fraction bars and so
  SI       wquad;            // quad space (often width of widest character M)

  double   last_zoom;        // last rendered zoom
  font     zoomed_fn;        // zoomed font for last_zoom (or nil)

  // Microtypography
  SI   global_lsub_correct;  // global left subscript correction
  SI   global_lsup_correct;  // global left superscript correction
  SI   global_rsub_correct;  // global right subscript correction
  SI   global_rsup_correct;  // global right superscript correction
  hashmap<string,double> lsub_correct;     // left subscript adjustments
  hashmap<string,double> lsup_correct;     // left superscript adjustments
  hashmap<string,double> rsub_correct;     // right subscript adjustments
  hashmap<string,double> rsup_correct;     // right superscript adjustments
  hashmap<string,double> above_correct;    // wide accent above adjustments
  hashmap<string,double> below_correct;    // wide accent above adjustments
  hashmap<int,int>       protrusion_maps;  // tables for protrusion

  font_rep (string name);
  font_rep (string name, font fn);
  void copy_math_pars (font fn);

  virtual bool   supports (string c) = 0;
  virtual void   get_extents (string s, metric& ex) = 0;
  virtual void   get_extents (string s, metric& ex, bool ligf);
  virtual void   get_extents (string s, metric& ex, SI xk);
  virtual void   get_xpositions (string s, SI* xpos);
  virtual void   get_xpositions (string s, SI* xpos, bool ligf);
  virtual void   get_xpositions (string s, SI* xpos, SI xk);
  virtual void   draw_fixed (renderer ren, string s, SI x, SI y) = 0;
  virtual void   draw_fixed (renderer ren, string s, SI x, SI y, bool ligf);
  virtual void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  virtual font   poor_magnify (double zoomx, double zoomy);
  virtual font   magnify (double zoomx, double zoomy) = 0;
  virtual font   magnify (double zoom);
  virtual void   draw (renderer ren, string s, SI x, SI y, SI xk, bool ext);
  virtual void   draw (renderer ren, string s, SI x, SI y);
  virtual void   draw (renderer ren, string s, SI x, SI y, SI xk);

  virtual double get_left_slope  (string s);
  virtual double get_right_slope (string s);
  virtual SI     get_left_correction  (string s);
  virtual SI     get_right_correction (string s);
  virtual SI     get_lsub_correction  (string s);
  virtual SI     get_lsup_correction  (string s);
  virtual SI     get_rsub_correction  (string s);
  virtual SI     get_rsup_correction  (string s);
  virtual SI     get_left_protrusion  (string s, int mode);
  virtual SI     get_right_protrusion (string s, int mode);
  virtual SI     get_wide_correction (string s, int mode);

  void var_get_extents (string s, metric& ex);
  void var_get_xpositions (string s, SI* xpos);
  void var_draw (renderer ren, string s, SI x, SI y);

  virtual void  advance_glyph (string s, int& pos, bool ligf);
  virtual glyph get_glyph (string s);
  virtual int   index_glyph (string s, font_metric& fnm, font_glyphs& fng);
};

string default_chinese_font_name ();
string default_japanese_font_name ();
string default_korean_font_name ();

font error_font (font fn);
font virtual_font (font base, string fam, int sz, int hdpi, int vdpi, bool ext);
font virtual_enhance_font (font base, string virt);
font tt_font (string family, int size, int dpi);
font unicode_font (string family, int size, int dpi);
font unicode_math_font (font up, font it, font bup, font bit, font fb);
font rubber_unicode_font (font base);
font rubber_stix_font (font base);
font rubber_assemble_font (font base);
font rubber_font (font base);
bool use_poor_rubber (font fn);
font poor_rubber_font (font base);
font poor_smallcaps_font (font base);
font poor_italic_font (font base, double slant);
font poor_stretched_font (font base, double zoomx, double zoomy);
font poor_extended_font (font base, double factor, double lw);
font poor_extended_font (font base, double factor);
font poor_bold_font (font base, double lofat, double upfat);
font poor_bold_font (font base);
font poor_bbb_font (font base, double penw, double penh, double fatw);
font poor_bbb_font (font base);
font poor_distorted_font (font base, tree kind);
font x_font (string family, int size, int dpi);
font qt_font (string family, int size, int dpi);
font tex_font (string fam, int size, int dpi, int dsize=10);
font tex_cm_font (string fam, int size, int dpi, int dsize=10);
font tex_ec_font (string fam, int size, int dpi, int dsize=10);
font tex_la_font (string fam, int size, int dpi, int dsize=10);
font tex_gr_font (string fam, int size, int dpi, int dsize=10);
font tex_adobe_font (string fam, int size, int dpi, int dsize=10);
font tex_rubber_font (string trl_name,
		      string fam, int size, int dpi, int dsize=10);
font tex_dummy_rubber_font (font base_fn);

void font_rule (tree which, tree by);
font find_font (scheme_tree t);
font find_magnified_font (scheme_tree t, double zoomx, double zoomy);
font find_font (string family, string fn_class,
		string series, string shape, int sz, int dpi);
bool find_closest (string& family, string& variant,
                   string& series, string& shape, int attempt= 1);
font closest_font (string family, string variant, string series, string shape,
		   int sz, int dpi, int attempt= 1);

font math_font (scheme_tree t, font base_fn, font error_fn,
                double zoomx, double zoomy);
font compound_font (scheme_tree def, double zoomx, double zoomy);
font smart_font (string family, string variant, string series, string shape,
                 int sz, int dpi);
font smart_font (string family, string variant, string series, string shape,
                 string tf, string tv, string tw, string ts, int sz, int dpi);
font apply_effects (font fn, string effects);

int  script (int sz, int level);

// Microtypography
void adjust_char (hashmap<string,double>& t, string c, double delta);
void adjust_pair (hashmap<string,double>& t, string c, double delta);
void rsub_adjust_std (hashmap<string,double>& t);
void rsup_adjust_std (hashmap<string,double>& t);
void above_adjust_std (hashmap<string,double>& t);
void below_adjust_std (hashmap<string,double>& t);
void above_adjust_frak (hashmap<string,double>& t, double force);
void above_adjust_bbb (hashmap<string,double>& t, double force);

// Font database
extern bool new_fonts;
void set_new_fonts (bool new_val);
bool get_new_fonts ();
void font_database_build (url u);
void font_database_build_local ();
void font_database_extend_local (url u);
void font_database_build_global ();
void font_database_build_global (url u);
void font_database_build_characteristics (bool force);
void font_database_load ();
void font_database_global_load ();
void font_database_save ();
void font_database_filter ();
void font_database_save_local_delta ();
array<string> font_database_families ();
array<string> font_database_delta_families ();
array<string> font_database_styles (string family);
array<string> font_database_global_styles (string family);
array<string> font_database_search (string family, string style);
array<string> font_database_search (string fam, string var,
                                    string series, string shape);
array<string> font_database_characteristics (string family, string style);
tree font_database_substitutions (string family);

// Font selection
tree array_as_tuple (array<string> a);
array<string> tuple_as_array (tree t);
string encode_feature (string s);
string family_to_master (string f);
array<string> master_to_families (string f);
array<string> master_features (string m);
array<string> family_features (string f);
array<string> family_strict_features (string f);
array<string> style_features (string s);
array<string> logical_font (string family, string shape);
array<string> logical_font_exact (string family, string style);
array<string> logical_font (string f, string v, string ser, string sh);
array<string> guessed_features (string family, string shape);
array<string> guessed_features (string family, bool pure_guess);
double guessed_distance (string fam1, string sty1, string fam2, string sty2);
double guessed_distance (string master1, string master2);
string get_family (array<string> v);
string get_variant (array<string> v);
string get_series (array<string> v);
string get_shape (array<string> v);
array<string> search_font (array<string> v, int attempt= 1);
array<string> search_font_exact (array<string> v);
array<string> search_font_families (array<string> v);
array<string> search_font_styles (string s, array<string> v);
array<string> patch_font (array<string> v, array<string> w, bool decode= true);
array<string> apply_substitutions (array<string> v);
string main_family (string f);

#endif // defined FONT_H
