
/******************************************************************************
* MODULE     : tex_font.cpp
* DESCRIPTION: TeX text fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "Metafont/load_tex.hpp"
#include "translator.hpp"
#include "iterator.hpp"
#include "gui.hpp"

#define TEX_ANY   0
#define TEX_EC    1
#define TEX_LA    2
#define TEX_GR    3
#define TEX_CM    4
#define TEX_ADOBE 5

static void special_initialize ();
font_metric tfm_font_metric (tex_font_metric tfm, font_glyphs pk, double unit);

hashmap<string,double> rsub_cmr_table ();
hashmap<string,double> rsup_cmr_table ();
hashmap<string,double> rsub_cmmi_table ();
hashmap<string,double> rsup_cmmi_table ();
hashmap<string,double> above_cmmi_table ();
hashmap<string,double> above_cmsy_table ();
hashmap<string,double> rsub_bbm_table ();
hashmap<string,double> rsup_bbm_table ();

/******************************************************************************
* TeX text fonts
******************************************************************************/

struct tex_font_rep: font_rep {
  int              status;
  string           family;
  int              dpi;
  int              dsize;
  tex_font_metric  tfm;
  font_glyphs      pk;
  double           unit;
  bool             exec;             // execute ligature and kerning program?

  tex_font_rep (string name, int status,
		string family, int size, int dpi, int dsize);

  bool  raw_supports (unsigned char c);
  bool  supports (string c);
  void  get_extents (string s, metric& ex);
  void  get_xpositions (string s, SI* xpos, bool ligf);
  void  get_xpositions (string s, SI* xpos);
  void  draw_fixed (renderer ren, string s, SI x, SI y);
  font  magnify (double zoomx, double zoomy);
  SI    get_left_correction  (string s);
  SI    get_right_correction (string s);
  SI    get_rsub_correction  (string s);
  SI    get_rsup_correction  (string s);
  SI    get_wide_correction  (string s, int mode);
  void  advance_glyph (string s, int& pos);
  glyph get_glyph (string s);
  int   index_glyph (string s, font_metric& fnm, font_glyphs& fng);
  int   get_ligature_code (string s);
  void  special_get_extents (string s, metric& ex);
  void  special_get_xpositions (string s, SI* xpos, bool ligf);
  void  special_draw (renderer ren, string s, SI x, SI y);
  SI    special_get_left_correction (string s);
  SI    special_get_right_correction (string s);
  void  accented_get_extents (string s, metric& ex);
  void  accented_get_xpositions (string s, SI* xpos, bool ligf);
  void  accented_draw (renderer ren, string s, SI x, SI y);
  SI    accented_get_left_correction (string s);
  SI    accented_get_right_correction (string s);
};

/******************************************************************************
* The implementation of tex_fonts
******************************************************************************/

#define conv(x) ((SI) (((double) (x))*unit))

tex_font_rep::tex_font_rep (string name, int status2,
  string family2, int size2, int dpi2, int dsize2):
  font_rep (name), status (status2), dsize (dsize2)
{
  load_tex (family2, size2, dpi2, dsize, tfm, pk);

  family       = family2;
  type         = FONT_TYPE_TEX;
  size         = size2;
  dpi          = dpi2;
  design_size  = tfm->design_size () >> 12;
  display_size = (((design_size*dpi)/72)*PIXEL) >> 8;
  unit         = ((double) display_size) / ((double) (1<<20));
  slope        = tfm->slope ();
  spc->def     = conv (tfm->spc ());
  spc->min     = spc->def - conv (tfm->spc_shrink ());
  spc->max     = spc->def + conv (tfm->spc_stretch ());
  extra        = conv (tfm->spc_extra ());
  extra->min   = extra->min >> 1;
  extra->max   = extra->min << 1;
  sep          = ((((dpi*PIXEL)/72)*design_size) >> 8) / 10;
  exec         = !ends (family, "tt");

  y1           = conv (-262080);   // -0.25 quad
  y2           = y1+ display_size; //  0.75 quad
  yx           = conv (tfm->x_height ());
  yfrac        = yx >> 1;
  ysub_lo_base = -yx/3;
  ysub_hi_lim  = (5*yx)/6;
  ysup_lo_lim  = yx/2;
  ysup_lo_base = (5*yx)/6;
  ysup_hi_lim  = yx;
  yshift       = yx/6;

  wpt          = (dpi*PIXEL)/72;
  hpt          = (dpi*PIXEL)/72;
  wfn          = (wpt*design_size) >> 8;
  wline        = wfn/20;
  wquad        = conv (tfm->spc_quad ());

  if ((family == "cmr") || (family == "ecrm") || (family == "cmmi")) {
    if (size < 8)
      wline= wfn / (size==7? 16: (size==6? 14: 12));
    else if (size < 10) yfrac += (size * wfn) / 1600;
    else if (size <= 14) yfrac += (size * wfn) / 1000;
    else {
      wline= wfn / (size>16? 28: 24);
      yfrac += (size * wfn) / 700;
    }
  }

  special_initialize ();

  if (family == "cmr" || family == "cmbx") {
    rsub_correct= rsub_cmr_table ();
    rsup_correct= rsup_cmr_table ();
    above_correct= hashmap<string,double> (0.0);
  }
  else if (family == "cmmi" || family == "cmmib") {
    rsub_correct= rsub_cmmi_table ();
    rsup_correct= rsup_cmmi_table ();
    above_correct= above_cmmi_table ();
  }
  else if (family == "cmsy" || family == "cmbsy") {
    rsub_correct= hashmap<string,double> (0.0);
    rsup_correct= hashmap<string,double> (0.0);
    above_correct= above_cmsy_table ();
  }
  else if (family == "bbm" || family == "bbmbx") {
    rsub_correct= rsub_bbm_table ();
    rsup_correct= rsup_bbm_table ();
    above_correct= hashmap<string,double> (0.0);
  }
  else {
    rsub_correct= hashmap<string,double> (0.0);
    rsup_correct= hashmap<string,double> (0.0);
    above_correct= hashmap<string,double> (0.0);
  }
}

/******************************************************************************
* Handle <, > and (in the future?) other special characters
******************************************************************************/

static bool special_initialized= false;
static hashmap<string,string> special_translate ("");

static void
special_initialize (string enc) {
  translator trl= load_translator (enc);
  iterator<string> it= iterate (trl->dict);
  while (it->busy ()) {
    string s= it->next ();
    special_translate (s)= string ((char) (unsigned char) trl->dict[s]);
    if (N(s) > 2 && s(0,2) == "<#") {
      string sl= locase_all (s), su= upcase_all (s);
      special_translate (sl)= string ((char) (unsigned char) trl->dict[s]);
      special_translate (su)= string ((char) (unsigned char) trl->dict[s]);
    }
  }
}

static void
special_initialize () {
  if (special_initialized) return;
  special_translate ("<less>")= "<";
  special_translate ("<gtr>")= ">";
  special_initialize ("larm");
  special_initialize ("grmn");
  special_initialized= true;
}

void
tex_font_rep::special_get_extents (string s, metric& ex) {
  register int i, j;
  for (i=0; i<N(s); i++)
    if (s[i]=='<') break;
  get_extents (s (0, i), ex);
  for (j=i+1; j<N(s); j++)
    if (s[j]=='>') break;
  if (j<N(s)) j++;

  SI x;
  metric ey;
  int temp= status;
  status= TEX_ANY;
  string r = s (i, j);
  string rr= special_translate[r];
  if (N(rr) != 0) r= rr;
  get_extents (r, ey);
  x= ex->x2;
  ex->x1= min (ex->x1, x+ ey->x1); ex->y1= min (ex->y1, ey->y1);
  ex->x2= max (ex->x2, x+ ey->x2); ex->y2= max (ex->y2, ey->y2);
  ex->x3= min (ex->x3, x+ ey->x3); ex->y3= min (ex->y3, ey->y3);
  ex->x4= max (ex->x4, x+ ey->x4); ex->y4= max (ex->y4, ey->y4);
  status= temp;
  
  get_extents (s (j, N(s)), ey);
  x= ex->x2;
  ex->x1= min (ex->x1, x+ ey->x1); ex->y1= min (ex->y1, ey->y1);
  ex->x2= max (ex->x2, x+ ey->x2); ex->y2= max (ex->y2, ey->y2);
  ex->x3= min (ex->x3, x+ ey->x3); ex->y3= min (ex->y3, ey->y3);
  ex->x4= max (ex->x4, x+ ey->x4); ex->y4= max (ex->y4, ey->y4);
}

void
tex_font_rep::special_get_xpositions (string s, SI* xpos, bool ligf) {
  SI offset= 0;
  register int l=0, i, j, n=N(s);
  while (l<n) {
    for (i=l; i<n; i++)
      if (s[i]=='<') break;
    if (l<i) {
      get_xpositions (s (l, i), xpos + l, ligf);
      for (j=l+1; j<=i; j++) xpos[j] += offset;
      if (i==n) break;
      offset= xpos[i];
    }

    for (j=i+1; j<n; j++) {
      xpos[j]= offset;
      if (s[j]=='>') break;
    }
    if (j<n) j++;
    metric ey;
    int temp= status;
    status= TEX_ANY;
    string r= s (i, j);
    string rr= special_translate[r];
    if (N(rr) != 0) r= rr;
    get_extents (r, ey);
    status= temp;
    offset += ey->x2;
    xpos[j]= offset;
    l= j;
  }
}

void
tex_font_rep::special_draw (renderer ren, string s, SI x, SI y) {
  register int i, j;
  metric ex;
  for (i=0; i<N(s); i++)
    if (s[i]=='<') break;
  draw_fixed (ren, s (0, i), x, y);
  get_extents (s (0, i), ex);
  x += ex->x2;
  for (j=i+1; j<N(s); j++)
    if (s[j]=='>') break;
  if (j<N(s)) j++;

  int temp= status;
  status= TEX_ANY;
  string r= s (i, j);
  string rr= special_translate[r];
  pencil pen= ren->get_pencil ();
  if (N(rr) != 0) r= rr;
  else ren->set_pencil (red);
  draw_fixed (ren, r, x, y);
  ren->set_pencil (pen);
  get_extents (r, ex);
  x += ex->x2;
  status= temp;
  
  draw_fixed (ren, s (j, N(s)), x, y);
}

SI
tex_font_rep::special_get_left_correction (string s) {
  int i= 0;
  tm_char_forwards (s, i);
  string r= special_translate (s (0, i));
  if (N(r)!=0) return (SI) (slope * conv (tfm->d ((QN) r[0])));
  return (SI) (slope * conv (tfm->d ((QN) '<')));
}

SI
tex_font_rep::special_get_right_correction (string s) {
  int n= N(s), i= n;
  tm_char_backwards (s, i);
  string r= special_translate (s (i, n));
  if (N(r)!=0) return conv (tfm->i ((QN) r[0]));
  return conv (tfm->i ((QN) '>'));
}

/******************************************************************************
* Handle accents
******************************************************************************/

static char CM_unaccented[128]= {
    'A', ' ', 'C', 'C',   'D',   'E',   ' ',   'G', 
    'L', 'L', ' ', 'N',   'N',   ' ',   'O',   'R', 
    'R', 'S', 'S', 'S',   'T',   'T',   'U',   'U', 
    'Y', 'Z', 'Z', 'Z',   ' ',   'I',   'd',   ' ', 
    'a', ' ', 'c', 'c',   'd',   'e',   ' ',   'g', 
    'l', 'l', ' ', 'n',   'n',   ' ',   'o',   'r', 
    'r', 's', 's', 's',   't',   't',   'u',   'u', 
    'y', 'z', 'z', 'z',   ' ', '\74', '\76',   ' ', 
    'A', 'A', 'A', 'A',   'A',   'A', '\35',   'C', 
    'E', 'E', 'E', 'E',   'I',   'I',   'I',   'I', 
    'D', 'N', 'O', 'O',   'O',   'O',   'O', '\36', 
  '\37', 'U', 'U', 'U',   'U',   'Y',   ' ',   ' ', 
    'a', 'a', 'a', 'a',   'a',   'a', '\32',   'c', 
    'e', 'e', 'e', 'e', '\20', '\20', '\20', '\20', 
    'd', 'n', 'o', 'o',   'o',   'o',   'o', '\33', 
  '\34', 'u', 'u', 'u',   'u',   'y',   ' ', '\31'
};

static char CM_accents[128]= {
   '\25',    ' ',  '\23',  '\24',  '\24',  '\24',    ' ',  '\25', 
   '\23',  '\47',    ' ',  '\23',  '\24',    ' ', '\175',  '\23', 
   '\24',  '\23',  '\24',  '\30',  '\24',  '\30', '\175',  '\27', 
  '\177',  '\23',  '\24', '\137',    ' ', '\137',  '\26',    ' ', 
   '\25',    ' ',  '\23',  '\24',  '\24',  '\24',    ' ',  '\25', 
   '\23',  '\47',    ' ',  '\23',  '\24',    ' ', '\175',  '\23', 
   '\24',  '\23',  '\24',  '\30',  '\24',  '\30', '\175',  '\27', 
  '\177',  '\23',  '\24', '\137',    ' ',    ' ',    ' ',    ' ', 
   '\22',  '\23', '\136', '\176', '\177',  '\27',    ' ',  '\30', 
   '\22',  '\23', '\136', '\177',  '\22',  '\23', '\136', '\177', 
   '\26', '\176',  '\22',  '\23', '\136', '\176', '\177',    ' ', 
     ' ',  '\22',  '\23', '\136', '\177',  '\23',    ' ',    ' ', 
   '\22',  '\23', '\136', '\176', '\177',  '\27',    ' ',  '\30', 
   '\22',  '\23', '\136', '\177',  '\22',  '\23', '\136', '\177', 
   '\26', '\176',  '\22',  '\23', '\136', '\176', '\177',    ' ', 
     ' ',  '\22',  '\23', '\136', '\177',  '\23',    ' ',    ' ' 
};

static char ADOBE_unaccented[128]= {
     'A', 'A',    'C', 'C',    'D',    'E',    'E',    'G', 
     'L', 'L', '\350', 'N',    'N',    ' ',    'O',    'R', 
     'R', 'S',    'S', 'S',    'T',    'T',    'U',    'U', 
     'Y', 'Z',    'Z', 'Z',    ' ',    'I',    'd', '\247', 
     'a', 'a',    'c', 'c',    'd',    'e',    'e',    'g', 
     'l', 'l', '\370', 'n',    'n',    ' ',    'o',    'r', 
     'r', 's',    's', 's',    't',    't',    'u',    'u', 
     'y', 'z',    'z', 'z',    ' ', '\241', '\277', '\243', 
     'A', 'A',    'A', 'A',    'A',    'A', '\341',    'C', 
     'E', 'E',    'E', 'E',    'I',    'I',    'I',    'I', 
     'D', 'N',    'O', 'O',    'O',    'O',    'O',  '\36', 
  '\351', 'U',    'U', 'U',    'U',    'Y',    ' ',    ' ', 
     'a', 'a',    'a', 'a',    'a',    'a', '\361',    'c', 
     'e', 'e',    'e', 'e', '\365', '\365', '\365', '\365', 
     'd', 'n',    'o', 'o',    'o',    'o',    'o', '\372', 
  '\371', 'u',    'u', 'u',    'u',    'y',    ' ', '\373'
};

static char ADOBE_accents[128]= {
  '\306', '\316', '\302', '\317', '\317', '\317', '\316', '\306', 
  '\302',  '\47',    ' ', '\302', '\317',    ' ', '\315', '\302', 
  '\317', '\302', '\317', '\313', '\317', '\313', '\315', '\312', 
  '\310', '\302', '\317', '\307',    ' ', '\307', '\305',    ' ', 
  '\306', '\316', '\302', '\317', '\317', '\317', '\316', '\306', 
  '\302',  '\47',    ' ', '\302', '\317',    ' ', '\315', '\302', 
  '\317', '\302', '\317', '\313', '\317', '\313', '\315', '\312', 
  '\310', '\302', '\317', '\307',    ' ',    ' ',    ' ',    ' ', 
  '\301', '\302', '\303', '\304', '\310', '\312',    ' ', '\313', 
  '\301', '\302', '\303', '\310', '\301', '\302', '\303', '\310', 
  '\305', '\304', '\301', '\302', '\303', '\304', '\310',    ' ', 
     ' ', '\301', '\302', '\303', '\310', '\302',    ' ',    ' ', 
  '\301', '\302', '\303', '\304', '\310', '\312',    ' ', '\313', 
  '\301', '\302', '\303', '\310', '\301', '\302', '\303', '\310', 
  '\305', '\304', '\301', '\302', '\303', '\304', '\310',    ' ', 
     ' ', '\301', '\302', '\303', '\310', '\302',    ' ',    ' ' 
};

static char* the_unaccented;
static char* the_accents;

#define ACCENTS_PREPARE \
  if (status==TEX_CM) { \
    the_unaccented= CM_unaccented; \
    the_accents   = CM_accents; \
  } \
  else { \
    the_unaccented= ADOBE_unaccented; \
    the_accents   = ADOBE_accents; \
  }

static string
get_unaccented (string s) {
  int i;
  string r(N(s));
  for (i=0; i<N(s); i++)
    if ((s[i] & 128) == 0) r[i]= s[i];
    else {
      char c= the_unaccented[s[i] & 127];
      if (c==' ') r[i]= s[i];
      else r[i]= the_unaccented[s[i] & 127];
    }
  return r;
}

static string
get_accents (string s) {
  int i, n= N(s);
  string r (n);
  for (i=0; i<n; i++) {
    if ((s[i] & 128) == 0) r[i]= ' ';
    else r[i]= (char) the_accents [s[i] & 127];
  }
  return r;
}

void
tex_font_rep::accented_get_extents (string s, metric& ex) {
  int old_status= status;
  status= TEX_ANY;

  register int i;
  string acc= get_accents (s);
  s= get_unaccented (s);
  get_extents (s, ex);
  
  for (i=0; i<N(acc); i++)
    if (acc[i] != ' ') {
      SI xx, yy;
      char c= acc[i];
      metric ey, ez;
      get_extents (s(0,i+1), ey); xx= ey->x2;
      get_extents (s[i], ey);
      get_extents (c, ez);
      xx -= (((ey->x2 - ey->x1) + (ez->x2 - ez->x1)) >> 1);
      yy  = ey->y2- yx;
      if (c == 24) yy=PIXEL;
      else if (c == ((char) 203)) yy= 0;
      else if (c == ((char) 206)) {
        yy= 0;
        if ((s[i] == 'a') || (s[i] == 'A')) xx += (ey->x2 - ey->x1) / 3;
        else xx += (ey->x2 - ey->x1) / 5;
      }
      else xx += (SI) (((double) yy) * slope);
      ex->x3 = min (ex->x3, xx + ez->x3);
      ex->y3 = min (ex->y3, yy + ez->y3);
      ex->x4 = max (ex->x4, xx + ez->x4);
      ex->y4 = max (ex->y4, yy + ez->y4);
    }
  
  status= old_status;
}

void
tex_font_rep::accented_get_xpositions (string s, SI* xpos, bool ligf) {
  int old_status= status;
  status= TEX_ANY;
  string acc= get_accents (s);
  s= get_unaccented (s);
  get_xpositions (s, xpos, ligf);
  status= old_status;
}

void
tex_font_rep::accented_draw (renderer ren, string s, SI x, SI y) {
  int old_status= status;
  status= TEX_ANY;

  register int i;
  string acc= get_accents (s);
  s= get_unaccented (s);
  draw_fixed (ren, s, x, y);

  for (i=0; i<N(acc); i++)
    if (acc[i] != ' ') {
      SI xx, yy;
      char c= acc[i];
      metric ey, ez;
      get_extents (s(0,i+1), ey); xx= ey->x2;
      get_extents (s[i], ey);
      get_extents (c, ez);
      xx -= (((ey->x2 - ey->x1) + (ez->x2 - ez->x1)) >> 1);
      yy  = ey->y2- yx;
      if (c == 24) yy=PIXEL;
      else if (c == ((char) 203)) yy= 0;
      else if (c == ((char) 206)) {
        yy= 0;
        if ((s[i] == 'a') || (s[i] == 'A')) xx += (ey->x2 - ey->x1) / 3;
        else xx += (ey->x2 - ey->x1) / 5;
      }
      else xx += (SI) (((double) yy) * slope);
      draw_fixed (ren, string (c), x+ xx, y+ yy);
    }
  
  status= old_status;
}

SI
tex_font_rep::accented_get_left_correction (string s) {
  s= get_unaccented (s);
  return (SI) (slope * conv (tfm->d ((QN) s[0])));
}

SI
tex_font_rep::accented_get_right_correction (string s) {
  s= get_unaccented (s);
  return conv (tfm->i ((QN) s[N(s)-1]));
}

/******************************************************************************
* The general case
******************************************************************************/

bool
tex_font_rep::raw_supports (unsigned char c) {
  glyph gl= pk->get ((int) c);
  return !is_nil (gl);
}

bool
tex_font_rep::supports (string s) {
  switch (status) {
    case TEX_ANY:
      if (s == "<less>") return raw_supports ('<');
      else if (s == "<gtr>") return raw_supports ('>');
      else if (N(s) == 1) return raw_supports (s[0]);
      else return false;
    case TEX_EC:
    case TEX_LA:
    case TEX_GR:
      return N(s) == 1 || s == "<less>" || s == "<gtr>";
    case TEX_CM:
    case TEX_ADOBE:
      if (N(s) != 1) return s == "<less>" || s == "<gtr>";
      else if (((unsigned int) s[0]) < ((unsigned int) 128)) return true;
      else {
        ACCENTS_PREPARE;
        return get_accents (s) != " ";
      }
  }
  return false;
}

void
tex_font_rep::get_extents (string s, metric& ex) {
  register int i;
  switch (status) {
    case TEX_ANY:
      break;
    case TEX_EC:
    case TEX_LA:
    case TEX_GR:
      for (i=0; i<N(s); i++)
        if (s[i]=='<') {
          special_get_extents (s, ex);
          return;
        }
      break;
    case TEX_CM:
    case TEX_ADOBE:
      for (i=0; i<N(s); i++) {
        if (s[i]=='<') {
          special_get_extents (s, ex);
          return;
        }
        if ((s[i] & 128) != 0) {
          ACCENTS_PREPARE;
          accented_get_extents (s, ex);
          return;
        }
      }
      break;
  }

  int n= N(s);
  int m= (n+16) << 1;
  STACK_NEW_ARRAY (s_copy, int, n);
  STACK_NEW_ARRAY (buf, int, m);
  STACK_NEW_ARRAY (ker, int, m);

  if (exec) {
    for (i=0; i<n; i++) s_copy[i]= ((QN) s[i]);
    tfm->execute (s_copy, n, buf, ker, m);
  }
  else {
    m = n;
    for (i=0; i<m; ++i) {
      buf[i]= s[i] & 255;
      ker[i]= 0;
    }
  }

  SI x1= 0;
  SI x2= 0;
  SI x3= PLUS_INFINITY;
  SI x4= MINUS_INFINITY;
  SI y1= PLUS_INFINITY;
  SI y2= MINUS_INFINITY;
  SI y3= PLUS_INFINITY;
  SI y4= MINUS_INFINITY;

  for (i=0; i<m; i++) {
    int c= buf[i];
    glyph gl= pk->get (c);
    if (is_nil (gl)) continue;
    
    y1= min (y1, -conv (tfm->d(c)));
    y2= max (y2,  conv (tfm->h(c)));
    x3= min (x3, x2- ((int) gl->xoff) * PIXEL);
    x4= max (x4, x2+ ((int) (gl->width- gl->xoff)) * PIXEL);
    y3= min (y3, ((int) (gl->yoff- gl->height)) * PIXEL);
    y4= max (y4, ((int) gl->yoff) * PIXEL);
    x2 += conv (tfm->w(c)+ ker[i]);
  }

  if ((x3 == PLUS_INFINITY) || (x4 == MINUS_INFINITY) ||
      (y3 == PLUS_INFINITY) || (y4 == MINUS_INFINITY))
    {
      x1= x3= x2= x4= 0;
      y3= y1= 0; y4= y2= yx;
    }

  ex->x1= x1;
  ex->x2= x2;
  ex->x3= x3 - 2*PIXEL;
  ex->x4= x4 + 2*PIXEL;
  ex->y1= y1;
  ex->y2= y2;
  ex->y3= y3 - 2*PIXEL;
  ex->y4= y4 + 2*PIXEL;

  STACK_DELETE_ARRAY (s_copy);
  STACK_DELETE_ARRAY (buf);
  STACK_DELETE_ARRAY (ker);
}

void
tex_font_rep::get_xpositions (string s, SI* xpos, bool ligf) {
  register int i, n= N(s);
  xpos[0]= 0;
  if (n == 0) return;
  
  switch (status) {
    case TEX_ANY:
      break;
    case TEX_EC:
    case TEX_LA:
    case TEX_GR:
      for (i=0; i<n; i++)
        if (s[i]=='<') {
          special_get_xpositions (s, xpos, ligf);
          return;
        }
      break;
    case TEX_CM:
    case TEX_ADOBE:
      for (i=0; i<n; i++) {
        if (s[i]=='<') {
          special_get_xpositions (s, xpos, ligf);
          return;
        }
        if ((s[i] & 128) != 0) {
          ACCENTS_PREPARE;
          accented_get_xpositions (s, xpos, ligf);
          return;
        }
      }
      break;
  }

  STACK_NEW_ARRAY (s_copy, int, n);
  for (i=0; i<n; i++) s_copy[i]= ((QN) s[i]);
  tfm->get_xpositions (s_copy, n, unit, xpos, ligf);
  STACK_DELETE_ARRAY (s_copy);
}

void
tex_font_rep::get_xpositions (string s, SI* xpos) {
  get_xpositions (s, xpos, true);
}

void
tex_font_rep::draw_fixed (renderer ren, string s, SI ox, SI y) {
  register int i;
  switch (status) {
    case TEX_ANY:
      break;
    case TEX_EC:
    case TEX_LA:
    case TEX_GR:
      for (i=0; i<N(s); i++)
        if (s[i]=='<') {
          special_draw (ren, s, ox, y);
          return;
        }
      break;
    case TEX_CM:
    case TEX_ADOBE:
      for (i=0; i<N(s); i++) {
        if (s[i]=='<') {
          special_draw (ren, s, ox, y);
          return;
        }
        if ((s[i] & 128) != 0) {
          ACCENTS_PREPARE;
          accented_draw (ren, s, ox, y);
          return;
        }
      }
      break;
  }

  SI  x= ox;
  int n= N(s);
  int m= (n+16) << 1;
  STACK_NEW_ARRAY (str, int, n);
  STACK_NEW_ARRAY (buf, int, m);
  STACK_NEW_ARRAY (ker, int, m);

  if (exec) {
    for (i=0; i<n; i++) str[i]= ((QN) s[i]);
    tfm->execute (str, n, buf, ker, m);
  }
  else {
    m = n;
    for (i=0; i<m; ++i) {
      buf[i]= s[i] & 255;
      ker[i]= 0;
    }
  }

  for (i=0; i<m; i++) {
    register int c= buf[i];
    glyph gl= pk->get (c);
    if (is_nil (gl)) continue;
    ren->draw (c, pk, x, y);
    x += conv (tfm->w(c)+ ker[i]);
  }
  STACK_DELETE_ARRAY (str);
  STACK_DELETE_ARRAY (buf);
  STACK_DELETE_ARRAY (ker);
}

font
tex_font_rep::magnify (double zoomx, double zoomy) {
  if (zoomx != zoomy) return poor_magnify (zoomx, zoomy);
  int ndpi= (int) tm_round (dpi * zoomx);
  switch (status) {
  case TEX_ANY:
    return tex_font (family, size, ndpi, dsize);
  case TEX_EC:
    return tex_ec_font (family, size, ndpi, dsize);
  case TEX_LA:
    return tex_la_font (family, size, ndpi, dsize);
  case TEX_GR:
    return tex_gr_font (family, size, ndpi, dsize);
  case TEX_CM:
    return tex_cm_font (family, size, ndpi, dsize);
  case TEX_ADOBE:
    return tex_adobe_font (family, size, ndpi, dsize);
  }
  return tex_font (family, size, ndpi, dsize);
}

SI
tex_font_rep::get_left_correction (string s) {
  if (N(s) == 0) return 0;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
  case TEX_GR:
    if (s[0] == '<') return special_get_left_correction (s);
    break;
  case TEX_CM:
  case TEX_ADOBE:
    if (s[0] == '<') return special_get_left_correction (s);
    if ((s[0] & 128) != 0) {
      ACCENTS_PREPARE;
      return accented_get_left_correction (s);
    }
  }
  return (SI) (slope * conv (tfm->d ((QN) s[0])));
}

SI
tex_font_rep::get_right_correction (string s) {
  if (N(s) == 0) return 0;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
  case TEX_GR:
    if (s[N(s)-1] == '>') return special_get_right_correction (s);
    break;
  case TEX_CM:
  case TEX_ADOBE:
    if (s[N(s)-1] == '>') return special_get_right_correction (s);
    if ((s[N(s)-1] & 128) != 0) {
      ACCENTS_PREPARE;
      return accented_get_right_correction (s);
    }
  }
  return conv (tfm->i ((QN) s[N(s)-1]));
}

SI
tex_font_rep::get_rsub_correction (string s) {
  SI r= 0;
  if (rsub_correct->contains (s)) r += (SI) (rsub_correct[s] * wfn);
  return r;
}

SI
tex_font_rep::get_rsup_correction (string s) {
  SI r= get_right_correction (s);
  if (rsup_correct->contains (s)) r += (SI) (rsup_correct[s] * wfn);
  return r;
}

SI
tex_font_rep::get_wide_correction (string s, int mode) {
  if (mode > 0 && above_correct->contains (s))
    return (SI) (above_correct[s] * wfn);
  else if (mode < 0 && below_correct->contains (s))
    return (SI) (below_correct[s] * wfn);
  else return 0;
}

void
tex_font_rep::advance_glyph (string s, int& pos) {
  if (pos >= N(s)) return;
  if (!exec || (status != TEX_ANY && s[pos] == '<'))
    tm_char_forwards (s, pos);
  else {
    int c= -1;
    int start= pos;
    while (pos < N(s)) {
      int prev= pos;
      tm_char_forwards (s, pos);
      string r= s (start, pos);

      int n= N(r);
      int m= (n+16) << 1;
      STACK_NEW_ARRAY (str, int, n);
      STACK_NEW_ARRAY (buf, int, m);
      STACK_NEW_ARRAY (ker, int, m);
      for (int i=0; i<n; i++) str[i]= ((QN) r[i]);
      tfm->execute (str, n, buf, ker, m);
      bool done= (m > 0 && buf[0] == c);
      if (!done && m > 0) c= buf[0];
      STACK_DELETE_ARRAY (str);
      STACK_DELETE_ARRAY (buf);
      STACK_DELETE_ARRAY (ker);

      if (done) {
        pos= prev;
        break;
      }
    }
  }
}

glyph
tex_font_rep::get_glyph (string s) {
  register int i;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
  case TEX_GR:
    if (s == "<less>") s= "<";
    else if (s == "<gtr>") s= ">";
    else if (N(s) >= 2 && s[0] == '<') s= special_translate[s];
    break;
  case TEX_CM:
  case TEX_ADOBE:
    if (s == "<less>") s= "<";
    if (s == "<gtr>") s= ">";
    for (i=0; i<N(s); i++)
      if ((s[i] & 128) != 0)
	return font_rep::get_glyph (s);
    break;
  }
  int c;
  if (N(s) != 1) c= get_ligature_code (s);
  else c= ((QN) s[0]);
  if (c == -1) return font_rep::get_glyph (s);
  glyph gl= pk->get (c);
  if (is_nil (gl)) return font_rep::get_glyph (s);
  return gl;
}

int
tex_font_rep::index_glyph (string s, font_metric& rm, font_glyphs& rg) {
  register int i;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
  case TEX_GR:
    if (s == "<less>") s= "<";
    else if (s == "<gtr>") s= ">";
    else if (N(s) >= 2 && s[0] == '<') s= special_translate[s];
    break;
  case TEX_CM:
  case TEX_ADOBE:
    if (s == "<less>") s= "<";
    if (s == "<gtr>") s= ">";
    for (i=0; i<N(s); i++)
      if ((s[i] & 128) != 0)
	return font_rep::index_glyph (s, rm, rg);
    break;
  }
  int c;
  if (N(s) != 1) c= get_ligature_code (s);
  else c= ((QN) s[0]);
  if (c == -1) return font_rep::index_glyph (s, rm, rg);
  glyph gl= pk->get (c);
  if (is_nil (gl)) return font_rep::index_glyph (s, rm, rg);
  rm= tfm_font_metric (tfm, pk, unit);
  rg= pk;
  return c;
}

int
tex_font_rep::get_ligature_code (string s) {
  int n= N(s);
  int m= (n+16) << 1;
  STACK_NEW_ARRAY (str, int, n);
  STACK_NEW_ARRAY (buf, int, m);
  STACK_NEW_ARRAY (ker, int, m);
  for (int i=0; i<n; i++) str[i]= ((QN) s[i]);
  tfm->execute (str, n, buf, ker, m);
  STACK_DELETE_ARRAY (str);
  STACK_DELETE_ARRAY (buf);
  STACK_DELETE_ARRAY (ker);
  if (m == 1) return buf[0];
  else return -1;
}

/******************************************************************************
* TeX font metrics as usual metrics
******************************************************************************/

extern metric error_metric;

struct tfm_font_metric_rep: public font_metric_rep {
  tex_font_metric tfm;
  font_glyphs pk;
  double unit;
  hashmap<int,pointer> ms;
  tfm_font_metric_rep (string name, tex_font_metric tfm2,
                       font_glyphs pk2, double unit2):
    font_metric_rep (name), tfm (tfm2), pk (pk2),
    unit (unit2), ms (error_metric) {}
  bool exists (int c) {
    return c >= ((int) tfm->bc) && ((int) tfm->ec) >= c; }
  metric& get (int c) {
    if (!exists (c)) return error_metric;
    if (!ms->contains (c)) {
      metric_struct* r= tm_new<metric_struct> ();
      ms(c)= (pointer) r;
      r->x1=  0;
      r->x2=  conv (tfm->w(c));
      r->y1= -conv (tfm->d(c));
      r->y2=  conv (tfm->h(c));
      glyph gl= pk->get (c);
      r->x3= -((int) gl->xoff) * PIXEL;
      r->x4=  ((int) (gl->width- gl->xoff)) * PIXEL;
      r->y3=  ((int) (gl->yoff- gl->height)) * PIXEL;
      r->y4=  ((int) gl->yoff) * PIXEL;
    }
    return *((metric*) ((void*) ms[c])); }
};

#undef conv

font_metric
tfm_font_metric (tex_font_metric tfm, font_glyphs pk, double unit) {
  string name= tfm->res_name * ":" * pk->res_name * ":" * as_string (unit);
  return make (font_metric, name,
               tm_new<tfm_font_metric_rep> (name, tfm, pk, unit));
}

/******************************************************************************
* Interface
******************************************************************************/

font
tex_font (string family, int size, int dpi, int dsize) {
  string name= "tex:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_font_rep> (name, TEX_ANY, family, size, dpi, dsize));
}

font
tex_cm_font (string family, int size, int dpi, int dsize) {
  string name= "cm:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_font_rep> (name, TEX_CM, family, size, dpi, dsize));
}

font
tex_ec_font (string family, int size, int dpi, int dsize) {
  string name= "ec:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_font_rep> (name, TEX_EC, family, size, dpi, dsize));
}

font
tex_la_font (string family, int size, int dpi, int dsize) {
  string name= "la:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_font_rep> (name, TEX_LA, family, size, dpi, dsize));
}

font
tex_gr_font (string family, int size, int dpi, int dsize) {
  string name= "gr:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_font_rep> (name, TEX_GR, family, size, dpi, dsize));
}

font
tex_adobe_font (string family, int size, int dpi, int dsize) {
  string name= "adobe:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_font_rep> (name, TEX_ADOBE, family, size, dpi, dsize));
}
