
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
#define TEX_CM    3
#define TEX_ADOBE 4

static void special_initialize ();

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

  tex_font_rep (string name, int status,
		string family, int size, int dpi, int dsize);

  void get_extents (string s, metric& ex);
  void get_xpositions (string s, SI* xpos);
  void draw (renderer ren, string s, SI x, SI y);
  SI   get_left_correction (string s);
  SI   get_right_correction (string s);
  glyph get_glyph (string s);
  void special_get_extents (string s, metric& ex);
  void special_get_xpositions (string s, SI* xpos);
  void special_draw (renderer ren, string s, SI x, SI y);
  SI   special_get_left_correction (string s);
  SI   special_get_right_correction (string s);
  void accented_get_extents (string s, metric& ex);
  void accented_get_xpositions (string s, SI* xpos);
  void accented_draw (renderer ren, string s, SI x, SI y);
  SI   accented_get_left_correction (string s);
  SI   accented_get_right_correction (string s);
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
}

/******************************************************************************
* Handle <, > and (in the future?) other special characters
******************************************************************************/

static bool special_initialized= false;
static hashmap<string,string> special_translate ("");

static void
special_initialize () {
  if (special_initialized) return;
  special_translate ("<less>")= "<";
  special_translate ("<gtr>")= ">";
  translator trl= load_translator ("larm");
  iterator<string> it= iterate (trl->dict);
  while (it->busy ()) {
    string s= it->next ();
    special_translate (s)= string ((char) (unsigned char) trl->dict[s]);
  }
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
tex_font_rep::special_get_xpositions (string s, SI* xpos) {
  SI offset= 0;
  register int l=0, i, j, n=N(s);
  while (l<n) {
    for (i=l; i<n; i++)
      if (s[i]=='<') break;
    if (l<i) {
      get_xpositions (s (l, i), xpos + l);
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
  draw (ren, s (0, i), x, y);
  get_extents (s (0, i), ex);
  x += ex->x2;
  for (j=i+1; j<N(s); j++)
    if (s[j]=='>') break;
  if (j<N(s)) j++;

  int temp= status;
  status= TEX_ANY;
  string r= s (i, j);
  string rr= special_translate[r];
  color c= ren->get_color ();
  if (N(rr) != 0) r= rr;
  else ren->set_color (red);
  draw (ren, r, x, y);
  ren->set_color (c);
  get_extents (r, ex);
  x += ex->x2;
  status= temp;
  
  draw (ren, s (j, N(s)), x, y);
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
  'A', ' ', 'C', 'C', 'D', 'E', ' ', 'G',
  'L', 'L', ' ', 'N', 'N', ' ', 'O', 'R',
  'R', 'S', 'S', 'S', 'T', 'T', 'U', 'U',
  'Y', 'Z', 'Z', 'Z', ' ', 'I', 'd', ' ',
  'a', ' ', 'c', 'c', 'd', 'e', ' ', 'g',
  'l', 'l', ' ', 'n', 'n', ' ', 'o', 'r',
  'r', 's', 's', 's', 't', 't', 'u', 'u',
  'y', 'z', 'z', 'z', ' ', 60 , 62 , ' ',
  'A', 'A', 'A', 'A', 'A', 'A', 29 , 'C',
  'E', 'E', 'E', 'E', 'I', 'I', 'I', 'I',
  'D', 'N', 'O', 'O', 'O', 'O', 'O', 30 ,
  31 , 'U', 'U', 'U', 'U', 'Y', ' ', ' ',
  'a', 'a', 'a', 'a', 'a', 'a', 26 , 'c',
  'e', 'e', 'e', 'e', 16 , 16 , 16 , 16 ,
  'd', 'n', 'o', 'o', 'o', 'o', 'o', 27 ,
  28 , 'u', 'u', 'u', 'u', 'y', ' ', 25
};

static char CM_accents[128]= {
  21 , ' ', 19 , 20 , 20 , 20 , ' ', 21 ,
  19 , 39 , ' ', 19 , 20 , ' ', 125, 19 ,
  20 , 19 , 20 , 24 , 20 , 24 , 125, 23 ,
  127, 19 , 20 , 95 , ' ', 95 , 22 , ' ',
  21 , ' ', 19 , 20 , 20 , 20 , ' ', 21 ,
  19 , 39 , ' ', 19 , 20 , ' ', 125, 19 ,
  20 , 19 , 20 , 24 , 20 , 24 , 125, 23 ,
  127, 19 , 20 , 95 , ' ', ' ', ' ', ' ',
  18 , 19 , 94 , 126, 127, 23 , ' ', 24 ,
  18 , 19 , 94 , 127, 18 , 19 , 94 , 127,
  22 , 126, 18 , 19 , 94 , 126, 127, ' ',
  ' ', 18 , 19 , 94 , 127, 19 , ' ', ' ',
  18 , 19 , 94 , 126, 127, 23 , ' ', 24 ,
  18 , 19 , 94 , 127, 18 , 19 , 94 , 127,
  22 , 126, 18 , 19 , 94 , 126, 127, ' ',
  ' ', 18 , 19 , 94 , 127, 19 , ' ', ' '
};

static char ADOBE_unaccented[128]= {
  'A', 'A', 'C', 'C', 'D', 'E', 'E', 'G',
  'L', 'L', 232, 'N', 'N', ' ', 'O', 'R',
  'R', 'S', 'S', 'S', 'T', 'T', 'U', 'U',
  'Y', 'Z', 'Z', 'Z', ' ', 'I', 'd', 167,
  'a', 'a', 'c', 'c', 'd', 'e', 'e', 'g',
  'l', 'l', 248, 'n', 'n', ' ', 'o', 'r',
  'r', 's', 's', 's', 't', 't', 'u', 'u',
  'y', 'z', 'z', 'z', ' ', 161, 191, 163,
  'A', 'A', 'A', 'A', 'A', 'A', 225, 'C',
  'E', 'E', 'E', 'E', 'I', 'I', 'I', 'I',
  'D', 'N', 'O', 'O', 'O', 'O', 'O', 30 ,
  233, 'U', 'U', 'U', 'U', 'Y', ' ', ' ',
  'a', 'a', 'a', 'a', 'a', 'a', 241, 'c',
  'e', 'e', 'e', 'e', 245, 245, 245, 245,
  'd', 'n', 'o', 'o', 'o', 'o', 'o', 250,
  249, 'u', 'u', 'u', 'u', 'y', ' ', 251
};

static char ADOBE_accents[128]= {
  198, 206, 194, 207, 207, 207, 206, 198,
  194, 39 , ' ', 194, 207, ' ', 205, 194,
  207, 194, 207, 203, 207, 203, 205, 202,
  200, 194, 207, 199, ' ', 199, 197, ' ',
  198, 206, 194, 207, 207, 207, 206, 198,
  194, 39 , ' ', 194, 207, ' ', 205, 194,
  207, 194, 207, 203, 207, 203, 205, 202,
  200, 194, 207, 199, ' ', ' ', ' ', ' ',
  193, 194, 195, 196, 200, 202, ' ', 203,
  193, 194, 195, 200, 193, 194, 195, 200,
  197, 196, 193, 194, 195, 196, 200, ' ',
  ' ', 193, 194, 195, 200, 194, ' ', ' ',
  193, 194, 195, 196, 200, 202, ' ', 203,
  193, 194, 195, 200, 193, 194, 195, 200,
  197, 196, 193, 194, 195, 196, 200, ' ',
  ' ', 193, 194, 195, 200, 194, ' ', ' '
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
  int i;
  string r(N(s));
  for (i=0; i<N(s); i++)
    if ((s[i] & 128) == 0) r[i]= ' ';
    else r[i]= (char) the_accents [s[i] & 127];
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
tex_font_rep::accented_get_xpositions (string s, SI* xpos) {
  int old_status= status;
  status= TEX_ANY;
  string acc= get_accents (s);
  s= get_unaccented (s);
  get_xpositions (s, xpos);
  status= old_status;
}

void
tex_font_rep::accented_draw (renderer ren, string s, SI x, SI y) {
  int old_status= status;
  status= TEX_ANY;

  register int i;
  string acc= get_accents (s);
  s= get_unaccented (s);
  draw (ren, s, x, y);

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
      draw (ren, string (c), x+ xx, y+ yy);
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

void
tex_font_rep::get_extents (string s, metric& ex) {
  register int i;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
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
  for (i=0; i<n; i++) s_copy[i]= ((QN) s[i]);
  tfm->execute (s_copy, n, buf, ker, m);

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
tex_font_rep::get_xpositions (string s, SI* xpos) {
  register int i, n= N(s);
  if (n == 0) return;

  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
    for (i=0; i<n; i++)
      if (s[i]=='<') {
	special_get_xpositions (s, xpos);
	return;
      }
    break;
  case TEX_CM:
  case TEX_ADOBE:
    for (i=0; i<n; i++) {
      if (s[i]=='<') {
	special_get_xpositions (s, xpos);
	return;
      }
      if ((s[i] & 128) != 0) {
	ACCENTS_PREPARE;
	accented_get_xpositions (s, xpos);
	return;
      }
    }
    break;
  }

  STACK_NEW_ARRAY (s_copy, int, n);
  for (i=0; i<n; i++) s_copy[i]= ((QN) s[i]);
  tfm->get_xpositions (s_copy, n, unit, xpos);
  STACK_DELETE_ARRAY (s_copy);
}

void
tex_font_rep::draw (renderer ren, string s, SI ox, SI y) {
  register int i;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
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
  for (i=0; i<n; i++) str[i]= ((QN) s[i]);
  tfm->execute (str, n, buf, ker, m);

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

SI
tex_font_rep::get_left_correction (string s) {
  if (N(s) == 0) return 0;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
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

glyph
tex_font_rep::get_glyph (string s) {
  register int i;
  switch (status) {
  case TEX_ANY:
    break;
  case TEX_EC:
  case TEX_LA:
    if (s == "<less>") s= "<";
    if (s == "<gtr>") s= ">";
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
  if (N(s)!=1) return font_rep::get_glyph (s);
  int c= ((QN) s[0]);
  glyph gl= pk->get (c);
  if (is_nil (gl)) return font_rep::get_glyph (s);
  return gl;
}

#undef conv

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
tex_adobe_font (string family, int size, int dpi, int dsize) {
  string name= "adobe:" * family * as_string (size) * "@" * as_string(dpi);
  return make (font, name,
    tm_new<tex_font_rep> (name, TEX_ADOBE, family, size, dpi, dsize));
}
