
/******************************************************************************
* MODULE     : virtual_font.cpp
* DESCRIPTION: fonts consisting of extra symbols which can be generated
*              automatically from a defining tree
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "font.hpp"
#include "translator.hpp"
#include "analyze.hpp"

/******************************************************************************
* The virtual font class
******************************************************************************/

struct virtual_font_rep: font_rep {
  font           base_fn;
  translator     virt;
  int            size, dpi;
  int            last;
  bitmap_metric  bmm;
  bitmap_font    bmf;
  double         unit;

  virtual_font_rep (string name, font base, string vname, int size, int dpi);
  bitmap_char compile (scheme_tree t, text_extents& ex);
  int         get_char (string s, bitmap_metric& bmm, bitmap_font& bmf);
  bitmap_char get_bitmap (string s);

  void get_extents (string s, text_extents& ex);
  void draw (ps_device dev, string s, SI x, SI y);
};

virtual_font_rep::virtual_font_rep (
  string name, font base, string vname, int size2, int dpi2):
    font_rep (base->dis, name, base), base_fn (base),
    virt (load_translator (vname)), size (size2), dpi (dpi2),
    last (N(virt->virt_def)),
    bmm (std_bitmap_metric (name, new text_extents [last], 0, last-1)),
    bmf (std_bitmap_font (name, new bitmap_char  [last], 0, last-1))
{
  copy_math_pars (base_fn);
  unit= ((size*dpi)/72)*PIXEL;
}

/******************************************************************************
* Compilation of virtual characters
******************************************************************************/

static void
outer_fit (text_extents& ex, text_extents& ey, SI x, SI y) {
  ex->x1= min (ex->x1, x+ ey->x1);
  ex->y1= min (ex->y1, y+ ey->y1);
  ex->x2= max (ex->x2, x+ ey->x2);
  ex->y2= max (ex->y2, y+ ey->y2);
  ex->x3= min (ex->x3, x+ ey->x3);
  ex->y3= min (ex->y3, y+ ey->y3);
  ex->x4= max (ex->x4, x+ ey->x4);
  ex->y4= max (ex->y4, y+ ey->y4);
}

bitmap_char
virtual_font_rep::compile (scheme_tree t, text_extents& ex) {
  // cout << "Compile " << t << "\n";

  if (is_atomic (t)) {
    string r= t->label;
    if (N(r)>1) r= "<" * r * ">";
    base_fn->get_extents (r, ex);
    return base_fn->get_bitmap (r);
  }

  if (is_func (t, TUPLE, 3) &&
      (is_double (t[0])) && (is_double (t[1])))
    {
      SI x= (SI) (as_double (t[0]) * unit);
      SI y= (SI) (as_double (t[1]) * unit);
      bitmap_char bmc= compile (t[2], ex);
      ex->x1 += x; ex->y1 += y;
      ex->x2 += x; ex->y2 += y;
      ex->x3 += x - PIXEL; ex->y3 += y + PIXEL;
      ex->x4 += x - PIXEL; ex->y4 += y + PIXEL;
      return move (bmc, x, y);
    }

  if (is_tuple (t, "join")) {
    int i, n= N(t);
    bitmap_char bmc1= compile (t[1], ex);
    for (i=2; i<n; i++) {
      text_extents ey;
      bitmap_char bmc2= compile (t[i], ey);
      outer_fit (ex, ey, 0, 0);
      bmc1= join (bmc1, bmc2);
    }
    return bmc1;
  }

  if (is_tuple (t, "glue", 2)) {
    text_extents ey;
    bitmap_char bmc1= compile (t[1], ex);
    bitmap_char bmc2= compile (t[2], ey);
    SI dx= ex->x2- ((base_fn->wpt*28)>>4);
    outer_fit (ex, ey, dx, 0);
    return join (bmc1, move (bmc2, dx, 0));
  }

  if (is_tuple (t, "glue*", 2)) {
    text_extents ey;
    bitmap_char bmc1= compile (t[1], ex);
    bitmap_char bmc2= compile (t[2], ey);
    SI dx= ex->x2;
    outer_fit (ex, ey, dx, 0);
    return join (bmc1, move (bmc2, dx, 0));
  }

  if (is_tuple (t, "add", 2)) {
    text_extents ey;
    bitmap_char bmc1= compile (t[1], ex);
    bitmap_char bmc2= compile (t[2], ey);
    SI dx= ((ex->x1+ ex->x2- ey->x1- ey->x2) >> 1);
    outer_fit (ex, ey, dx, 0);
    return join (bmc1, move (bmc2, dx, 0));
  }

  if (is_tuple (t, "enlarge")) {
    bitmap_char bmc= compile (t[1], ex);
    if (N(t)>2) ex->x1 -= (SI) (as_double (t[2]) * unit);
    if (N(t)>3) ex->x2 += (SI) (as_double (t[3]) * unit);
    if (N(t)>4) ex->y1 -= (SI) (as_double (t[4]) * unit);
    if (N(t)>5) ex->y2 += (SI) (as_double (t[5]) * unit);
    return bmc;
  }

  if (is_tuple (t, "hor-flip", 1))
    return hor_flip (compile (t[1], ex));

  if (is_tuple (t, "ver-flip", 1))
    return ver_flip (compile (t[1], ex));

  if (is_tuple (t, "rot-left", 1)) {
    text_extents ey;
    bitmap_char bmc= pos_rotate (compile (t[1], ey));
    ex->x1= 0;
    ex->y1= 0;
    ex->x2= ey->y2- ey->y1;
    ex->y2= ey->x2- ey->x1;
    ex->x3= ey->y2- ey->y4;
    ex->y3= ey->x3- ey->x1;
    ex->x4= ey->y2- ey->y3;
    ex->y4= ey->x4- ey->x1;
    return move (bmc, ey->y2, -ey->x1);
  }

  if (is_tuple (t, "rot-right", 1)) {
    text_extents ey;
    bitmap_char bmc= pos_rotate (pos_rotate (pos_rotate (compile (t[1], ey))));
    ex->x1= 0;
    ex->y1= 0;
    ex->x2= ey->y2- ey->y1;
    ex->y2= ey->x2- ex->x1;
    ex->x3= ey->y3- ey->y1;
    ex->y3= ey->x2- ey->x4;
    ex->x4= ey->y4- ey->y1;
    ex->y4= ey->x2- ey->x3;
    return move (bmc, -ey->y1, ey->x2);
  }

  if (is_tuple (t, "hor-extend", 3) || is_tuple (t, "hor-extend", 4)) {
    bitmap_char bmc= compile (t[1], ex);
    int pos= (int) (as_double (t[2]) * bmc->width);
    SI  add= (SI)  (as_double (t[3]) * unit);
    if (is_tuple (t, "hor-extend", 4))
      add= (SI)  (as_double (t[3]) * as_double (t[4]) * unit);
    int by = add / PIXEL;
    if (pos < 0) pos= 0;
    if (pos >= bmc->width) pos= bmc->width-1;
    ex->x2 += add;
    ex->x4 += by * PIXEL;
    return hor_extend (bmc, pos, by);
  }

  if (is_tuple (t, "ver-extend", 3) || is_tuple (t, "ver-extend", 4)) {
    bitmap_char bmc= compile (t[1], ex);
    int pos= (int) (as_double (t[2]) * bmc->height);
    SI  add= (SI)  (as_double (t[3]) * unit);
    if (is_tuple (t, "ver-extend", 4))
      add= (SI)  (as_double (t[3]) * as_double (t[4]) * unit);
    int by = add / PIXEL;
    if (pos < 0) pos= 0;
    if (pos >= bmc->height) pos= bmc->height-1;
    ex->y1 -= add;
    ex->y3 -= by * PIXEL;
    return ver_extend (bmc, pos, by);
  }

  cerr << "TeXmacs] The defining tree is " << t << "\n";
  fatal_error ("Invalid virtual character", "virtual_font_rep::compile");
  return bitmap_char(); // avoids error message when C++ compiler behaves badly
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

static tree
subst_sharp (tree t, string by) {
  if (is_atomic (t)) {
    int i;
    string s= t->label;
    i= search_forwards ("#", s);
    if (i == -1) return s;
    else return s(0,i) * by * s(i+1,N(s));
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= subst_sharp (t[i], by);
    return r;
  }
}

static void
make_char_font (string name, bitmap_metric& cbmm, bitmap_font& cbmf) {
  cbmm= std_bitmap_metric (name, new text_extents [1], 0, 0);
  cbmf= std_bitmap_font (name, new bitmap_char [1], 0, 0);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

int
virtual_font_rep::get_char (string s, bitmap_metric& cbmm, bitmap_font& cbmf) {
  int c= ((N(s)==0)? -1: ((QN) s[0]));
  if ((c<0) || (c>=last)) return -1;
  if (N(s)==1) {
    cbmm= bmm;
    cbmf= bmf;
    if (nil (bmf->get(c)))
      bmf->get(c)= compile (virt->virt_def[c], bmm->get(c));
    return c;
  }
  else {
    make_char_font (res_name * s, cbmm, cbmf);
    tree t= subst_sharp (virt->virt_def[c], s(1,N(s)));
    if (nil (cbmf->get(0)))
      cbmf->get(0)= compile (t, cbmm->get(0));
    return 0;
  }
}

void
virtual_font_rep::get_extents (string s, text_extents& ex) {
  bitmap_metric cbmm;
  bitmap_font cbmf;
  int c= get_char (s, cbmm, cbmf);
  if (c == -1) {
    ex->y1= y1; ex->y2= y2;
    ex->x1= ex->x2= ex->x3= ex->x4= ex->y3= ex->y4= 0;
  }
  else {
    text_extents_struct* ey= cbmm->get(c);
    ex->x1= ey->x1; ex->y1= ey->y1;
    ex->x2= ey->x2; ex->y2= ey->y2;
    ex->x3= ey->x3; ex->y3= ey->y3;
    ex->x4= ey->x4; ex->y4= ey->y4;
  }
}

void
virtual_font_rep::draw (ps_device dev, string s, SI x, SI y) {
  bitmap_metric cbmm;
  bitmap_font cbmf;
  int c= get_char (s, cbmm, cbmf);
  if (c != -1) dev->draw (c, cbmf, x, y);
}

bitmap_char
virtual_font_rep::get_bitmap (string s) {
  bitmap_metric cbmm;
  bitmap_font cbmf;
  int c= get_char (s, cbmm, cbmf);
  if (c == -1) return font_rep::get_bitmap (s);
  else return cbmf->get(c);
}

/******************************************************************************
* User interface
******************************************************************************/

font
virtual_font (font base, string name, int size, int dpi) {
  string full_name=
    base->res_name * "#virtual-" *
    name * as_string (size) * "@" * as_string (dpi);
  return make (font, full_name,
    new virtual_font_rep (full_name, base, name, size, dpi));
}
