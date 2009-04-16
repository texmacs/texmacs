
/******************************************************************************
* MODULE     : text.cpp
* DESCRIPTION: text boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "font.hpp"
#include "Boxes/construct.hpp"
#include "analyze.hpp"

/******************************************************************************
* Text boxes
******************************************************************************/

struct text_box_rep: public box_rep {
  int       pos;
  string    str;
  font      fn;
  color     col;

  text_box_rep (path ip, int pos, string s, font fn, color col);
  operator tree () { return str; }

  void      display (renderer ren);
  double    left_slope ();
  double    right_slope ();
  SI        left_correction ();
  SI        right_correction ();
  SI        lsub_correction ();
  SI        lsup_correction ();
  SI        rsub_correction ();
  SI        rsup_correction ();
  SI        sub_lo_base (int level);
  SI        sub_hi_lim  (int level);
  SI        sup_lo_lim  (int level);
  SI        sup_lo_base (int level);
  SI        sup_hi_lim  (int level);

  path      find_box_path (SI x, SI y, SI delta, bool force);
  path      find_lip ();
  path      find_rip ();
  path      find_right_box_path ();
  path      find_box_path (path p, bool& found);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);

  int       get_leaf_left_pos ();
  int       get_leaf_right_pos ();
  string    get_leaf_string ();
  font      get_leaf_font ();
  color     get_leaf_color ();
  SI        get_leaf_offset (string search);
};

/******************************************************************************
* Routines for text boxes
******************************************************************************/

text_box_rep::text_box_rep (path ip, int pos2, string s, font fn2, color col2):
  box_rep (ip), pos (pos2), str (s), fn (fn2), col (col2)
{
  metric ex;
  fn->get_extents (str, ex);
  x1= ex->x1; y1= ex->y1;
  x2= ex->x2; y2= ex->y2;
  x3= ex->x3; y3= ex->y3;
  x4= ex->x4; y4= ex->y4;
}

void
text_box_rep::display (renderer ren) {
  ren->set_color (col);
  fn->draw (ren, str, 0, 0);
}

double text_box_rep::left_slope () {
  return fn->get_left_slope (str); }
double text_box_rep::right_slope () {
  return fn->get_right_slope (str); }
SI text_box_rep::left_correction () {
  return fn->get_left_correction (str); }
SI text_box_rep::right_correction () {
  return fn->get_right_correction (str); }
SI text_box_rep::lsub_correction () {
  return -left_correction (); }
SI text_box_rep::lsup_correction () {
  return right_correction (); }
SI text_box_rep::rsub_correction () {
  return 0; }
SI text_box_rep::rsup_correction () {
  return right_correction (); }

SI
text_box_rep::sub_lo_base (int level) {
  if (level <= 0) return fn->ysub_lo_base;
  else return fn->ysub_lo_base + fn->yshift;
}

SI
text_box_rep::sub_hi_lim  (int level) {
  (void) level;
  return fn->ysub_hi_lim;
}

SI
text_box_rep::sup_lo_lim  (int level) {
  (void) level;
  return fn->ysup_lo_lim;
}

SI
text_box_rep::sup_lo_base (int level) {
  if (level >= 0) return fn->ysup_lo_base;
  else return fn->ysup_lo_base - fn->yshift;
}

SI
text_box_rep::sup_hi_lim  (int level) {
  (void) level;
  return fn->ysup_hi_lim;
}

/******************************************************************************
* New routines concerning the cursor
******************************************************************************/

path
text_box_rep::find_box_path (SI x, SI y, SI delta, bool force) {
  (void) y;
  (void) force;
  STACK_NEW_ARRAY (xpos, SI, N(str)+1);
  fn->get_xpositions (str, xpos);

  int prev_i, prev_x=0, i=0;
  while (i<N(str)) {
    prev_i= i;
    if (str[i]=='<')
      while ((i<N(str)) && (str[i]!='>')) i++;
    i++;

    int m= (prev_x + xpos[i]) >> 1;
    if ((x<m) || ((x==m) && (delta<0))) {
      STACK_DELETE_ARRAY (xpos);
      return path (prev_i);
    }
    prev_x= xpos[i];
  }
  STACK_DELETE_ARRAY (xpos);
  return path (i);
}

path
text_box_rep::find_lip () {
  if (is_accessible (ip)) return descend (ip, pos);
  return ip;
}

path
text_box_rep::find_rip () {
  if (is_accessible (ip)) return descend (ip, pos+ N(str));
  else return ip;
}

path
text_box_rep::find_right_box_path () {
  return path (N(str));
  /*
  if (is_accessible (ip)) return path (N(str));
  else return path (1);
  */
}

path
text_box_rep::find_box_path (path p, bool& found) {
  // cout << "Find box path " << box (this) << ", " << p
  //      << "; " << reverse (ip)
  //      << ", " << reverse (find_lip ())
  //      << " -- " << reverse (find_rip ()) << "\n";
  found= (!is_nil(p)) && is_accessible (ip);
  if (found) {
    int i= last_item (p) - pos;
    if (i < 0) return path (0);
    else if (i > N(str)) return N(str);
    else return path (i);
  }
  else return path (0);
}

path
text_box_rep::find_tree_path (path bp) {
  if (is_accessible (ip)) return reverse (descend (ip, pos+ bp->item));
  else return reverse (descend_decode (ip, bp->item <= N(str) ? 0 : 1));
}

cursor
text_box_rep::find_cursor (path bp) {
  metric ex;
  cursor cu (0, 0);
  int l= min (bp->item, N(str));
  fn->get_extents (str (0, l), ex);
  cu->ox= ex->x2;
  if (l != 0) {
    int k= l;
    tm_char_backwards (str, k);
    fn->get_extents (str (k, l), ex);
  }
  cu->y1= min (ex->y1, 0);
  cu->y2= max (ex->y2, fn->yx);
  cu->slope= fn->get_right_slope (str);
  return cu;
}

selection
text_box_rep::find_selection (path lbp, path rbp) {
  SI x1, y1, x2, y2;
  metric ex;
  fn->get_extents (str (0, lbp->item), ex);
  x1= ex->x2;
  fn->get_extents (str (0, rbp->item), ex);
  x2= ex->x2;
  fn->get_extents (str (lbp->item, rbp->item), ex);
  y1= ex->y1;
  y2= ex->y2;
  return selection (rectangle (x1, y1, x2, y2),
		    find_tree_path (lbp), find_tree_path (rbp));
}

/******************************************************************************
* Getting information about box
******************************************************************************/

int
text_box_rep::get_leaf_left_pos () {
  return pos;
}

int
text_box_rep::get_leaf_right_pos () {
  return pos+ N(str);
}

string
text_box_rep::get_leaf_string () {
  return str;
}

font
text_box_rep::get_leaf_font () {
  return fn;
}

color
text_box_rep::get_leaf_color () {
  return col;
}

SI
text_box_rep::get_leaf_offset (string search) {
  int pos= search_forwards (search, 0, str);
  if (pos == -1) return w();
  metric ex;
  fn->get_extents (str (0, pos), ex);
  return ex->x2- ex->x1;
}

/******************************************************************************
* Computing right size for rubber characters
******************************************************************************/

static string
get_delimiter (string s, font fn, SI height) {
  ASSERT (N(s) >= 2 && s[0] == '<' && s[N(s)-1] == '>',
	  "invalid rubber character");
  height -= PIXEL;
  string radical= s (0, N(s)-1) * "-";
  string first  = radical * "0>";
  metric ex;
  fn->get_extents (first, ex);
  if ((ex->y2- ex->y1) >= height) return first;

  string second  = radical * "1>";
  fn->get_extents (second, ex);
  SI h1= ex->y2- ex->y1;
  if (h1 >= (height-PIXEL)) return second;

  string third = radical * "2>";
  metric ey;
  fn->get_extents (third, ey);
  SI h2= ey->y2- ey->y1;
  if (h2 <= h1) return second;
  SI  d= h2- h1;
  int n= (height + (d-1) - h1) / d;

  while (true) {
    string test= radical * as_string (n+1) * ">";
    fn->get_extents (test, ey);
    if (ey->y2- ey->y1 >= height) return test;
    if ((ey->y2- ey->y1 <= h2) && (n>1)) return radical * as_string (n) * ">";
    h2= ey->y2- ey->y1;
    n++;
  }
}

static string
get_wide (string s, font fn, SI width) {
  ASSERT (N(s) >= 2 && s[0] == '<' && s[N(s)-1] == '>',
	  "invalid rubber character");
  string radical= s (0, N(s)-1) * "-";
  string first  = radical * "0>";
  metric ex;
  fn->get_extents (first, ex);
  if ((ex->x2- ex->x1) >= width) return first;

  string second = radical * "1>";
  metric ey;
  fn->get_extents (second, ey);
  SI w1= ex->x2- ex->x1;
  SI w2= ey->x2- ey->x1;
  if ((w2 <= w1) || (w2 > width)) return first;
  SI  d= w2- w1;
  int n= (width-w1) / (d+1);

  while (true) {
    string test= radical * as_string (n+1) * ">";
    fn->get_extents (test, ey);
    if (ey->x2- ey->x1 > width) return radical * as_string (n) * ">";
    n++;
  }
}

/******************************************************************************
* Exported routines
******************************************************************************/

box
delimiter_box (path ip, string s, font fn, color col, SI bot, SI top) {
  string r= get_delimiter (s, fn, top-bot);
  metric ex;
  fn->get_extents (r, ex);
  SI x= -ex->x1;
  SI y= (top+ bot- ex->y1- ex->y2) >> 1;
  box mvb= move_box (ip, text_box (ip, 0, r, fn, col), x, y, false, true);
  return macro_box (ip, mvb, fn);
}

box
big_operator_box (path ip, string s, font fn, color col, int n) {
  ASSERT (N(s) >= 2 && s[0] == '<' && s[N(s)-1] == '>',
	  "invalid rubber character");
  string r= s (0, N(s)-1) * "-" * as_string (n) * ">";
  metric ex;
  fn->get_extents (r, ex);
  SI y= fn->yfrac - ((ex->y1 + ex->y2) >> 1);
  box mvb= move_box (ip, text_box (ip, 0, r, fn, col), 0, y, false, true);
  return macro_box (ip, mvb, fn);
}

box
wide_box (path ip, string s, font fn, color col, SI width) {
  string r= get_wide (s, fn, width);
  metric ex;
  fn->get_extents (r, ex);
  box b= text_box (ip, 0, r, fn, col);
  return macro_box (ip, b, fn);
}

box
text_box (path ip, int pos, string s, font fn, color col) {
  return tm_new<text_box_rep> (ip, pos, s, fn, col);
}
