
/******************************************************************************
* MODULE     : cursor.cpp
* DESCRIPTION: cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_cursor.hpp"
#include "iterator.hpp"
#include "tm_buffer.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

edit_cursor_rep::edit_cursor_rep ():
  cu (0, 0), mv (0, 0), mv_status (0) {}
edit_cursor_rep::~edit_cursor_rep () {}
cursor& edit_cursor_rep::the_cursor () { return cu; }
cursor& edit_cursor_rep::the_ghost_cursor () { return mv; }

/******************************************************************************
* Cursor movement
******************************************************************************/

#define DELTA (1<<23)

path
edit_cursor_rep::tree_path (SI x, SI y, SI delta) {
  return correct_cursor (et, eb->find_tree_path (x, y, delta));
}

bool
edit_cursor_rep::cursor_move_sub (SI& x0, SI& y0, SI& d0, SI dx, SI dy) {
  int i,d;
  path ref_p= tree_path (x0, y0, d0);
  if (ref_p != tp) {
    tp= ref_p;
    return true;
  }
  
  // cout << "ref_p = " << ref_p << "\n";
  if (ref_p == tree_path (x0, y0, d0+ dx*DELTA)) {
    for (i=1; i<DELTA; i=i<<1)
      if (ref_p != tree_path (x0+ dx*i, y0+ dy*i, d0+ dx*DELTA)) break;
    if (i>=DELTA) return false;
    for (d=i>>2; d>=1; d=d>>1)
      if (ref_p != tree_path (x0+ dx*(i-d), y0+ dy*(i-d), d0+ dx*DELTA)) i-=d;

    x0 += dx*i;
    y0 += dy*i;
  }
  
  // cout << "path  = " << tree_path (x0, y0, d0) << "\n";
  if (dx!=0) {
    if (ref_p == tree_path (x0, y0, d0)) {
      for (i=1; i<DELTA; i=i<<1)
	if (ref_p != tree_path (x0, y0, d0+ dx*i)) break;
      if (i>=DELTA)
	fatal_error ("inconsistent cursor handling",
		     "edit_cursor_rep::cursor_move_sub");
      for (d=i>>2; d>=1; d=d>>1)
	if (ref_p != tree_path (x0, y0, d0+ dx*(i-d))) i-=d;
      d0 += dx*i;
    }
    else {
      for (i=1; i<DELTA; i=i<<1)
	if (ref_p == tree_path (x0, y0, d0- dx*i)) break;
      if (i<DELTA) {
	for (d=i>>2; d>=1; d=d>>1)
	  if (ref_p == tree_path (x0, y0, d0- dx*(i-d))) i-=d;
	i--;
	d0 -= dx*i;
      }
      else {  // exceptional case
	ref_p= tree_path (x0, y0, d0- dx*DELTA);
	for (i=1; i<DELTA; i=i<<1)
	  if (ref_p == tree_path (x0, y0, d0- dx*i)) break;
	for (d=i>>2; d>=1; d=d>>1)
	  if (ref_p == tree_path (x0, y0, d0- dx*(i-d))) i-=d;
	d0 -= dx*i;
      }
    }
  }

  tp= tree_path (x0, y0, d0);
  return true;
}

void
edit_cursor_rep::cursor_move (SI dx, SI dy) {
  cursor_move_sub (mv->ox, mv->oy, mv->delta, dx, dy);
}

/******************************************************************************
* Routines affecting both the cursor and the ghost cursor
******************************************************************************/

void
edit_cursor_rep::adjust_ghost_cursor (int status) {
  if (status==mv_status) {
    if (status!=HORIZONTAL) {
      mv->ox   = cu->ox;
      mv->delta= cu->delta;
    }
    if (status!=VERTICAL)
      mv->oy= cu->oy;
  }
}

void
edit_cursor_rep::notify_cursor_moved (int status) {
  mv_status= status;
  cu= eb->find_check_cursor (tp);
  notify_change (THE_CURSOR);
}

void
edit_cursor_rep::go_to (SI x, SI y) {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  tp= tree_path (x, y, 0);
  notify_cursor_moved (CENTER);
  mv->ox   = x;
  mv->oy   = y;
  mv->delta= 0;
}

void
edit_cursor_rep::go_left () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (VERTICAL);
  cursor_move (-1, 0);
  notify_cursor_moved (HORIZONTAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_right () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (VERTICAL);
  cursor_move (1, 0);
  notify_cursor_moved (HORIZONTAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_up () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (HORIZONTAL);
  cursor_move (0, 1);
  notify_cursor_moved (VERTICAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_down () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (HORIZONTAL);
  cursor_move (0, -1);
  notify_cursor_moved (VERTICAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_page_up () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  if (mv->oy >= eb->y2) return;
  go_to (mv->ox, mv->oy+ get_window_height ());
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_page_down () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  if (mv->oy < eb->y1) return;
  go_to (mv->ox, mv->oy- get_window_height ());
  select_from_cursor_if_active ();
}

/******************************************************************************
* Logical cursor changes
******************************************************************************/

path
edit_cursor_rep::current_position () {
  return tp;
}

path
edit_cursor_rep::path_xy (double x, double y) {
  point p= find_frame()(point (x, y));
  return tree_path ((SI)p[0], (SI)p[1], 0);
}

void
edit_cursor_rep::go_to (path p) {
  tp= p;
  mv_status= DIRECT;
  if (!has_changed (THE_TREE+THE_ENVIRONMENT)) {
    cu= eb->find_check_cursor (tp);
    mv= copy (cu);
  }
  notify_change (THE_CURSOR);
}

void
edit_cursor_rep::go_to_correct (path p) {
  p= correct_cursor (et, p);
  go_to (p);
}

void
edit_cursor_rep::go_to_start (path p) {
  go_to (start (et, p));
}

void
edit_cursor_rep::go_to_end (path p) {
  go_to (end (et, p));
}

void
edit_cursor_rep::go_to_border (path p, bool at_start) {
  if (at_start) go_to_start (p);
  else go_to_end (p);
}

void
edit_cursor_rep::go_to_here () {
  cu= eb->find_check_cursor (tp);
  if (!cu->valid) {
    tp= super_correct (et, tp);
    cu= eb->find_check_cursor (tp);
  }
  if (mv_status == DIRECT) mv= copy (cu);
  notify_change (THE_CURSOR);
}

void
edit_cursor_rep::go_start () {
  go_to (correct_cursor (et, rp * 0));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_end () {
  go_to (correct_cursor (et, rp * 1));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_start_line () {
  path p= search_parent_upwards (DOCUMENT);
  go_to (start (et, p));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_end_line () {
  path p= search_parent_upwards (DOCUMENT);
  go_to (end (et, p));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_start_of (string what) {
  path p= search_upwards (what);
  if (!nil (p)) go_to (start (et, p));
}

void
edit_cursor_rep::go_end_of (string what) {
  path p= search_upwards (what);
  if (!nil (p)) go_to (end (et, p));
}

void
edit_cursor_rep::go_start_with (string var, string val) {
  path p= search_upwards_with (var, val);
  if (!nil (p)) go_to (start (et, p));
}

void
edit_cursor_rep::go_end_with (string var, string val) {
  path p= search_upwards_with (var, val);
  if (!nil (p)) go_to (end (et, p));
}

/******************************************************************************
* Jumping to a label
******************************************************************************/

tree
edit_cursor_rep::get_labels () {
  tree r (TUPLE);
  hashmap<string,tree> h= buf->ref;
  if (buf->prj != NULL) {
    h= copy (buf->prj->ref);
    h->join (buf->ref);
  }
  iterator<string> it= iterate (h);
  while (it->busy ()) {
    string ref= it->next ();
    r << ref;
  }
  return r;
}

static path
search_tree_in (tree t, tree what) {
  if (is_atomic (t)) return path ();
  else if (t == what) return path (1);
  else {
    int i, n=N(t);
    for (i=0; i<n; i++) {
      path q= search_tree_in (t[i], what);
      if (!nil (q)) return path (i, q);
    }
    return path ();
  }
}

void
edit_cursor_rep::go_to_label (string s) {
  path p= search_tree_in (et, tree (LABEL, s));
  if (!nil (p)) go_to (p);
  else if (!nil (eb)) {
    p= eb->find_tag (s);
    if (!nil (p)) go_to (p);
  }
}
