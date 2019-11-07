
/******************************************************************************
* MODULE     : cursor.cpp
* DESCRIPTION: cursor handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_cursor.hpp"
#include "iterator.hpp"
#include "tm_buffer.hpp"
#include "tree_traverse.hpp"
#include "drd_mode.hpp"
#include "analyze.hpp"

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

static bool searching_forwards;

//#define old_cursor_accessible

#ifdef old_cursor_accessible
path
edit_cursor_rep::make_cursor_accessible (path p, bool forwards) {
  //time_t t1= texmacs_time ();
  path start_p= p;
  bool inverse= false;
  int old_mode= get_access_mode ();
  if (get_init_string (MODE) == "src")
    set_access_mode (DRD_ACCESS_SOURCE);
  while (!is_accessible_cursor (et, p) && !in_source ()) {
    path pp;
    ASSERT (rp <= p, "path outside document");
    p= rp * closest_inside (subtree (et, rp), p / rp);
    if (forwards ^ inverse)
      pp= rp * next_valid (subtree (et, rp), p / rp);
    else
      pp= rp * previous_valid (subtree (et, rp), p / rp);
    if (pp == p) {
      if (inverse) break;
      else { p= start_p; inverse= true; }
    }
    else p= pp;
  }
  set_access_mode (old_mode);
  //time_t t2= texmacs_time ();
  //if (t2-t1 >= 1) cout << "made_cursor_accessible took " << t2-t1 << "ms\n";
  return p;
}
#else
path
edit_cursor_rep::make_cursor_accessible (path p, bool forwards) {
  //time_t t1= texmacs_time ();
  path start_p= p;
  bool inverse= false;
  int old_mode= get_access_mode ();
  if (get_init_string (MODE) == "src")
    set_access_mode (DRD_ACCESS_SOURCE);
  while (!is_accessible_cursor (et, p) && !in_source ()) {
    ASSERT (rp <= p, "path outside document");
    tree st = subtree (et, rp);
    path sp = p / rp;
    path pp = sp;
    int  dir= (forwards ^ inverse)? 1: -1;
    path cp = closest_accessible_inside (st, sp, dir);
    if (cp != sp) {
      /*
      if (( forwards && path_less (cp, sp)) ||
          (!forwards && path_less (sp, cp)))
        cout << "Warning: " << sp << LF
             << "  ->   : " << cp << LF
             << "Tree   : " << subtree (st, path_up (sp)) << LF
             << "  ->   : " << subtree (st, path_up (cp)) << LF;
      */
      sp= cp;
    }
    else {
      if (dir > 0) sp= next_valid (st, cp);
      else sp= previous_valid (st, cp);
    }
    if ((dir > 0 && !path_less (pp, sp)) ||
        (dir < 0 && !path_less (sp, pp))) {
      if (inverse) {
        p= rp * closest_accessible_inside (st, sp, forwards? 1: -1); break; }
      else {
        p= start_p; inverse= true; }
    }
    else p= rp * sp;
  }
  set_access_mode (old_mode);
  //time_t t2= texmacs_time ();
  //if (t2-t1 >= 1) cout << "made_cursor_accessible took " << t2-t1 << "ms\n";
  return p;
}
#endif

path
edit_cursor_rep::tree_path (path sp, SI x, SI y, SI delta) {
  path stp= find_scrolled_tree_path (eb, sp, x, y, delta);
  path p= correct_cursor (et, stp /*, searching_forwards */);
  return make_cursor_accessible (p, searching_forwards);
}

bool
edit_cursor_rep::cursor_move_sub (SI& x0, SI& y0, SI& d0, SI dx, SI dy) {
  path sp= find_innermost_scroll (eb, tp);
  searching_forwards= dx == 1 || dy == -1;

  int i,d;
  path ref_p= tree_path (sp, x0, y0, d0);
  if (ref_p != tp) {
#ifdef old_cursor_accessible
    tp= ref_p;
    return true;
#else
    if (!searching_forwards && path_less (tp, ref_p));
    else if (searching_forwards && path_less (ref_p, tp));
    else {
      tp= ref_p;
      return true;
    }
#endif
  }
  
  // cout << "ref_p = " << ref_p << "\n";
  if (ref_p == tree_path (sp, x0, y0, d0+ dx*DELTA)) {
    for (i=1; i<DELTA; i=i<<1)
      if (ref_p != tree_path (sp, x0+ dx*i, y0+ dy*i, d0+ dx*DELTA))
        break;
    if (i>=DELTA) return false;
    for (d=i>>2; d>=1; d=d>>1)
      if (ref_p != tree_path (sp, x0+ dx*(i-d), y0+ dy*(i-d), d0+ dx*DELTA))
        i-=d;

    x0 += dx*i;
    y0 += dy*i;
  }
  
  // cout << "path  = " << tree_path (sp, x0, y0, d0) << "\n";
  if (dx!=0) {
    if (ref_p == tree_path (sp, x0, y0, d0)) {
      for (i=1; i<DELTA; i=i<<1)
        if (ref_p != tree_path (sp, x0, y0, d0+ dx*i)) break;
      if (i>=DELTA)
        FAILED ("inconsistent cursor handling");
      for (d=i>>2; d>=1; d=d>>1)
        if (ref_p != tree_path (sp, x0, y0, d0+ dx*(i-d))) i-=d;
      d0 += dx*i;
    }
    else {
      for (i=1; i<DELTA; i=i<<1)
        if (ref_p == tree_path (sp, x0, y0, d0- dx*i)) break;
      if (i<DELTA) {
        for (d=i>>2; d>=1; d=d>>1)
          if (ref_p == tree_path (sp, x0, y0, d0- dx*(i-d))) i-=d;
        i--;
        d0 -= dx*i;
      }
      else {  // exceptional case
        ref_p= tree_path (sp, x0, y0, d0- dx*DELTA);
        for (i=1; i<DELTA; i=i<<1)
          if (ref_p == tree_path (sp, x0, y0, d0- dx*i)) break;
        for (d=i>>2; d>=1; d=d>>1)
          if (ref_p == tree_path (sp, x0, y0, d0- dx*(i-d))) i-=d;
        d0 -= dx*i;
      }
    }
  }

  tp= tree_path (sp, x0, y0, d0);
  return true;
}

void
edit_cursor_rep::cursor_move (SI dx, SI dy) {
  //time_t t1= texmacs_time ();
  //stretched_print ((tree) eb, false);
  cursor_move_sub (mv->ox, mv->oy, mv->delta, dx, dy);
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "cursor_move took " << t2-t1 << "ms\n";
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
    if (status!=VERTICAL) {
      SI dy= (cu->y1 + cu->y2) >> 1;
      mv->oy= cu->oy + dy;
    }
  }
}

void
edit_cursor_rep::notify_cursor_moved (int status) {
  mv_status= status;
  cu= eb->find_check_cursor (tp);
  notify_change (THE_CURSOR);
  if (cu->valid) call ("notify-cursor-moved", object (status));
}

void
edit_cursor_rep::go_to (SI x, SI y, bool absolute) {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  tp= tree_path (absolute? path (): find_innermost_scroll (eb, tp), x, y, 0);
  notify_cursor_moved (CENTER);
  mv->ox   = x;
  mv->oy   = y;
  mv->delta= 0;
}

void
edit_cursor_rep::go_left_physical () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (VERTICAL);
  cursor_move (-1, 0);
  notify_cursor_moved (HORIZONTAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_right_physical () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  adjust_ghost_cursor (VERTICAL);
  cursor_move (1, 0);
  notify_cursor_moved (HORIZONTAL);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_up () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path scroll_p= find_innermost_scroll (eb, tp);
  path start_p= tree_path (scroll_p, -(1 << 30), 1 << 30, 0);
  if (tp == start_p) return;
  path old_p= tp;
  adjust_ghost_cursor (HORIZONTAL);
  cursor_move (0, 1);
  notify_cursor_moved (VERTICAL);
  if (tp == old_p) tp= start_p;
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_down () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path scroll_p= find_innermost_scroll (eb, tp);
  path end_p= tree_path (scroll_p, 1 << 30, -(1 << 30), 0);
  if (tp == end_p) return;
  path old_p= tp;
  adjust_ghost_cursor (HORIZONTAL);
  cursor_move (0, -1);
  notify_cursor_moved (VERTICAL);
  if (tp == old_p) tp= end_p;
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_page_up () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path sp= find_innermost_scroll (eb, tp);
  if (is_nil (sp)) go_to (mv->ox, min (mv->oy + get_visible_height (), eb->y2));
  else {
    SI x, y, sx, sy;
    rectangle outer, inner;
    box b= eb[path_up (sp)];
    find_canvas_info (eb, sp, x, y, sx, sy, outer, inner);
    go_to (mv->ox, min (mv->oy + b->h (), y + sy + inner->y2), false);
  }
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_page_down () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path sp= find_innermost_scroll (eb, tp);
  if (is_nil (sp)) go_to (mv->ox, max (mv->oy - get_visible_height (), eb->y1));
  else {
    SI x, y, sx, sy;
    rectangle outer, inner;
    box b= eb[path_up (sp)];
    find_canvas_info (eb, sp, x, y, sx, sy, outer, inner);
    go_to (mv->ox, max (mv->oy - b->h (), y + sy + inner->y1), false);
  }
  select_from_cursor_if_active ();
}

/******************************************************************************
* Adapt physical horizontal cursor movement to line breaking
******************************************************************************/

void
edit_cursor_rep::go_left () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path old_tp= copy (tp);
  go_left_physical ();
  if (tp != old_tp && inside_contiguous_document (et, old_tp, tp)) return;
  path p= previous_valid (et, old_tp);
  if (rp < p) go_to (p);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_right () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path old_tp= copy (tp);
  go_right_physical ();
  if (tp != old_tp && inside_contiguous_document (et, old_tp, tp)) return;
  path p= next_valid (et, old_tp);
  if (rp < p) go_to (p);
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_start_line () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path orig_tp= copy (tp);
  while (true) {
    cursor old_cu= copy (cu);
    cursor old_mv= copy (mv);
    path   old_tp= copy (tp);
    adjust_ghost_cursor (VERTICAL);
    cursor_move (-1, 0);
    if (tp == old_tp || !inside_same_or_more (et, tp, orig_tp, DOCUMENT)) {
      notify_cursor_moved (HORIZONTAL);
      cu= old_cu;
      mv= old_mv;
      tp= old_tp;
      select_from_cursor_if_active ();
      return;
    }
  }
}

void
edit_cursor_rep::go_end_line () {
  if (has_changed (THE_TREE+THE_ENVIRONMENT)) return;
  path orig_tp= copy (tp);
  while (true) {
    cursor old_cu= copy (cu);
    cursor old_mv= copy (mv);
    path   old_tp= copy (tp);
    adjust_ghost_cursor (VERTICAL);
    cursor_move (1, 0);
    if (tp == old_tp || !inside_same_or_more (et, tp, orig_tp, DOCUMENT)) {
      notify_cursor_moved (HORIZONTAL);
      cu= old_cu;
      mv= old_mv;
      tp= old_tp;
      select_from_cursor_if_active ();
      return;
    }
  }
}

/******************************************************************************
* Logical cursor changes
******************************************************************************/

void
edit_cursor_rep::adjust_cursor () {
  path sp= find_innermost_scroll (eb, tp);
  cursor mv= copy (cu);
  SI dx= PIXEL << 8, ddelta= 0;
  path p= tree_path (sp, mv->ox, mv->oy, mv->delta);
  if (p != tp) {
    // cout << "Cursors don't match\n";
    while (dx != 0 || ddelta != 0) {
      // cout << "  " << tp << ", " << p << "\n";
      p= tree_path (sp, mv->ox, mv->oy, mv->delta);
      int eps= (path_inf (p, tp)? 1: -1);
      if (p == tp) eps= (mv->ox < cu->ox? 1: -1);
      if (p == tp && mv->ox == cu->ox) eps= (mv->delta < cu->delta? 1: -1);
      if (dx > 0) {
        if (p != tp ||
            tree_path (sp, mv->ox + eps * dx, mv->oy, mv->delta) == tp)
          mv->ox += eps * dx;
        dx >>= 1;
        if (dx == 0) ddelta= DELTA;
      }
      else if (ddelta > 0) {
        if (p != tp ||
            tree_path (sp, mv->ox, mv->oy, mv->delta + eps * ddelta) == tp)
          mv->delta += eps * ddelta;
        ddelta >>= 1;
      }
    }
  }
  if (p == tp) cu= mv;
}

void
edit_cursor_rep::go_to_here () {
  cu= eb->find_check_cursor (tp);
  if (!cu->valid || !valid_cursor (et, tp)) {
    tp= super_correct (et, tp);
    cu= eb->find_check_cursor (tp);
  }
  if (!cu->valid || !valid_cursor (et, tp)) {
    tp= make_cursor_accessible (tp, false);
    cu= eb->find_check_cursor (tp);
  }
  if (cu->valid) adjust_cursor ();
  if (mv_status == DIRECT) mv= copy (cu);
  notify_change (THE_CURSOR);
  if (cu->valid) call ("notify-cursor-moved", object (DIRECT));
}

void
edit_cursor_rep::go_to (path p) {
  if (rp <= p) {
    //if (tp != p) cout << "Go to " << p << "\n";
    tp= p;
    mv_status= DIRECT;
    if (!has_changed (THE_TREE+THE_ENVIRONMENT)) {
      cu= eb->find_check_cursor (tp);
      if (cu->valid) adjust_cursor ();
      mv= copy (cu);
    }
    notify_change (THE_CURSOR);
    if (cu->valid) call ("notify-cursor-moved", object (DIRECT));
  }
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
edit_cursor_rep::go_start_paragraph () {
  path p= search_parent_upwards (DOCUMENT);
  go_to (start (et, p));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_end_paragraph () {
  path p= search_parent_upwards (DOCUMENT);
  go_to (end (et, p));
  select_from_cursor_if_active ();
}

void
edit_cursor_rep::go_start_of (tree_label what) {
  path p= search_upwards (what);
  if (!is_nil (p)) go_to (start (et, p));
}

void
edit_cursor_rep::go_end_of (tree_label what) {
  path p= search_upwards (what);
  if (!is_nil (p)) go_to (end (et, p));
}

void
edit_cursor_rep::go_start_with (string var, string val) {
  path p= search_upwards_with (var, val);
  if (!is_nil (p)) go_to (start (et, p));
}

void
edit_cursor_rep::go_end_with (string var, string val) {
  path p= search_upwards_with (var, val);
  if (!is_nil (p)) go_to (end (et, p));
}

/******************************************************************************
* Jumping to a label
******************************************************************************/

tree
edit_cursor_rep::get_labels () {
  tree r (TUPLE);
  hashmap<string,tree> h= buf->data->ref;
  if (buf->prj != NULL) {
    h= copy (buf->prj->data->ref);
    h->join (buf->data->ref);
  }
  iterator<string> it= iterate (h);
  while (it->busy ()) {
    string ref= it->next ();
    r << ref;
  }
  return r;
}

static path
search_label (tree t, string which) {
  if (is_atomic (t)) return path ();
  else if (t == tree (LABEL, which)) return path (1);
  else if (is_compound (t, "tag", 2) && t[0] == which)
    return path (1, start (t[1]));
  else {
    int i, n=N(t);
    for (i=0; i<n; i++) {
      path q= search_label (t[i], which);
      if (!is_nil (q)) return path (i, q);
    }
    return path ();
  }
}

bool
edit_cursor_rep::cursor_is_accessible () {
  return is_accessible_cursor (et, tp);
}

void
edit_cursor_rep::show_cursor_if_hidden () {
  if (!is_accessible_cursor (et, tp) && !in_source ()) {
    eval ("(use-modules (utils edit variants))");
    eval ("(cursor-show-hidden)");
  }
}

void
edit_cursor_rep::go_to_label (string s) {
  path p= search_label (subtree (et, rp), s);
  if (!is_nil (p)) {
    go_to (rp * p);
    show_cursor_if_hidden ();
    return;
  }
  if (!is_nil (eb)) {
    p= eb->find_tag (s);
    if (!is_nil (p)) {
      go_to (p);
      show_cursor_if_hidden ();
      return;
    }
  }
  tree val= (buf->prj==NULL? buf->data->ref[s]: buf->prj->data->ref[s]);
  if (is_func (val, TUPLE, 3) && is_atomic (val[2])) {
    string extra= val[2]->label;
    if (starts (extra, "#")) {
      string part= extra (1, N (extra));
      int i= search_forwards (".", part);
      if (i >= 0) part= part (0, i);
      string show= "(show-hidden-part " * scm_quote (part) * ")";
      string jump= "(go-to-label " * scm_quote (s) * ")";
      exec_delayed (scheme_cmd ("(if " * show * " (delayed " * jump * "))"));
    }
    else {
      url u= relative (buf->buf->name, url (extra));
      if (u != buf->buf->name) {
        string new_buf = scm_quote (as_string (u));
        string load_buf= "(load-buffer (system->url " * new_buf * "))";
        string jump_to = "(go-to-label " * scm_quote (s) * ")";
        exec_delayed (scheme_cmd ("(begin " * load_buf * " " * jump_to * ")"));
      }
    }
  }
}
