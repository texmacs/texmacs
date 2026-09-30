
/******************************************************************************
* MODULE     : typesetter.hpp
* DESCRIPTION: Implementation of the main TeXmacs typesetting routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Bridge/impl_typesetter.hpp"
#include "iterator.hpp"

/******************************************************************************
* Constructor and destructor
******************************************************************************/

typesetter_rep::typesetter_rep (edit_env& env2, tree et, path ip):
  env (env2), old_patch (UNINIT),
  snap_pixel (0), shift_allowed (false), shift_y1 (0), shift_y2 (0),
  shift_dy (0)
{
  paper= (env->get_string (PAGE_MEDIUM) == "paper");
  br= make_bridge (this, et, ip);
  x1= y1= x2= y2=0;
}

typesetter
new_typesetter (edit_env& env, tree et, path ip) {
  return tm_new<typesetter_rep> (env, et, ip);
}

void
delete_typesetter (typesetter ttt) {
  tm_delete (ttt);
}

/******************************************************************************
* Output flux
******************************************************************************/

void
typesetter_rep::insert_stack (array<page_item> l2, stack_border sb2) {
  merge_stack (l, sb, l2, sb2);
}

void
typesetter_rep::insert_parunit (tree t, path ip) {
  insert_paragraph (t, ip);
}

void
typesetter_rep::insert_paragraph (tree t, path ip) {
  // cout << "Typesetting " << t << ", " << ip << "\n";
  stack_border     temp_sb;
  array<page_item> temp_l= typeset_stack (env, t, ip, a, b, temp_sb);
  insert_stack (temp_l, temp_sb);

  /*
  int i, n= N(temp_l);
  for (i=0; i<n; i++)
    cout << i << ", "
	 << temp_l[i]->b->find_lip () << ", "
	 << temp_l[i]->b->find_rip () << ",\t"
	 << temp_l[i]->b << "\n";
  */
}

void
typesetter_rep::insert_surround  (array<line_item> a2, array<line_item> b2) {
  a << a2;
  array<line_item> temp_b= b;
  b= copy (b2);
  b << temp_b;
}

void
typesetter_rep::insert_marker (tree st, path ip) {
  (void) st;
  // if (!is_multi_paragraph (st)) {
  array<line_item> a2= typeset_marker (env, descend (ip, 0));
  array<line_item> b2= typeset_marker (env, descend (ip, 1));
  insert_surround (a2, b2);
  // }
}

void
typesetter_rep::local_start (array<page_item>& prev_l, stack_border& prev_sb) {
  prev_l   = l;
  prev_sb  = sb;
  l        = array<page_item> ();
  sb       = stack_border ();
}

void
typesetter_rep::local_end (array<page_item>& prev_l, stack_border& prev_sb) {
  array<page_item> temp_l   = l;
  stack_border     temp_sb  = sb;
  l        = prev_l;
  sb       = prev_sb;
  prev_l   = temp_l;
  prev_sb  = temp_sb;
}

/******************************************************************************
* Main typesetting routines
******************************************************************************/

static rectangles
requires_update (rectangles log, SI y1, SI y2, SI dy) {
  // Pairs (new, old) of phrase box areas, (0, old) for a destroyed box and
  // (new, 0) for a new one.  If the editor moves the pixels between the
  // ordinates y1 and y2 by dy (dy != 0), boxes inside that band which moved
  // by exactly dy need no repainting, while whatever else was drawn in the
  // band has been moved along and must be repainted at its new place.
  rectangle zero (0, 0, 0, 0);
  rectangles rs;
  while (!is_nil (log)) {
    rectangle r1= log->item;
    rectangle r2= log->next->item;
    log= log->next->next;
    if (dy != 0 && r2 != zero && r2->y2 > y1 && r2->y1 < y2) {
      if (r1 == translate (r2, 0, dy) && r2->y1 >= y1 && r2->y2 <= y2)
        continue;
      rs= rectangles (translate (r2, 0, dy), rs);
    }
    if (r1 == zero) rs= rectangles (r2, rs);
    else if (r2 == zero) rs= rectangles (r1, rs);
    else if (r1 != r2) rs= rectangles (r1, rectangles (r2, rs));
  }
  return reverse (rs);
}

static bool
find_body (box b, box body, SI y, SI& by, array<rectangle>& others,
           int depth) {
  // Ordinate by of the origin of body inside b, looking only at a few levels
  // of boxes with few children (the pages and their parts).  The vertical
  // extents of the other children of the boxes on the path from b to body
  // (headers, footers, floats, notes, ...) are added to others.
  int i, n= b->subnr ();
  if (depth > 12 || n > 16) return false;
  for (i=0; i<n; i++) {
    box c= b->subbox (i);
    bool found= (c == body);
    if (found) by= y + b->sy (i);
    else found= find_body (c, body, y + b->sy (i), by, others, depth + 1);
    if (found) {
      for (int k=0; k<n; k++)
        if (k != i) {
          box o= b->subbox (k);
          SI  oy= y + b->sy (k);
          others << rectangle (0, oy + min (o->y1, o->y3),
                               0, oy + max (o->y2, o->y4));
        }
      return true;
    }
  }
  return false;
}

static bool
clear_of (array<rectangle> others, SI y1, SI y2) {
  for (int i=0; i<N(others); i++)
    if (others[i]->y1 < y2 && others[i]->y2 > y1) return false;
  return true;
}

void
typesetter_rep::find_shift (box b, box body, bool plain) {
  // Outside paper mode, the body of the document is one stack of lines
  // (paragraphs are merged into a single line).  When the last lines are
  // the same boxes as at the previous pass, all moved by the same multiple
  // dy of the screen pixel (see snap_stack_spacing), the editor can move
  // their pixels on the screen instead of redrawing them, provided that
  // nothing else is drawn in the band that they cover, before and after:
  // no other line of the body, no other part of the page, and only a plain
  // page background (plain; the editor checks the one of the document).
  array<box>       old_lines = body_lines;
  array<SI>        old_ys    = body_ys;
  array<rectangle> old_others= body_others;
  body_lines = array<box> ();
  body_ys    = array<SI> ();
  body_others= array<rectangle> ();
  SI oy;
  array<rectangle> others;
  if (is_nil (body) || !find_body (b, body, 0, oy, others, 0)) return;
  while (body->get_type () == MOVE_BOX && body->subnr () == 1) {
    oy += body->sy (0);
    body= body->subbox (0);
  }
  if (body->get_type () != STACK_BOX) return;
  int i, n= body->subnr ();
  for (i=0; i<n; i++) {
    body_lines << body->subbox (i);
    body_ys    << oy + body->sy (i);
  }
  body_others= others;
  if (!shift_allowed || !plain || snap_pixel <= 0) return;
  int j, m= N(old_lines);
  i= n-1; j= m-1;
  SI dy= 0;
  while (i >= 0 && j >= 0 && body_lines[i] == old_lines[j]) {
    SI d= body_ys[i] - old_ys[j];
    if (i < n-1 && d != dy) break;
    dy= d; i--; j--;
  }
  // lines i+1..n-1 of the new body are lines j+1..m-1 of the old one
  if (dy == 0 || i+1 >= n || (dy % snap_pixel) != 0) return;
  SI y1= MAX_SI, y2= MIN_SI;
  for (int k= j+1; k<m; k++) {
    box l= old_lines[k];
    y1= min (y1, old_ys[k] + min (l->y1, l->y3));
    y2= max (y2, old_ys[k] + max (l->y2, l->y4));
  }
  // the other lines, old and new, must stay clear of the band
  for (int k= 0; k<=j; k++) {
    box l= old_lines[k];
    if (old_ys[k] + min (l->y1, l->y3) < y2) return;
  }
  for (int k= 0; k<=i; k++) {
    box l= body_lines[k];
    if (body_ys[k] + min (l->y1, l->y3) < y2 + dy) return;
  }
  // and so must the other parts of the page
  if (!clear_of (old_others, y1, y2)) return;
  if (!clear_of (others, y1 + dy, y2 + dy)) return;
  shift_y1= y1; shift_y2= y2; shift_dy= dy;
}

void
typesetter_rep::determine_page_references (box b) {
  hashmap<string,tree> h ("?");
  b->collect_page_numbers (h, "?");
  iterator<string> it= iterate (h);
  while (it->busy()) {
    string var= it->next ();
    tree   val= copy (h[var]);
    tree   old= env->local_ref [var];
    if (is_func (old, TUPLE, 2))
      env->local_ref (var)= tuple (old[0], val);
    else if (is_func (old, TUPLE, 3))
      env->local_ref (var)= tuple (old[0], val, old[2]);
    else env->local_ref (var)= tuple (old, val);
    env->touched (var)= true;
  }
}

box
typesetter_rep::typeset () {
  old_patch= hashmap<string,tree> (UNINIT);
  l        = array<page_item> ();
  sb       = stack_border ();
  a        = array<line_item> ();
  b        = array<line_item> ();
  paper    = (env->get_string (PAGE_MEDIUM) == "paper");

  // Test whether we are doing a complete typesetting
  env->complete= br->my_typeset_will_be_complete ();
  tree st= br->st;
  int i= 0, n= N(st);
  if (is_compound (st[0], "show-preamble")) { i++; env->complete= false; }
  if (is_compound (st[0], "hide-preamble")) i++;
  for (; i<n && env->complete; i++) {
    if (is_compound (st[i], "hide-part")) env->complete= false;
    if (!is_compound (st[i], "show-part")) break;
  }

  // Typeset
  shove_cache_new_pass ();
  last_body= box ();
  if (env->complete) {
    env->local_aux= hashmap<string,tree> (UNINIT);
    env->missing  = hashmap<string,tree> (UNINIT);
    env->redefined= array<tree> ();
    env->touched  = hashmap<string,bool> (false);
  }
  br->typeset (PROCESSED+ WANTED_PARAGRAPH);
  shove_cache_end_pass ();
  pager ppp= tm_new<pager_rep> (br->ip, env, l);
  if (!paper) ppp->snap= snap_pixel;
  box rb= ppp->make_pages ();
  if (!is_nil (ppp->body)) last_body= ppp->body;
  if (env->complete && paper) determine_page_references (rb);
  tm_delete (ppp);
  // env->complete= false;  // moved to edit_typeset_rep::typeset
  return rb;
}

box
typesetter_rep::typeset (SI& x1b, SI& y1b, SI& x2b, SI& y2b) {
  x1= x1b; y1= y1b; x2=x2b; y2= y2b;
  box b= typeset ();
  // cout << "-------------------------------------------------------------\n";
  array<brush> new_bgs;
  array<rectangle> rs;
  b->collect_page_colors (new_bgs, rs);
  bool plain= true;
  for (int i=0; i<N(new_bgs); i++)
    plain= plain && new_bgs[i]->get_type () != brush_pattern;
  for (int i=0; i<N(old_bgs); i++)
    plain= plain && old_bgs[i]->get_type () != brush_pattern;
  shift_dy= 0;
  find_shift (b, last_body, plain);  // before position_at: frees old lines
  last_body= box ();
  b->position_at (0, 0, change_log);
  change_log= requires_update (change_log, shift_y1, shift_y2, shift_dy);
  rectangle r (0, 0, 0, 0);
  if (!is_nil (change_log)) r= least_upper_bound (change_log);
  shift_rects= (shift_dy != 0? change_log: rectangles ());
  for (int i=0; i<min(N(old_bgs), N(new_bgs)); i++)
    if (new_bgs[i] != old_bgs[i]) {
      r= least_upper_bound (r, rs[i]);
      if (shift_dy != 0) shift_rects= rectangles (rs[i], shift_rects);
    }
  old_bgs= new_bgs;
  x1b= r->x1; y1b= r->y1; x2b= r->x2; y2b= r->y2;
  change_log= rectangles ();
  return b;
}

/******************************************************************************
* Event notification
******************************************************************************/

void
notify_assign (typesetter ttt, path p, tree u) {
  // cout << "Assign " << p << ", " << u << "\n";
  if (is_nil (p)) ttt->br= make_bridge (ttt, u, ttt->br->ip);
  else ttt->br->notify_assign (p, u);
}

void
notify_insert (typesetter ttt, path p, tree u) {
  // cout << "Insert " << p << ", " << u << "\n";
  ttt->br->notify_insert (p, u);
}

void
notify_remove (typesetter ttt, path p, int nr) {
  // cout << "Remove " << p << ", " << nr << "\n";
  ttt->br->notify_remove (p, nr);
}

void
notify_split (typesetter ttt, path p) {
  // cout << "Split " << p << "\n";
  ttt->br->notify_split (p);
}

void
notify_join (typesetter ttt, path p) {
  // cout << "Join " << p << "\n";
  ttt->br->notify_join (p);
}

void
notify_assign_node (typesetter ttt, path p, tree_label op) {
  // cout << "Assign node " << p << ", " << as_string (op) << "\n";
  tree t= subtree (ttt->br->st, p);
  int i, n= N(t);
  tree r (op, n);
  for (i=0; i<n; i++) r[i]= t[i];
  if (is_nil (p)) ttt->br= make_bridge (ttt, r, ttt->br->ip);
  else ttt->br->notify_assign (p, r);
}

void
notify_insert_node (typesetter ttt, path p, tree t) {
  // cout << "Insert node " << p << ", " << t << "\n";
  int i, pos= last_item (p), n= N(t);
  tree r (t, n+1);
  for (i=0; i<pos; i++) r[i]= t[i];
  r[pos]= subtree (ttt->br->st, path_up (p));
  for (i=pos; i<n; i++) r[i+1]= t[i];
  if (is_nil (path_up (p))) ttt->br= make_bridge (ttt, r, ttt->br->ip);
  else ttt->br->notify_assign (path_up (p), r);
}

void
notify_remove_node (typesetter ttt, path p) {
  // cout << "Remove node " << p << "\n";
  tree t= subtree (ttt->br->st, p);
  if (is_nil (path_up (p))) ttt->br= make_bridge (ttt, t, ttt->br->ip);
  else ttt->br->notify_assign (path_up (p), t);
}

/******************************************************************************
* Getting environment variables and typesetting interface
******************************************************************************/

void
exec_until (typesetter ttt, path p) {
  ttt->br->exec_until (p);
}

box
typeset (typesetter ttt, SI& x1, SI& y1, SI& x2, SI& y2) {
  return ttt->typeset (x1, y1, x2, y2);
}

box
typeset_as_document (edit_env env, tree t, path ip) {
  env->style_init_env ();
  env->update ();
  typesetter ttt= new_typesetter (env, t, ip);
  box b= ttt->typeset ();
  delete_typesetter (ttt);
  return b;
}
