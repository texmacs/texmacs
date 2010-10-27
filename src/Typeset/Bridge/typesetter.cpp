
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
  env (env2), old_patch (UNINIT)
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
requires_update (rectangles log) {
  rectangles rs;
  while (!is_nil (log)) {
    rectangle r1= log->item;
    rectangle r2= log->next->item;
    if (r1 == rectangle (0, 0, 0, 0)) rs= rectangles (r2, rs);
    else if (r2 == rectangle (0, 0, 0, 0)) rs= rectangles (r1, rs);
    else if (r1 != r2) rs= rectangles (r1, rectangles (r2, rs));
    log= log->next->next;
  }
  return reverse (rs);
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
  if (env->complete) env->local_aux= hashmap<string,tree> (UNINIT);
  br->typeset (PROCESSED+ WANTED_PARAGRAPH);
  pager ppp= tm_new<pager_rep> (br->ip, env, l);
  box b= ppp->make_pages ();
  if (env->complete && paper) determine_page_references (b);
  tm_delete (ppp);
  env->complete= false;

  return b;
}

box
typesetter_rep::typeset (SI& x1b, SI& y1b, SI& x2b, SI& y2b) {
  x1= x1b; y1= y1b; x2=x2b; y2= y2b;
  box b= typeset ();
  // cout << "-------------------------------------------------------------\n";
  b->position_at (0, 0, change_log);
  change_log= requires_update (change_log);
  rectangle r (0, 0, 0, 0);
  if (!is_nil (change_log)) r= least_upper_bound (change_log);
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
