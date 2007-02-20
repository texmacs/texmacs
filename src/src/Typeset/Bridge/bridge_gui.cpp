
/******************************************************************************
* MODULE     : bridge_with.cpp
* DESCRIPTION: Bridge for GUI markup
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bridge.hpp"
#include "analyze.hpp"

void get_canvas_horizontal (edit_env env, tree attrs, SI bx1, SI bx2,
			    SI& x1, SI& x2, SI& scx);
void get_canvas_vertical   (edit_env env, tree attrs, SI by1, SI by2,
			    SI& y1, SI& y2, SI& scy);
box put_scroll_bars (edit_env env, box b, path ip, tree attrs,
		     box inner, tree xt, tree yt, SI scx, SI scy);

/******************************************************************************
* Abstract bridge for ornaments
******************************************************************************/

class bridge_ornament_rep: public bridge_rep {
protected:
  int    last;
  bridge body;
  tree   with;

public:
  bridge_ornament_rep (typesetter ttt, tree st, path ip);
  void initialize ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  box  typeset_ornament (int desired_status);
  void insert_ornament (box b);
};

bridge_ornament_rep::bridge_ornament_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip), with (tree (TUPLE))
{
  initialize ();
}

void
bridge_ornament_rep::initialize () {
  last= N(st)-1;
  if (nil(body)) body= make_bridge (ttt, st[last], descend (ip, last));
  else replace_bridge (body, st[last], descend (ip, last));
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_ornament_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  if (nil (p)) {
    st= u;
    initialize ();
  }
  else {
    bool mp_flag= is_multi_paragraph (st);
    if (p->item == last) {
      if (atom (p)) body= make_bridge (ttt, u, descend (ip, last));
      else body->notify_assign (p->next, u);
      st= substitute (st, p->item, body->st);
    }
    else {
      st= substitute (st, p, u);
      body->notify_change ();
    }
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_ornament_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  if (nil (p)) fatal_error ("Nil path", "bridge_ornament_rep::notify_insert");
  if (atom (p) || (p->item != last)) bridge_rep::notify_insert (p, u);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_insert (p->next, u);
    st= substitute (st, last, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_ornament_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  if (nil (p)) fatal_error ("Nil path", "bridge_ornament_rep::notify_remove");
  if (atom (p) || (p->item != last)) bridge_rep::notify_remove (p, nr);
  else {
    bool mp_flag= is_multi_paragraph (st);
    body->notify_remove (p->next, nr);
    st= substitute (st, last, body->st);
    if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

bool
bridge_ornament_rep::notify_macro (int type, string var, int l, path p, tree u) {
  bool flag= body->notify_macro (type, var, l, p, u);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_ornament_rep::notify_change () {
  status= CORRUPTED;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_ornament_rep::my_exec_until (path p) {
  for (int i=0; i<N(with)-1; i+=2)
    env->monitored_write_update (with[i]->label, with[i+1]);
  body->exec_until (p->next);
}

bool
bridge_ornament_rep::my_typeset_will_be_complete () {
  if (status != CORRUPTED) return false;
  return body->my_typeset_will_be_complete ();
}

static box
make_ornament_body (path ip, array<page_item> l) {
  int i, n= N(l);
  array<box> lines_bx (n);
  array<SI>  lines_ht (n);
  for (i=0; i<n; i++) {
    page_item item= copy (l[i]);
    lines_bx[i]= item->b;
    lines_ht[i]= item->spc->def;
  }
  box b= stack_box (ip, lines_bx, lines_ht);
  SI dy= n==0? 0: b[0]->y2;
  return move_box (decorate (ip), stack_box (ip, lines_bx, lines_ht), 0, dy);
}

box
bridge_ornament_rep::typeset_ornament (int desired_status) {
  int i;
  tree old (TUPLE, N(with));
  for (i=0; i<N(with)-1; i+=2) {
    old[i+1]= env->read (with[i]->label);
    env->write_update (with[i]->label, with[i+1]);
  }
  array<page_item> l2;
  stack_border sb2;
  ttt->local_start (l2, sb2);
  body->typeset (desired_status);
  ttt->local_end (l2, sb2);
  for (i-=2; i>=0; i-=2)
    env->write_update (with[i]->label, old[i+1]);
  return make_ornament_body (ip, l2);
}

void
bridge_ornament_rep::insert_ornament (box b) {
  ttt->insert_marker (st, ip);
  array<page_item> l2= array<page_item> (1);
  l2[0]= page_item (b);
  ttt->insert_stack (l2, stack_border ());
}

/******************************************************************************
* Canvases
******************************************************************************/

class bridge_canvas_rep: public bridge_ornament_rep {
public:
  bridge_canvas_rep (typesetter ttt, tree st, path ip):
    bridge_ornament_rep (ttt, st, ip) {}
  void my_typeset (int desired_status);
};

bridge
bridge_canvas (typesetter ttt, tree st, path ip) {
  return new bridge_canvas_rep (ttt, st, ip);
}

void
bridge_canvas_rep::my_typeset (int desired_status) {
  int i, n= N(st);
  tree attrs (TUPLE, n-1);
  for (i=0; i<4; i++)
    attrs[i]= env->exec (st[i]);
  tree xt = env->expand (st[4]);
  tree yt = env->expand (st[5]);
  attrs[4]= env->exec (xt);
  attrs[5]= env->exec (yt);
  for (i=6; i<n-1; i++)
    attrs[i]= env->exec (st[i]);    

  SI delta= 0;
  if (N(attrs) > 6) {
    SI w  = env->as_length (attrs[6]);
    SI pad= env->as_length (attrs[7]);
    SI bor= 6*env->get_int (SFACTOR) * PIXEL;
    if (is_atomic (attrs[8]))
      if (ends (attrs[8]->label, "w") || ends (attrs[8]->label, "e"))
	delta= max (0, w + pad);
    delta += 2 * bor;
  }
  SI l= env->get_length (PAR_LEFT);
  SI r= env->get_length (PAR_RIGHT) + delta;
  with= tuple (PAR_RIGHT, tree (TMLEN, as_string (r)));

  box b = typeset_ornament (desired_status);

  SI x1, y1, x2, y2, scx, scy;
  get_canvas_horizontal (env, attrs, b->x1, b->x2, x1, x2, scx);
  get_canvas_vertical (env, attrs, b->y1, b->y2, y1, y2, scy);
  path dip= (n > 7? decorate (ip): ip);
  box cb= clip_box (dip, b, x1, y1, x2, y2, xt, yt, scx, scy);
  if (n > 7) cb= put_scroll_bars (env, cb, ip, attrs, b, xt, yt, scx, scy);
  if (l != 0) cb= move_box (decorate (ip), cb, l, 0);
  insert_ornament (cb);
}

/******************************************************************************
* Hightlighting
******************************************************************************/

class bridge_highlight_rep: public bridge_ornament_rep {
public:
  bridge_highlight_rep (typesetter ttt, tree st, path ip):
    bridge_ornament_rep (ttt, st, ip) {}
  void my_typeset (int desired_status);
};

bridge
bridge_highlight (typesetter ttt, tree st, path ip) {
  return new bridge_highlight_rep (ttt, st, ip);
}

void
bridge_highlight_rep::my_typeset (int desired_status) {
  SI    w     = env->as_length (env->exec (st[0]));
  SI    xpad  = env->as_length (env->exec (st[1]));
  SI    ypad  = env->as_length (env->exec (st[2]));
  color bg    = env->dis->get_color (env->exec_string (st[3]));
  color sunny = env->dis->get_color (env->exec_string (st[4]));
  color shadow= env->dis->get_color (env->exec_string (st[5]));
  SI l= env->get_length (PAR_LEFT ) + w + xpad;
  SI r= env->get_length (PAR_RIGHT) + w + xpad;
  with= tuple (PAR_LEFT , tree (TMLEN, as_string (l))) *
        tuple (PAR_RIGHT, tree (TMLEN, as_string (r)));
  box b = typeset_ornament (desired_status);
  box hb= highlight_box (ip, b, w, xpad, ypad, bg, sunny, shadow);
  insert_ornament (move_box (decorate (ip), hb, -w-xpad, 0));
}
