
/******************************************************************************
* MODULE     : concat_active.cpp
* DESCRIPTION: Typeset active markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "packrat.hpp"

/******************************************************************************
* Typesetting executable markup
******************************************************************************/

void
concater_rep::typeset_if (tree t, path ip) {
  // This method must be kept consistent with edit_env_rep::exec(tree)
  // in ../Env/env_exec.cpp
  if ((N(t)!=2) && (N(t)!=3)) {
    typeset_executable (t, ip);
    return;
  }
  tree tt= env->exec (t[0]);
  if (is_compound (tt) || ! is_bool (tt->label)) {
    typeset_executable (t, ip);
    return;
  }
  marker (descend (ip, 0));
  if (as_bool (tt->label)) typeset (t[1], descend (ip, 1));
  else if (N(t) == 3) typeset (t[2], descend (ip, 2));
  marker (descend (ip, 1));
}

void
concater_rep::typeset_var_if (tree t, path ip) {
  tree flag= env->exec (t[0]);
  box  b   = typeset_as_concat (env, attach_right (t[1], ip));
  marker (descend (ip, 0));
  if (flag == "true") print (STD_ITEM, b);
  else print (STD_ITEM, empty_box (b->ip, b->x1, b->y1, b->x2, b->y2));
  marker (descend (ip, 1));
}

void
concater_rep::typeset_case (tree t, path ip) {
  // This method must be kept consistent with edit_env_rep::exec(tree)
  // in ../Env/env_exec.cpp
  if (N(t)<2) {
    typeset_executable (t, ip);
    return;
  }
  marker (descend (ip, 0));
  int i, n= N(t);
  for (i=0; i<(n-1); i+=2) {
    tree tt= env->exec (t[i]);
    if (is_compound (tt) || !is_bool (tt->label)) {
      typeset_executable (t, ip);
      i=n;
    }
    else if (as_bool (tt->label)) {
      typeset (t[i+1], descend (ip, i+1));
      i=n;
    }
  }
  if (i<n) typeset (t[i], descend (ip, i));
  marker (descend (ip, 1));
}

/******************************************************************************
* Typesetting linking primitives
******************************************************************************/

bool
build_locus (edit_env env, tree t, list<string>& ids, string& col) {
  //cout << "Typeset " << t << "\n";
  int last= N(t)-1;
  tree body= env->expand (t[last], true);
  //cout << "Typeset " << body << "\n";
  bool accessible= is_accessible (obtain_ip (body));
  bool visited= false;
  if (!is_nil (env->link_env)) {
    int i, j;
    for (i=0; i<last; i++) {
      tree arg= env->exec (t[i]);
      if (is_compound (arg, "id", 1)) {
	string id= as_string (arg[0]);
	if (accessible) env->link_env->insert_locus (id, body);
	else if (N (obtain_ip (body)) > 1) {
	  extern tree get_subtree (path p);
	  path p= path_up (reverse (descend_decode (obtain_ip (body), 1)));
	  env->link_env->insert_locus ("&" * id, get_subtree (p));
	}
	ids= list<string> (id, ids);
	visited= visited || has_been_visited ("id:" * id);
      }
      if (is_compound (arg, "link") && N(arg) >= 2) {
	if (is_func (arg[1], ATTR)) arg= copy (arg);
	else arg= arg (0, 1) * tree (LINK, tree (ATTR)) * arg (1, N(arg));
	arg[1] << tree ("secure")
	       << (env->secure? tree ("true"): tree ("false"));
	env->link_env->insert_link (arg);
	for (j=2; j<N(arg); j++) {
	  if (is_compound (arg[j], "id", 1) && is_atomic (arg[j][0]))
	    visited= visited || has_been_visited ("id:" * arg[j][0]->label);
	  if (is_compound (arg[j], "url", 1) && is_atomic (arg[j][0]))
	    visited= visited || has_been_visited ("url:" * arg[j][0]->label);
	}
      }
    }
  }

  bool on_paper= (env->get_string (PAGE_PRINTED) == "true");
  bool preserve= (get_locus_rendering ("locus-on-paper") == "preserve");
  string var= (visited? VISITED_COLOR: LOCUS_COLOR);
  string current_col= env->get_string (COLOR);
  string locus_col= env->get_string (var);
  if (locus_col == "preserve") col= current_col;
  else if (on_paper && preserve) col= locus_col;
  else if (locus_col == "global") col= get_locus_rendering (var);
  else col= locus_col;

  return accessible;
}

void
concater_rep::typeset_locus (tree t, path ip) {
  int last= N(t)-1;
  list<string> ids;
  string col;
  if (build_locus (env, t, ids, col)) {
    marker (descend (ip, 0));
    tree old= env->local_begin (COLOR, col);
    typeset (t[last], descend (ip, last));
    env->local_end (COLOR, old);
    marker (descend (ip, 1));
  }
  else {
    tree old= env->local_begin (COLOR, col);
    box b= typeset_as_concat (env, t[last], descend (ip, last));
    env->local_end (COLOR, old);
    print (STD_ITEM, locus_box (ip, b, ids, env->get_int (SFACTOR) * PIXEL));
  }
}

void
concater_rep::typeset_set_binding (tree t, path ip) {
  tree keys= env->exec (t);
  if (L(keys) == HIDDEN) {
    keys= keys[1];
    flag ("set binding", ip, blue);
    if (N(keys) > 0) {
      path sip= ip;
      if (N(t) >= 3 && (!is_nil (env->macro_src))) {
	tree body= env->expand (tree (ARG, t[2]), true);
	sip= obtain_ip (body);
      }
      box b= tag_box (sip, empty_box (sip, 0, 0, 0, env->fn->yx), keys);
      a << line_item (CONTROL_ITEM, b, HYPH_INVALID, "label");
    }
  }
  else typeset_dynamic (keys, ip);
}

void
concater_rep::typeset_write (tree t, path ip) {
  if (N(t)==2) {
    string s= env->exec_string (t[0]);
    tree   r= copy (env->exec (t[1]));
    if (env->complete) {
      if (!env->local_aux->contains (s))
	env->local_aux (s)= tree (DOCUMENT);
      env->local_aux (s) << r;
    }
  }
  control ("write", ip);
}

/******************************************************************************
* Typesetting other dynamic markup
******************************************************************************/

void
concater_rep::typeset_specific (tree t, path ip) {
  string which= env->exec_string (t[0]);
  if (which == "texmacs" || which == "image") {
    marker (descend (ip, 0));
    typeset (t[1], descend (ip, 1));
    marker (descend (ip, 1));
    //typeset_dynamic (t[1], descend (ip, 1));
  }
  else if ((which == "screen") || (which == "printer")) {
    bool pr= (which != "screen");
    box  sb= typeset_as_concat (env, attach_middle (t[1], ip));
    box  b = specific_box (decorate_middle (ip), sb, pr, env->fn);
    marker (descend (ip, 0));
    print (STD_ITEM, b);
    marker (descend (ip, 1));
  }
  else control ("specific", ip);
}

void
concater_rep::typeset_flag (tree t, path ip) {
  string name= env->exec_string (t[0]);
  string col = env->exec_string (t[1]);
  path sip= ip;
  if ((N(t) >= 3) && (!is_nil (env->macro_src))) {
    string var= env->exec_string (t[2]);
    sip= env->macro_src->item [var];
  }
  if (((N(t) == 2) || is_accessible (sip)) && (!env->read_only)) {
    marker (descend (ip, 0));
    flag_ok (name, ip, named_color (col));
    marker (descend (ip, 1));  
  }
}

/******************************************************************************
* Typesetting images
******************************************************************************/

bool
is_percentage (tree t, string s) {
  return
    is_atomic (t) &&
    ends (t->label, s) &&
    is_double (t->label (0, N (t->label) - 1));
}

bool
is_percentage (tree t) {
  return is_percentage (t, "%");
}

double
as_percentage (tree t) {
  return as_double (t->label (0, N (t->label) - 1)) / 100.0;
}

bool
is_magnification (string s) {
  double result;
  if (N(s) == 0) return false;
  for (int i=0; i<N(s); /*nop*/) {
    if (s[i]=='*') { i++; read_double (s, i, result); }
    else if (s[i]=='/') { i++; read_double (s, i, result); }
    else return false;
  }
  return true;
}

double
get_magnification (string s) {
  int i=0;
  double magn= 1.0, result;
  while (i<N(s)) {
    if (s[i]=='*') { i++; read_double (s, i, result); magn *= result; }
    else if (s[i]=='/') { i++; read_double (s, i, result); magn /= result; }
    else return magn;
  }
  return magn;
}

#define error_image(t) { \
  typeset_dynamic (tree (ERROR, "bad image", t), ip); \
  return; \
}

void
concater_rep::typeset_image (tree t, path ip) {
  // determine the image url
  if (N(t)!=7) error_image ("parameters");
  tree image_tree= env->exec (t[0]);
  url image= url_none ();
  if (is_atomic (image_tree)) {
    if (N(image_tree->label)==0)
      error_image (tree (WITH, "color", "red", "no image"));
    url im= image_tree->label;
    image= resolve (relative (env->base_file_name, im));
    if (is_none (image)) image= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";
  } else if (is_func (image_tree, TUPLE, 2) &&
	     is_func (image_tree[0], RAW_DATA, 1) &&
	     is_atomic (image_tree[0][0]) && is_atomic (image_tree[1])) {
    image= url_ramdisc (image_tree[0][0]->label) *
           url ("image." * image_tree[1]->label);
  } else error_image (image_tree);

  // determine the original size of the image
  int iw, ih;
  image_size (image, iw, ih);
  double pt= ((double) env->dpi*PIXEL) / 72.0;

  // determine the clipping rectangle
  tree tx1= env->exec (t[3]);
  tree ty1= env->exec (t[4]);
  tree tx2= env->exec (t[5]);
  tree ty2= env->exec (t[6]);
  if (is_compound (tx1)) error_image (tx1);
  if (is_compound (ty1)) error_image (ty1);
  if (is_compound (tx2)) error_image (tx2);
  if (is_compound (ty2)) error_image (ty2);

  string sx1= tx1->label;
  string sy1= ty1->label;
  string sx2= tx2->label;
  string sy2= ty2->label;
  if (sx1 != "" && ! env->is_length (sx1)) error_image (sx1);
  if (sy1 != "" && ! env->is_length (sy1)) error_image (sy1);
  if (sx2 != "" && ! env->is_length (sx2)) error_image (sx2);
  if (sy2 != "" && ! env->is_length (sy2)) error_image (sy2);

  int x1, y1, x2, y2;
  if (sx1 == "") x1= 0;
  else x1= ((SI) (((double) env->as_length (sx1)) / pt));
  if (sy1 == "") y1= 0;
  else y1= ((SI) (((double) env->as_length (sy1)) / pt));
  if (sx2 == "") x2= iw;
  else x2= ((SI) (((double) env->as_length (sx2)) / pt));
  if (sy2 == "") y2= ih;
  else y2= ((SI) (((double) env->as_length (sy2)) / pt));
  if ((x1>=x2) || (y1>=y2))
    error_image (tree (WITH, "color", "red", "null box"));

  double cx1= ((double) x1) / ((double) iw);
  double cy1= ((double) y1) / ((double) ih);
  double cx2= ((double) x2) / ((double) iw);
  double cy2= ((double) y2) / ((double) ih);

  // determine the width and the height
  tree tw= env->exec (t[1]);
  tree th= env->exec (t[2]);
  if (is_compound (tw)) error_image (tw);
  if (is_compound (th)) error_image (th);
  string ws= tw->label;
  string hs= th->label;
  if (! (N(ws)==0 || env->is_length (ws) || is_magnification (ws)))
    error_image (ws);
  if (! (N(hs)==0 || env->is_length (hs) || is_magnification (hs)))
    error_image (hs);
  
  SI w= 0, h= 0;
  bool ws_is_len= env->is_length (ws);
  bool hs_is_len= env->is_length (hs);    
  if (ws_is_len) w= env->as_length (ws);
  if (hs_is_len) h= env->as_length (hs);
  if (ws_is_len && !hs_is_len) h= (w * (y2-y1)) / (x2-x1);
  if (hs_is_len && !ws_is_len) w= (h * (x2-x1)) / (y2-y1);
  if (!ws_is_len && !hs_is_len) {
    w= (SI) (((double) (x2-x1)) * pt);
    h= (SI) (((double) (y2-y1)) * pt);
  }

  bool ws_is_mag= is_magnification (ws);
  bool hs_is_mag= is_magnification (hs);
  if (ws_is_mag) w= (SI) (((double) w) * get_magnification (ws));
  if (hs_is_mag) h= (SI) (((double) h) * get_magnification (hs));
  if (ws_is_mag && N(hs)==0) h= (SI) (((double) h) * get_magnification (ws));
  if (hs_is_mag && N(ws)==0) w= (SI) (((double) w) * get_magnification (hs));

  if (w <= 0 || h <= 0)
    error_image (tree (WITH, "color", "red", "null box"));

  // print the box
  print (STD_ITEM, image_box (ip, image, w, h, cx1, cy1, cx2, cy2));
}

#undef error_image
