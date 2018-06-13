
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
#include "convert.hpp"
#include "converter.hpp"

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
  if (N(t) != 2) { typeset_error (t, ip); return; }
  tree flag= env->exec (t[0]);
  box  b   = typeset_as_concat (env, attach_right (t[1], ip));
  marker (descend (ip, 0));
  if (flag == "true") print (b);
  else print (empty_box (b->ip, b->x1, b->y1, b->x2, b->y2));
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
build_locus (edit_env env, tree t, list<string>& ids, string& col, string &ref, string &anchor) {
  //cout << "Typeset " << t << "\n";
  int last= N(t)-1;
  tree body= env->expand (t[last], true);
  //cout << "Typeset " << body << "\n";
  bool accessible= is_accessible (obtain_ip (body));
  bool visited= false;
  ref= "";
  anchor= "";

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
      else if (is_compound (arg, "link") && N(arg) >= 2) {
	if (is_func (arg[1], ATTR)) arg= copy (arg);
	else arg= arg (0, 1) * tree (LINK, tree (ATTR)) * arg (1, N(arg));
	arg[1] << tree ("secure")
	       << (env->secure? tree ("true"): tree ("false"));
	env->link_env->insert_link (arg);
	for (j=2; j<N(arg); j++) {
	  if (is_compound (arg[j], "id", 1) && is_atomic (arg[j][0])) {
	    visited= visited || has_been_visited ("id:" * arg[j][0]->label);
	    anchor = arg[j][0]->label;
	  }
	  if (is_compound (arg[j], "url", 1) && is_atomic (arg[j][0])) {
	    visited= visited || has_been_visited ("url:" * arg[j][0]->label);
	    ref = arg[j][0]->label;
	  }
	}
      }
      else if (is_compound (arg, "observer", 2)) {
	string id= as_string (arg[0]);
	string cb= cork_to_utf8 (as_string (arg[1]));
	if (accessible) {
          if (env->secure ||
              as_bool (eval ("(secure? '(" * cb * " #f #f #f))")))
            env->link_env->insert_locus (id, body, cb);
        }
	ids= list<string> (id, ids);
	visited= visited || has_been_visited ("id:" * id);
      }
    }
  }

  bool on_paper= (env->get_string (PAGE_PRINTED) == "true");
  bool preserve= (get_locus_rendering ("locus-on-paper") == "preserve");
  string var= (visited? VISITED_COLOR: LOCUS_COLOR);
  string current_col= env->get_string (COLOR);
  string locus_col= env->get_string (var);
  if (on_paper) visited= false;
  if (locus_col == "preserve") col= current_col;
  else if (on_paper && preserve) col= current_col;
  else if (locus_col == "global") col= get_locus_rendering (var);
  else col= locus_col;

  return accessible;
}

bool
build_locus (edit_env env, tree t, list<string>& ids, string& col) {
  string ref;
  string anchor;
  return build_locus(env, t, ids, col, ref, anchor);
}

void
concater_rep::typeset_locus (tree t, path ip) {
  string ref;
  string anchor;

  if (N(t) == 0) { typeset_error (t, ip); return; }
  int last= N(t)-1;
  list<string> ids;
  string col;
  if (build_locus (env, t, ids, col, ref, anchor)) {
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
    print (locus_box (ip, b, ids, env->pixel, ref, anchor));
  }
}

void
concater_rep::typeset_set_binding (tree t, path ip) {
  tree keys= env->exec (t);
  if (L(keys) == HIDDEN) {
    keys= keys[0];
    flag ("set binding", ip, blue);
    if (N(keys) > 0) {
      path sip= ip;
      if (N(t) >= 3 && (!is_nil (env->macro_src))) {
	tree body= env->expand (tree (ARG, t[2]), true);
	sip= obtain_ip (body);
      }
      path dip= decorate_middle (sip);
      box b= tag_box (dip, sip, empty_box (dip, 0, 0, 0, env->fn->yx), keys);
      a << line_item (CONTROL_ITEM, OP_SKIP, b, HYPH_INVALID, "label");
    }
  }
  else typeset_dynamic (keys, ip);
}

static tree
remove_labels (tree t) {
  if (is_atomic (t)) return copy (t);
  else if (is_func (t, LABEL)) return "";
  else if (is_func (t, CONCAT)) {
    tree r (CONCAT);
    for (int i=0; i<N(t); i++)
      if (!is_func (t, LABEL))
        r << remove_labels (t[i]);
    if (N(r) == 0) return "";
    else if (N(r) == 1) return r[0];
    else return r;
  }
  else {
    tree r (t, N(t));
    for (int i=0; i<N(t); i++)
      r[i]= remove_labels (t[i]);
    return r;
  }
}

void
concater_rep::typeset_write (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  string s= env->exec_string (t[0]);
  tree   r= remove_labels (env->exec (t[1]));
  if (env->complete) {
    if (!env->local_aux->contains (s))
      env->local_aux (s)= tree (DOCUMENT);
    env->local_aux (s) << r;
  } 
  control ("write", ip);
}

void
concater_rep::typeset_toc_notify (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  string kind = tree_to_verbatim (env->exec (t[0]), false, "cork");
  string title= tree_to_verbatim (env->exec (t[1]), false, "cork");
  title= replace (title, "T_EX_MACS", "TeXmacs");
  title= replace (title, "L^AT_EX", "LaTeX");
  title= replace (title, "T_EX", "TeX");
  box  b = toc_box (decorate_middle (ip), kind, title, env->fn);
  marker (descend (ip, 0));
  print (b);
  marker (descend (ip, 1));  
}

/******************************************************************************
* Typesetting other dynamic markup
******************************************************************************/

void
concater_rep::typeset_specific (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  string which= env->exec_string (t[0]);
  if (which == "texmacs" || which == "image") {
    marker (descend (ip, 0));
    typeset (t[1], descend (ip, 1));
    marker (descend (ip, 1));
    //typeset_dynamic (t[1], descend (ip, 1));
  }
  else if (which == "screen" || which == "printer" ||
           which == "even" || which == "odd") {
    box  sb= typeset_as_concat (env, attach_middle (t[1], ip));
    box  b = specific_box (decorate_middle (ip), sb, which, env->fn);
    marker (descend (ip, 0));
    print (b);
    marker (descend (ip, 1));
  }
  else control ("specific", ip);
}

void
concater_rep::typeset_flag (tree t, path ip) {
  if (N(t) != 2 && N(t) != 3) { typeset_error (t, ip); return; }
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

void
concater_rep::typeset_hyphenate_as (tree t, path ip) {
  if (N(t) != 1 && N(t) != 2) { typeset_error (t, ip); return; }
  tree pat= env->exec (t[0]);
  if (N(t) == 1 && !is_atomic (pat)) { typeset_error (t, ip); return; }
  language old_lan= env->lan;
  env->lan= ad_hoc_language (env->lan, pat);
  marker (descend (ip, 0));
  if (N(t) == 1) typeset (replace (pat->label, "-", ""), decorate_middle (ip));
  else typeset (t[1], descend (ip, 1));
  marker (descend (ip, 1));
  env->lan= old_lan;
}

/******************************************************************************
* Typesetting images
******************************************************************************/

#define error_image(t) { \
  typeset_dynamic (tree (ERROR, "bad image", t), ip); \
  return; \
}

void
concater_rep::typeset_image (tree t, path ip) {
  // determine the image url
  if (N(t) != 5) error_image ("parameters");
  tree image_tree= env->exec (t[0]);
  url image= url_none ();
  if (is_atomic (image_tree)) {
    if (N (image_tree->label) == 0)
      error_image (tree (WITH, "color", "red", "no image"));
    url im= cork_to_os8bits (image_tree->label);
    image= resolve (relative (env->base_file_name, im));
    if (is_none (image) && suffix (im) == "")
      image= resolve (relative (env->base_file_name, ::glue (im, ".eps")));
    if (is_none (image) && suffix (im) == "")
      image= resolve (relative (env->base_file_name, ::glue (im, ".pdf")));
    if (is_none (image)) image= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";
  }
  else if (is_func (image_tree, TUPLE, 2) &&
	     is_func (image_tree[0], RAW_DATA, 1) &&
	     is_atomic (image_tree[0][0]) && is_atomic (image_tree[1])) {
    image= url_ramdisc (image_tree[0][0]->label) *
           url ("image." * image_tree[1]->label);
  }
  else error_image (image_tree);

  // determine the original size of the image
  int iw, ih;
  image_size (image, iw, ih);
  double pt= ((double) env->dpi*PIXEL) / 72.0;
  SI w= (SI) (((double) iw) * pt);
  SI h= (SI) (((double) ih) * pt);

  // determine the width and the height
  tree old_w= env->local_begin ("w-length", as_string (w) * "tmpt");
  tree old_h= env->local_begin ("h-length", as_string (h) * "tmpt");
  SI imw= (t[1] == ""? w: env->as_length (env->exec (t[1]), "w"));
  SI imh= (t[2] == ""? h: env->as_length (env->exec (t[2]), "h"));
  if (t[1] == "" && t[2] != "" && ih != 0)
    imw= (SI) ((iw * ((double) imh)) / ih);
  if (t[1] != "" && t[2] == "" && iw != 0)
    imh= (SI) ((ih * ((double) imw)) / iw);
  if (imw <= 0 || imh <= 0)
    error_image (tree (WITH, "color", "red", "null box"));
  env->local_end ("w-length", old_w);
  env->local_end ("h-length", old_h);
  
  // determine the offset
  old_w= env->local_begin ("w-length", as_string (imw) * "tmpt");
  old_h= env->local_begin ("h-length", as_string (imh) * "tmpt");
  SI imx= (t[3] == ""? 0: env->as_length (env->exec (t[3]), "w"));
  SI imy= (t[4] == ""? 0: env->as_length (env->exec (t[4]), "h"));
  env->local_end ("w-length", old_w);
  env->local_end ("h-length", old_h);
  
  // print the box
  box imb= image_box (ip, image, imw, imh, env->alpha, env->pixel);
  print (move_box (ip, imb, imx, imy, true));
}

#undef error_image
