
/******************************************************************************
* MODULE     : concat_active.cpp
* DESCRIPTION: Typeset active markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "concater.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "scheme.hpp"

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
* Typesetting other dynamic markup
******************************************************************************/

class guile_command_rep: public command_rep {
  string s;
  int    level;
public:
  guile_command_rep (string s2, int level2= 0): s (s2), level (level2) {}
  void apply () {
    string lambda;
    if (as_bool (eval ("(secure? '" * s * ")"))) level= 2;
    switch (max (script_status, level)) {
    case 0:
      eval_delayed("(set-message \"Error: scripts not accepted\" \"script\")");
      break;
    case 1:
      lambda= "(lambda (s) (if (equal? s \"y\") " * s * "))";
      eval_delayed ("(interactive " * lambda * "\"Accept script (y/n)?\")");
      break;
    case 2:
      eval_delayed (s);
      break;
    }
  }
};

void
concater_rep::typeset_specific (tree t, path ip) {
  string which= env->exec_string (t[0]);
  if (which == "texmacs") {
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
concater_rep::typeset_label (tree t, path ip) {
  string key  = env->exec_string (t[0]);
  tree   value= copy (env->read ("the-label"));
  tree   old_value= env->local_ref[key];
  if (is_func (old_value, TUPLE) && (N(old_value) >= 2))
    env->local_ref (key)= tuple (copy (value), old_value[1]);
  else env->local_ref (key)= tuple (copy (value), "?");
  if (env->cur_file_name != env->base_file_name) {
    url d= delta (env->base_file_name, env->cur_file_name);
    env->local_ref (key) << as_string (d);
  }

  flag (key, ip, env->dis->blue);
  // replacement for: control ("label", ip);
  box b= tag_box (ip, empty_box (ip, 0, 0, 0, env->fn->yx), key);
  a << line_item (CONTROL_ITEM, b, HYPH_INVALID, "label");
}

void
concater_rep::typeset_reference (tree t, path ip, int type) {
  marker (descend (ip, 0));
  string key= env->exec_string (t[0]);
  tree value=
    env->local_ref->contains (key)?
    env->local_ref [key]: env->global_ref [key];

  string s= "(go-to-label \"" * key * "\")";
  if (is_func (value, TUPLE, 3)) {
    url name= url_system (value[2]->label);
    string r= scm_quote (as_string (relative (env->base_file_name, name)));
    s= "(begin (load-browse-buffer (url-system " * r * ")) " * s * ")";
  }
  if (is_func (value, TUPLE) && (N(value) >= 2)) value= value[type];
  else if (type == 1) value= "?";

  command cmd (new guile_command_rep (s, 2));
  box b= typeset_as_concat (env, attach_right (value, ip));
  string action= env->read_only? string ("select"): string ("double-click");
  print (STD_ITEM, action_box (ip, b, action, cmd, true));
  marker (descend (ip, 1));  
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

void
concater_rep::typeset_hyperlink (tree t, path ip) {
  if (N(t)<2) return;
  string href      = env->exec_string (t[1]);
  string href_file = href;
  string href_label= "";
  int i, n= N(href);
  for (i=0; i<n; i++)
    if (href[i]=='#') {
      href_file = href (0, i);
      href_label= href (i+1, n);
      break;
    }

  string r, s;
  if (href_file == "") s= "(go-to-label \"" * href_label * "\")";
  else {
    r= as_string (relative (env->base_file_name, url_unix (href_file)));
    s= "(load-browse-buffer (url-system " * scm_quote (r) * "))";
    if (href_label != "")
      s= "(begin " * s * " (go-to-label \"" * href_label * "\"))";
  }
  command cmd (new guile_command_rep (s, 2));
  tree old_col= env->local_begin (COLOR, "blue");
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  env->local_end (COLOR, old_col);
  string action= env->read_only? string ("select"): string ("double-click");
  print (STD_ITEM, action_box (ip, b, action, cmd, true));
}

void
concater_rep::typeset_action (tree t, path ip) {
  if (N(t)<2) return;
  string cmd_s= env->exec_string (t[1]);
  command cmd (new guile_command_rep (cmd_s));
  tree old_col= env->local_begin (COLOR, "blue");
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  env->local_end (COLOR, old_col);
  path valip= decorate ();
  if ((N(t) >= 3) && (is_func (t[2], ARG)) && !nil (env->macro_arg)) {
    string var= env->exec_string (t[2][0]);
    tree   val= env->macro_arg->item [var];
    if ((var != "") && (!is_func (val, BACKUP))) {
      path new_valip= env->macro_src->item [var];
      if (is_accessible (new_valip)) valip= new_valip;
    }
  }
  string action= env->read_only? string ("select"): string ("double-click");
  print (STD_ITEM, action_box (ip, b, action, cmd, true, valip));
}

void
concater_rep::typeset_tag (tree t, path ip) {
  marker (descend (ip, 0));
  typeset (t[0], descend (ip, 0));
  marker (descend (ip, 1));  
}

void
concater_rep::typeset_meaning (tree t, path ip) {
  marker (descend (ip, 0));
  typeset (t[0], descend (ip, 0));
  marker (descend (ip, 1));  
}

void
concater_rep::typeset_flag (tree t, path ip) {
  string name= env->exec_string (t[0]);
  string col = env->exec_string (t[1]);
  path sip= ip;
  if ((N(t) >= 3) && (!nil (env->macro_src))) {
    string var= env->exec_string (t[2]);
    sip= env->macro_src->item [var];
  }
  if (((N(t) == 2) || is_accessible (sip)) && (!env->read_only)) {
    marker (descend (ip, 0));
    flag_ok (name, ip, env->dis->get_color (col));
    marker (descend (ip, 1));  
  }
}

/******************************************************************************
* Typesetting postscript images
******************************************************************************/

static bool
is_magnification (string s) {
  double result;
  for (int i=0; i<N(s); /*nop*/) {
    if (s[i]=='*') { i++; read_double (s, i, result); }
    else if (s[i]=='/') { i++; read_double (s, i, result); }
    else return false;
  }
  return true;
}

static double
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

#define error_postscript(t) { \
  typeset_dynamic (tree (ERROR, "bad postscript", t), ip); \
  return; \
}

void
concater_rep::typeset_postscript (tree t, path ip) {
  // determine the image url
  if (N(t)!=7) error_postscript ("parameters");
  tree image_tree= env->exec (t[0]);
  url image= url_none ();
  if (is_atomic (image_tree)) {
    if (N(image_tree->label)==0)
      error_postscript (tree (WITH, "color", "red", "no image"));
    url im= image_tree->label;
    image= resolve (relative (env->base_file_name, im));
    if (is_none (image)) image= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";
  } else if (is_func (image_tree, TUPLE, 2) &&
	     is_func (image_tree[0], RAW_DATA, 1) &&
	     is_atomic (image_tree[0][0]) && is_atomic (image_tree[1])) {
    image= url_ramdisc (image_tree[0][0]->label) *
           url ("image." * image_tree[1]->label);
  } else error_postscript (image_tree);

  // determine the original size of the image
  int iw, ih;
  image_size (image, iw, ih);
  double pt= ((double) env->dpi*PIXEL) / 72.0;

  // determine the clipping rectangle
  tree tx1= env->exec (t[3]);
  tree ty1= env->exec (t[4]);
  tree tx2= env->exec (t[5]);
  tree ty2= env->exec (t[6]);
  if (is_compound (tx1)) error_postscript (tx1);
  if (is_compound (ty1)) error_postscript (ty1);
  if (is_compound (tx2)) error_postscript (tx2);
  if (is_compound (ty2)) error_postscript (ty2);

  string sx1= tx1->label;
  string sy1= ty1->label;
  string sx2= tx2->label;
  string sy2= ty2->label;
  if (sx1 != "" && ! env->is_length (sx1)) error_postscript (sx1);
  if (sy1 != "" && ! env->is_length (sy1)) error_postscript (sy1);
  if (sx2 != "" && ! env->is_length (sx2)) error_postscript (sx2);
  if (sy2 != "" && ! env->is_length (sy2)) error_postscript (sy2);

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
    error_postscript (tree (WITH, "color", "red", "null box"));

  double cx1= ((double) x1) / ((double) iw);
  double cy1= ((double) y1) / ((double) ih);
  double cx2= ((double) x2) / ((double) iw);
  double cy2= ((double) y2) / ((double) ih);

  // determine the width and the height
  tree tw= env->exec (t[1]);
  tree th= env->exec (t[2]);
  if (is_compound (tw)) error_postscript (tw);
  if (is_compound (th)) error_postscript (th);
  string ws= tw->label;
  string hs= th->label;
  if (! (N(ws)==0 || env->is_length (ws) || is_magnification (ws)))
    error_postscript (ws);
  if (! (N(hs)==0 || env->is_length (hs) || is_magnification (hs)))
    error_postscript (hs);
  
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
    error_postscript (tree (WITH, "color", "red", "null box"));

  // print the box
  print (STD_ITEM, image_box (ip, image, w, h, cx1, cy1, cx2, cy2));
}

#undef error_postscript
