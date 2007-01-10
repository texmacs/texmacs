
/******************************************************************************
* MODULE     : evaluate_misc.cpp
* DESCRIPTION: various other primitives for evaluation
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"
#include "vars.hpp"
#include "analyze.hpp"
#include "url.hpp"
#include "../../Typeset/Graphics/frame.hpp"

static hashmap<string,tree> local_ref ("?");
static hashmap<string,tree> global_ref ("?");

tree
evaluate_formatting (tree t, string v) {
  int n= N(t);
  tree oldv= std_env [v];
  tree newv= oldv * t (0, n-1);
  assoc_environment local (1);
  local->raw_write (0, v, newv);
  begin_with (std_env, local);
  tree r= evaluate (t[n-1]);
  end_with (std_env);
  return t (0, n-1) * tree (TFORMAT, r);
}

tree
evaluate_table (tree t) {
  // FIXME: we should execute values in old cell format
  assoc_environment local (1);
  local->raw_write (0, CELL_FORMAT, tree (TFORMAT));
  begin_with (std_env, local);
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++) r[i]= evaluate (t[i]);
  end_with (std_env);
  return r;
}

tree
evaluate_hard_id (tree t) {
  t= expand (t, true);
  pointer ptr= (pointer) t.operator -> ();
  return "%" * as_hexadecimal (ptr);
}

tree
evaluate_script (tree t) {
  if (N(t) != 1 && N(t) != 2) return tree (ERROR, "bad script");
  if (N(t) == 1) return tree (SCRIPT, evaluate (t[0]));
  else return tree (SCRIPT, evaluate (t[0]), expand (t[1], true));
}

tree
evaluate_set_binding (tree t) {
  tree keys, value;
  if (N(t) == 1) {
    keys= std_env ["the-tags"];
    if (!is_tuple (keys))
      return tree (ERROR, "bad set binding");
    for (int i=0; i<N(keys); i++)
      if (!is_atomic (keys[i]))
	return tree (ERROR, "bad set binding");
    value= evaluate (t[0]);
    assoc_environment local (2);
    local->raw_write (0, string ("the-tags"), tree (TUPLE));
    local->raw_write (1, string ("the-label"), copy (value));
    assign (std_env, local);
  }
  else if (N(t) >= 2) {
    tree key= evaluate (t[0]);
    if (!is_atomic (key)) 
      return tree (ERROR, "bad set binding");
    keys= tuple (key);
    value= evaluate (t[1]);
  }
  else return tree (ERROR, "bad set binding");

  for (int i=0; i<N(keys); i++) {
    string key= keys[i]->label;
    tree old_value= local_ref[key];
    string part= as_string (std_env ["current-part"]);
    url base_file_name (as_string (std_env ["base-file-name"]));
    url cur_file_name (as_string (std_env ["cur-file-name"]));
    if (is_func (old_value, TUPLE) && (N(old_value) >= 2))
      local_ref (key)= tuple (copy (value), old_value[1]);
    else local_ref (key)= tuple (copy (value), "?");
    if (cur_file_name != base_file_name || N(part) != 0) {
      string extra;
      if (cur_file_name != base_file_name)
	extra << as_string (delta (base_file_name, cur_file_name));
      if (N(part) != 0)
	extra << "#" << part (1, N(part));
      local_ref (key) << extra;
    }
    if (complete && is_tuple (old_value) && N(old_value) >= 1) {
      string old_s= var_as_string (old_value[0]);
      string new_s= var_as_string (value);
      if (new_s != old_s && !starts (key, "auto-")) {
	if (new_s == "") system_warning ("Redefined", key);
	else system_warning ("Redefined " * key * " as", new_s);
      }
    }
  }
  return keys;
}

tree
evaluate_get_binding (tree t) {
  if (N(t) != 1 && N(t) != 2) return tree (ERROR, "bad get binding");
  string key= as_string (evaluate (t[0]));
  tree value= local_ref->contains (key)? local_ref [key]: global_ref [key];
  int type= (N(t) == 1? 0: as_int (evaluate (t[1])));
  if (type != 0 && type != 1) type= 0;
  if (is_func (value, TUPLE) && (N(value) >= 2)) value= value[type];
  else if (type == 1) value= tree (UNINIT);
  if (complete && value == tree (UNINIT))
    system_warning ("Undefined reference", key);
  return value;
}

tree
evaluate_point (tree t) {
  int i, n= N(t);
  tree u (_POINT, n);
  for (i=0; i<n; i++)
    u[i]= evaluate (t[i]);
  if (n==0 || is_double (u[0])) return u;
  return as_tree (as_point (u));
}

/*
tree
evaluate_box_info (tree t) {
  tree t1= t[0];
  tree t2= t[1];
  if (!is_string (t2))
    return tree (ERROR, "bad box info");
  return box_info (edit_env (this), t1, as_string (t2));
}

tree
evaluate_frame_direct (tree t) {
  tree t1= evaluate (t[0]);
  return as_tree (!nil (fr) ? fr (::as_point (t1)) : point ());
}

tree
evaluate_frame_inverse (tree t) {
  tree t1= evaluate (t[0]);
  return as_tree (!nil (fr) ? fr [::as_point (t1)] : point ());
}
*/
