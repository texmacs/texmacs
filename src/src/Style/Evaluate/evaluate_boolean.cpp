
/******************************************************************************
* MODULE     : evaluate_boolean.cpp
* DESCRIPTION: boolean operations and common predicates
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "evaluate_main.hpp"

/******************************************************************************
* Boolean operations
******************************************************************************/

tree
evaluate_or (tree t) {
  if (N(t)<2) return evaluate_error ("bad or");
  for (int i=0; i<N(t); i++) {
    tree u= evaluate (t[i]);
    if (!is_bool (u)) return evaluate_error ("bad or");
    if (as_bool (u)) return as_string_bool (true);
  }
  return as_string_bool (false);
}

tree
evaluate_xor (tree t) {
  if (N(t)!=2) return evaluate_error ("bad xor");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (!is_bool (t1) || !is_bool (t2))
    return evaluate_error ("bad xor");
  return as_string_bool (as_bool (t1) ^ as_bool (t2));
}

tree
evaluate_and (tree t) {
  if (N(t)<2) return evaluate_error ("bad and");
  for (int i=0; i<N(t); i++) {
    tree u= evaluate (t[i]);
    if (!is_bool (u)) return evaluate_error ("bad and");
    if (!as_bool (u)) return as_string_bool (false);
  }
  return as_string_bool (true);
}

tree
evaluate_not (tree t) {
  if (N(t)!=1) return evaluate_error ("bad not");
  tree u= evaluate(t[0]);
  if (!is_bool (u)) return evaluate_error ("bad not");
  return as_string_bool (!as_bool (u));
}

/******************************************************************************
* Predicates
******************************************************************************/

tree
evaluate_equal (tree t) {
  if (N(t)!=2) return evaluate_error ("bad equal");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_atomic (t1) && is_atomic (t2) &&
      is_length (t1->label) && is_length (t2->label))
    return as_string_bool (as_length (t1) == as_length (t2));
  return as_string_bool (t1 == t2);
}

tree
evaluate_unequal (tree t) {
  if (N(t)!=2) return evaluate_error ("bad unequal");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_atomic (t1) && is_atomic (t2) &&
      is_length (t1->label) && is_length (t2->label))
    return as_string_bool (as_length (t1) != as_length (t2));
  return as_string_bool (t1 != t2);
}

tree
evaluate_less (tree t) {
  if (N(t)!=2) return evaluate_error ("bad less");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad less");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && is_double (s2))
    return as_string_bool (as_double (s1) < as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) < as_length (s2));
  return evaluate_error ("bad less");
}

tree
evaluate_lesseq (tree t) {
  if (N(t)!=2) return evaluate_error ("bad less or equal");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad less or equal");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) <= as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) <= as_length (s2));
  return evaluate_error ("bad less or equal");
}

tree
evaluate_greater (tree t) {
  if (N(t)!=2) return evaluate_error ("bad greater");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad greater");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) > as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) > as_length (s2));
  return evaluate_error ("bad greater");
}

tree
evaluate_greatereq (tree t) {
  if (N(t)!=2) return evaluate_error ("bad greater or equal");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad greater or equal");
  string s1= t1->label;
  string s2= t2->label;
  if (is_double (s1) && (is_double (s2)))
    return as_string_bool (as_double (s1) >= as_double (s2));
  if (is_length (s1) && is_length (s2))
    return as_string_bool (as_length (s1) >= as_length (s2));
  return evaluate_error ("bad greater or equal");
}
