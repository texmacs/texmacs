
/******************************************************************************
* MODULE     : cleanhtml.cpp
* DESCRIPTION: clean TeXmacs documents imported from HTML/MathML
* COPYRIGHT  : (C) 2019  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"

bool is_section (tree t); // from fromtex_post.cpp

/******************************************************************************
* Clean spaces
******************************************************************************/

static bool
is_whitespace (tree t) {
  return is_atomic (t) && is_whitespace (t->label);
}

static bool
is_space_eating (tree t) {
  return is_func (t, LABEL) || is_section (t);
}

static tree
clean_line (tree t) {
  int n= N(t);
  if (is_whitespace (t)) return "";
  if (is_concat (t) && n >= 2) {
    if (is_whitespace (t[0]) && is_space_eating (t[1]))
      return clean_line (t (1, n));
    if (is_whitespace (t[1]) && is_space_eating (t[0]))
      return clean_line (t (0, 1) * t (2, n));
    if (is_space_eating (t[0]) && is_atomic (t[1]) &&
        trim_spaces_left (t[1]) != t[1])
      return clean_line (t (0, 1) *
                         tree (CONCAT, trim_spaces_left (t[1])) *
                         t (2, n));
    if (is_whitespace (t[n-1]) && is_space_eating (t[n-2]))
      return clean_line (t (0, n-1));
    if (is_whitespace (t[n-2]) && is_space_eating (t[n-1]))
      return clean_line (t (0, n-2) * t (n-1, n));
    if (is_space_eating (t[n-1]) && is_atomic (t[n-2]) &&
        trim_spaces_right (t[n-2]) != t[n-2])
      return clean_line (t (0, n-2) *
                         tree (CONCAT, trim_spaces_right (t[n-2])) *
                         t (n-1, n));
    if (is_func (t[0], LABEL))
      return t (0, 1) * clean_line (t (1, n));
    else if (is_func (t[n-1], LABEL))
      return clean_line (t (0, n-1)) * t (n-1, n);
  }
  if (is_concat (t) && N(t) == 0) return "";
  if (is_concat (t) && N(t) == 1) return t[0];
  return t;
}

static tree
clean_spaces (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "code")) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      r[i]= clean_spaces (t[i]);
      if (is_func (t, DOCUMENT) || is_section (t))
        r[i]= clean_line (r[i]);
      if (is_section (t))
        r[i]= trim_spaces (r[i]);
    }
    return r;
  }
}

/******************************************************************************
* Compress lines with singleton labels
******************************************************************************/

static bool
is_compressible (tree t) {
  if (is_func (t, LABEL)) return true;
  if (!is_concat (t)) return false;
  for (int i=0; i<N(t); i++)
    if (!is_compressible (t[i])) return false;
  return true;
}

static bool
is_accepting (tree t) {
  return is_atomic (t) || is_concat (t) || is_space_eating (t);
}

static tree
merge_lines (tree t, tree u) {
  if (t == "") return u;
  if (u == "") return t;
  if (!is_concat (t)) t= tree (CONCAT, t);
  if (!is_concat (u)) u= tree (CONCAT, u);
  return t * u;
}

static tree
compress_lines (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "code")) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= compress_lines (t[i]);
    if (!is_func (r, DOCUMENT)) return r;

    t= r;
    r= tree (DOCUMENT);
    for (i=0; i<n; i++) {
      tree c= t[i];
      while (is_compressible (c) && i+1<n &&
             (t[i+1] == "" || is_compressible (t[i+1]))) {
        c= merge_lines (c, t[i+1]);
        i++;
      }
      if (is_compressible (c) && i+1<n && is_accepting (t[i+1])) {
        r << merge_lines (c, t[i+1]);
        i++;
      }
      else r << c;
    }
    return r;
  }
}

/******************************************************************************
* Interface
******************************************************************************/

tree
clean_html (tree t) {
  t= clean_spaces (t);
  t= compress_lines (t);
  return t;
}
