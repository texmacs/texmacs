
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

/******************************************************************************
* Predicates
******************************************************************************/

bool is_section (tree t); // from fromtex_post.cpp

static bool
is_whitespace (tree t) {
  return is_atomic (t) && is_whitespace (t->label);
}

static bool
is_item (tree t) {
  return is_compound (t, "item") || is_compound (t, "item*");
}

static bool
is_item_list (tree t) {
  return 
    is_compound (t, "itemize") ||
    is_compound (t, "enumerate") ||
    is_compound (t, "description");
}

/******************************************************************************
* Downgrading bad block content
******************************************************************************/

static tree
downgrade_block (tree t, bool compress= false) {
  if (is_atomic (t)) return t;
  else if (is_func (t, DOCUMENT, 0))
    return "";
  else if (is_func (t, DOCUMENT, 1))
    return downgrade_block (t[0], compress);
  else if (is_func (t, DOCUMENT) && compress && is_whitespace (t[0]))
    return downgrade_block (t (1, N(t)));
  else if (is_func (t, DOCUMENT) && compress && is_whitespace (t[N(t)-1]))
    return downgrade_block (t (0, N(t)-1));
  else if (is_func (t, DOCUMENT)) {
    int i, n= N(t);
    tree r (DOCUMENT);
    for (i=0; i<n; i++) {
      tree d= downgrade_block (t[i]);
      if (is_func (d, DOCUMENT)) r << A(d);
      else r << d;
    }
    return r;
  }
  else if (is_func (t, CONCAT)) {
    int i, n= N(t);
    tree d (DOCUMENT);
    tree c (CONCAT);
    for (i=0; i<n; i++) {
      tree x= downgrade_block (t[i]);
      if (is_func (x, DOCUMENT) || is_item_list (x)) {
        if (N(c) == 1) d << c[0];
        else if (N(c) > 1) d << c;
        c= tree (CONCAT);
        if (is_func (x, DOCUMENT)) d << A(x);
        else d << x;
      }
      else c << x;
    }
    if (N(c) == 1) d << c[0];
    else if (N(c) > 1) d << c;
    if (N(d) == 0) return "";
    else if (N(d) == 1) return d[0];
    else return d;
  }
  else if (is_func (t, HLINK, 2))
    return tree (HLINK, downgrade_block (t[0], true), t[1]);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= downgrade_block (t[i]);
    return r;
  }
}

/******************************************************************************
* Clean spaces
******************************************************************************/

static bool
is_space_devouring (tree t) {
  if (is_func (t, HLINK, 2)) return is_whitespace (t[0]);
  return is_func (t, LABEL);
}

static bool
is_space_eating (tree t) {
  return is_space_devouring (t) || is_section (t);
}

static tree
clean_line (tree t) {
  int n= N(t);
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
    if (is_space_devouring (t[0]))
      return t (0, 1) * clean_line (t (1, n));
    else if (is_space_devouring (t[n-1]))
      return clean_line (t (0, n-1)) * t (n-1, n);
    for (int i=0; i+1<n; i++)
      if (is_func (t[i], RSUP, 1) && is_func (t[i+1], RSUP, 1)) {
        tree x= simplify_concat (tree (CONCAT, t[i][0], ",", t[i+1][0]));
        tree c= t (0, i) * tree (CONCAT, tree (RSUP, x)) * t (i+2, N(t));
        return clean_line (c);
      }
  }
  return t;
}

static tree
clean_line_master (tree t) {
  if (is_whitespace (t)) return "";
  t= clean_line (t);
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
        r[i]= clean_line_master (r[i]);
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
is_compressible (tree t, bool item_flag) {
  if (is_space_devouring (t)) return true;
  if (!is_concat (t)) return false;
  for (int i=0; i<N(t); i++) {
    if (item_flag && is_item (t[i])) continue;
    if (!is_compressible (t[i], item_flag)) return false;
  }
  return true;
}

static bool
is_accepting (tree t) {
  if (is_func (t, HLINK, 2)) return is_accepting (t[0]);
  return is_atomic (t) || is_concat (t) || is_space_eating (t);
}

static bool
is_item_line (tree t) {
  if (is_item (t)) return true;
  if (!is_concat (t)) return false;
  for (int i=0; i<N(t); i++)
    if (is_item_line (t[i])) return true;
  return false;
}

static tree
merge_lines (tree t, tree u) {
  if (t == "") return u;
  if (u == "") return t;
  u= trim_spaces_left (u);
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
      while (is_compressible (c, true) && i+1<n &&
             (t[i+1] == "" || is_compressible (t[i+1], false))) {
        c= merge_lines (c, t[i+1]);
        i++;
      }
      if (is_compressible (c, true) && i+1<n && is_accepting (t[i+1]) &&
          !(is_item_line (c) && is_item_line (t[i+1]))) {
        r << merge_lines (c, t[i+1]);
        i++;
      }
      else if (is_whitespace (c) && i+1<n && is_item_list (t[i+1]));
      else r << c;
    }
    return r;
  }
}

/******************************************************************************
* Clean MathML
******************************************************************************/

static bool
accepts_limits (tree t) {
  if (is_func (t, BIG)) return true;
  if (!is_atomic (t)) return false;
  string s= t->label;
  if (starts (s, "<big-") && ends (s, ">") && !is_digit (s[N(s)-2]))
    return true;
  return s == "lim" || s == "inf" || s == "sup" || s == "max" || s == "min";
}

static tree
clean_op (tree t) {
  if (!is_atomic (t)) return t;
  string s= t->label;
  if (starts (s, "<big-") && ends (s, ">") && !is_digit (s[N(s)-2]))
    return tree (BIG, s (5, N(s)-1));
  return t;
}

static tree
clean_mathml (tree t) {
  if (is_atomic (t)) {
    string s= t->label;
    if (!ends (s, ">")) return t;
    int pos= N(s);
    tm_char_backwards (s, pos);
    if (pos >= 0 && pos<N(s) &&
        starts (s (pos, N(s)), "<big-") &&
        !is_digit (s[N(s)-2])) {
      if (pos == 0) return tree (BIG, s (5, N(s)-1));
      else return tree (CONCAT, s (0, pos), tree (BIG, s (pos+5, N(s)-1)));
    }
    return t;
  }
  
  if (is_func (t, ABOVE, 2)) {
    if (is_func (t[0], BELOW, 2) && accepts_limits (t[0][0]))
      return tree (CONCAT,
                   clean_op (clean_mathml (t[0][0])),
                   tree (RSUB, clean_mathml (t[0][1])),
                   tree (RSUP, clean_mathml (t[1])));
    else if (accepts_limits (t[0]))
      return tree (CONCAT,
                   clean_op (clean_mathml (t[0])),
                   tree (RSUP, clean_mathml (t[1])));
  }
  if (is_func (t, BELOW, 2)) {
    if (is_func (t[0], ABOVE, 2) && accepts_limits (t[0][0]))
      return tree (CONCAT,
                   clean_op (clean_mathml (t[0][0])),
                   tree (RSUB, clean_mathml (t[1])),
                   tree (RSUP, clean_mathml (t[0][1])));
    else if (accepts_limits (t[0]))
      return tree (CONCAT,
                   clean_op (clean_mathml (t[0])),
                   tree (RSUB, clean_mathml (t[1])));
  }
  
  int n= N(t);
  tree r (t, n);
  for (int i=0; i<n; i++)
    r[i]= clean_mathml (t[i]);
  if (is_func (r, ABOVE, 2)) {
    if (r[1] == "\2") return tree (WIDE, r[0], "^");
    if (r[1] == "\3") return tree (WIDE, r[0], "~");
    if (r[1] == "\4") return tree (WIDE, r[0], "<ddot>");
    if (r[1] == "\7") return tree (WIDE, r[0], "<check>");
    if (r[1] == "\10") return tree (WIDE, r[0], "<breve>");
    if (r[1] == "\11") return tree (WIDE, r[0], "<bar>");
    if (r[1] == "\12") return tree (WIDE, r[0], "<dot>");
    if (r[1] == "^") return tree (WIDE, r[0], "^");
    if (r[1] == "~") return tree (WIDE, r[0], "~");
  }
  if (is_func (r, CONCAT)) {
    t= r;
    r= tree (CONCAT);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], CONCAT)) r << A(t[i]);
      else r << t[i];
    return r;
  }
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
clean_html (tree t) {
  t= downgrade_block (t);
  t= clean_spaces (t);
  t= compress_lines (t);
  t= clean_mathml (t);
  return t;
}
