
/******************************************************************************
* MODULE     : bibtex_functions.cpp
* DESCRIPTION: BiBTeX internal functions
* COPYRIGHT  : (C) 2010, 2015  David MICHEL, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bibtex_functions.hpp"
#include "converter.hpp"
#include "vars.hpp"

/******************************************************************************
* Helper functions
******************************************************************************/

static string
bib_parse_char (string s, int& pos, int& depth, bool& special, int& math) {
  switch (s[pos]) {
    case '{': {
      pos++;
      if (pos < N(s) && s[pos] == '\\' && depth == 0) special= true;
      depth++;
      return "{";
    }
    case '}': {
      pos++;
      depth--;
      if (special && depth == 0) special= false;
      return "}";
    }
    case '\\': {
      string r= "\\";
      pos++;
      if (pos < N(s) && (s[pos] == '{' || s[pos] == '}')) {
        r << s[pos];
        pos++;
      }
      else /* if (special) */ {
        if (pos < N(s) && !is_alpha (s[pos]) && !is_digit (s[pos])) {
          r << s[pos];
          pos++;
        }
        else {
          while (pos < N(s) && (is_alpha (s[pos]) || is_digit (s[pos]))) {
            r << s[pos];
            pos++;
          }
        }
        if (pos < N(s) && (is_alpha (s[pos]) || is_digit (s[pos]))) {
          r << s[pos];
          pos++;
        }
        else if (pos < N(s) && s[pos] == '{') {
          int d= 1;
          r << s[pos];
          pos++;
          while (pos < N(s) && d > 0) {
            if (s[pos] == '{') d++;
            if (s[pos] == '}') d--;
            r << s[pos];
            pos++;
          }
        }
      }
      return r;
    }
    case '$': {
      pos++;
      if (pos < N(s) && s[pos] == '$') {
        if (math == 2) math= 0;
        else math= 2;
        pos++;
        return "$$";
      }
      else {
        if (math == 1) math= 0;
        else math= 1;
        return "$";
      }
    }
    default: {
      string r;
      r << s[pos];
      pos++;
      return r;
    }
  }
}

static string
bib_get_char (string s, int pos) {
  int d= 0;
  bool sp= false;
  int m= 0;
  int p= pos;
  return bib_parse_char (s, p, d, sp, m);
}

static bool
bib_is_normal (string s) {
  return (N(s) == 1) && (s[0] != '{') && (s[0] != '}') &&
         (s[0] != '\\') && !is_space (s[0]);
}

static bool
bib_is_space (string s) {
  return (N(s) == 1) && is_space (s[0]);
}

static bool
bib_keepcase_sep (string s) {
  return (N(s) == 1) && (is_space (s[0]) || s[0] == '[' || s[0] == ']');
}

/*
static bool
bib_is_iso_alpha (string s) {
  return (N(s) == 1) && is_iso_alpha (s[0]);
}

static bool
bib_is_digit (string s) {
  return (N(s) == 1) && is_digit (s[0]);
}
*/

static bool
bib_is_char (string s, char c) {
  return (N(s) == 1) && (s[0] == c);
}
 
static string
bib_to_latex (string s) {
  string r= "{";
  int pos= 0;
  int depth= 0;
  bool special= false;
  bool specialsav= special;
  int math= 0;
  int keepcase= -1;
  while (pos < N(s)) {
    specialsav= special;
    string ch= bib_parse_char (s, pos, depth, special, math);
    // convert_warning << ch << " " << as_string (keepcase) << " " << as_string (depth) << "\n";
    if (ch == "$$") r << "$";
    else if (special || math) r << ch;
    else if (bib_keepcase_sep (ch)) {
      if (bib_is_space (ch)) ch= " ";
      if (ch == "[" || ch == "]") ch= "{" * ch * "}";
      if (keepcase == depth+1) {
        r << "}" << ch;
        keepcase= -1;
      }
      else r << ch;
    }
    else if (ch == "%") r << "\\%";
    else if (bib_is_char (ch, '{') && depth > 0 &&
             bib_get_char (s, pos) != "\\") {
      if (keepcase == -1) {
        r << "\\keepcase{";
        keepcase= depth;
      }
      else if (keepcase < depth) r << "{";
    }
    else if (bib_is_char (ch, '}')) {
      if (keepcase <= depth || specialsav) r << ch;
    }
    else r << ch;
  }
  for (int i= keepcase; i>0; i--) r << "}";
  r << "}";
  // cout << "<<< " << s << "\n";
  // cout << ">>> " << r << "\n";
  // convert_warning << r << "\n";
  return r;
}

/******************************************************************************
* BibTeX add.period$
******************************************************************************/

bool
bib_is_bad (tree t) {
  if (is_atomic(t)) {
    string s= t->label;
    for (int i= N(s)-1; i >= 0; i--)
      if (!is_space (s[i])) return false;
    return true;
  }
  else return (N(t) == 0);
}

char*
bib_last_char (tree t) {
  if (is_atomic (t)) {
    string s= t->label;
    int end= N(s)-1;
    while ((end >= 0) && is_space (s[end])) end--;
    if (end >= 0) return &(s[end]);
    else return 0;
  }
  else {
    int pos= N(t)-1;
    while ((pos >= 0) && bib_is_bad (t[pos])) pos--;
    if (pos >= 0) return bib_last_char (t[pos]);
    else return 0;
  }
}

scheme_tree
bib_add_period (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  char* ch= bib_last_char (t);
  if (ch == 0) return tree_to_scheme_tree (t);
  if (*ch == ',' || *ch == ';') {
    *ch= '.';
    return tree_to_scheme_tree (t);
  }
  else if (*ch != '!' && *ch != '?' && *ch != '.') {
    tree res (CONCAT);
    res << t;
    res << ".";
    return tree_to_scheme_tree (res);
  }
  else return tree_to_scheme_tree (t);
}

/******************************************************************************
* Change case of the first letter
******************************************************************************/

bool
bib_change_first (tree& t, string (*change_fun) (string)) {
  if (is_atomic (t)) {
    string s= t->label;
    int beg= 0;
    while ((beg < N(s)) && is_space (s[beg])) beg++;
    if (beg >= N(s)) return false;
    t= s (0, beg) * change_fun (s (beg, N(s)));
    return true;
  }
  else if (is_compound (t, "verbatim"))
    return false;
  else if (is_compound (t, "slink"))
    return false;
  else if (is_func (t, WITH, 3) && t[0] == FONT_FAMILY && t[1] == "tt")
    return false;
  else if (is_func (t, WITH, 3) && t[0] == MATH_FONT)
    return false;
  else {
    int pos= 0;
    if (L(t) == WITH) pos= N(t)-1;
    else while ((pos < N(t)) && bib_is_bad (t[pos])) pos++;
    if (pos < N(t)) return bib_change_first (t[pos], change_fun);
    else return false;
  }
}

scheme_tree
bib_locase_first (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  bib_change_first (t, uni_locase_first);
  return tree_to_scheme_tree (t); 
}

scheme_tree
bib_upcase_first (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  bib_change_first (t, uni_upcase_first);
  return tree_to_scheme_tree (t); 
}

/******************************************************************************
* BibTeX change.case$
******************************************************************************/

/*
static string
bib_change_case_aux (string s, bool keep_first, char (*change_case) (char)) {
  string r;
  int pos= 0;
  int depth= 0;
  bool special= false;
  bool first= true;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special);
    if (bib_is_normal (ch)) {
      if ((keep_first && first) || (!special && depth > 0)) r << ch[0];
      else r << change_case (ch[0]);
      first= false;
    }
    else r << ch;
  }
  return r;
}

string
bib_change_case (string s, string op) {
  if (op == "t") return bib_change_case_aux (s, true, locase);
  else if (op == "l") return bib_change_case_aux (s, false, locase);
  else if (op == "u") return bib_change_case_aux (s, false, upcase);
  else return copy (s);
}
*/

void
bib_change_case (tree& t, string (*change_case) (string)) {
  //cout << "Change case " << t << "\n";
  if (is_atomic (t) && change_case) t->label= change_case (t->label);
  else if (is_compound (t, "verbatim"));
  else if (is_compound (t, "slink"));
  else if (is_func (t, WITH, 3) && t[0] == FONT_FAMILY && t[1] == "tt");
  else if (is_func (t, WITH, 3) && t[0] == MATH_FONT);
  else if (L(t) == WITH) bib_change_case (t[N(t)-1], change_case);
  else if (L(t) == as_tree_label ("keepcase")) t= t[0];
  else if (L(t) == CONCAT || L(t) == DOCUMENT)
    for (int i= 0; i<N(t); i++) bib_change_case (t[i], change_case);
}

scheme_tree
bib_locase (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  bib_change_case (t, uni_locase_all);
  return tree_to_scheme_tree (t); 
}

scheme_tree
bib_upcase (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  bib_change_case (t, uni_upcase_all);
  return tree_to_scheme_tree (t); 
}

scheme_tree
bib_default_preserve_case (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  return tree_to_scheme_tree (t); 
}

scheme_tree
bib_default_upcase_first (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  bib_change_case (t, 0);
  return tree_to_scheme_tree (t); 
}

/******************************************************************************
* BibTeX num.names$
******************************************************************************/

static bool
search_and_keyword (string s, int& pos) {
  int depth= 0;
  bool special= false;
  int math= 0;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special, math);
    if (bib_is_space (ch) && depth == 0) {
      while (pos < N(s) && is_space (s[pos])) pos++;
      if (pos < N(s)-3 && s (pos, pos+3) == "and" && is_space (s[pos+3])) {
        pos += 4;
        return true;
      }
    }
  }
  return false;
}

int
bib_num_names (string s) {
  int pos= 0;
  int nband= 0;
  while (pos < N(s))
    if (search_and_keyword (s, pos)) nband++;
  return nband+1;
}

/******************************************************************************
* Helper functions for BibTeX trees
******************************************************************************/

static bool
bib_is_entry (tree t) {
  return (as_string (L (t)) == "bib-entry") && (arity (t) == 3) &&
         is_atomic (t[0]) && is_atomic (t[1]) && (L(t[2]) == DOCUMENT); 
}

static bool
bib_is_field (tree t) {
  return (as_string (L (t)) == "bib-field") && (arity (t) == 2) &&
         is_atomic (t[0]); 
}

static bool
bib_is_comment (tree t) {
  return (as_string (L (t)) == "bib-comment"); 
}

static bool
bib_is_var (tree t) {
  return as_string (L(t)) == "bib-var" && arity (t) == 1 && is_atomic (t[0]);
}

static bool
bib_is_string (tree t) {
  return as_string (L(t)) == "bib-string" && arity (t) == 1 &&
         L(t[0]) == DOCUMENT;
}

static bool
bib_is_assign (tree t) {
  return as_string (L(t)) == "bib-assign" && arity (t) == 2 &&
         is_atomic (t[0]);
}

static bool
bib_is_preamble (tree t) {
  return as_string (L(t)) == "bib-preamble" && arity (t) == 1 &&
         L(t[0]) == DOCUMENT;
}

static bool
bib_is_latex (tree t) {
  return as_string (L(t)) == "bib-latex" && arity (t) == 1 &&
         is_atomic (t[0]);
}


static bool
bib_is_blank_string (string s) {
  for (int i= 0; i<N(s); i++)
    if (!is_space (s[i])) return false;
  return true;
}

static tree
bib_assoc (tree entry, string key) {
  if (bib_is_entry (entry)) {
    tree doc= entry[2];
    for (int i= 0; i<N(doc); i++)
      if (bib_is_field (doc[i]) && doc[i][0]->label == key)
	return doc[i][1];
  }
  return "";
}

/******************************************************************************
* BibTeX format.name$ - only returns first, von, jr and last
******************************************************************************/

static list<string>
get_words (string s) {
  list<string> words;
  string curr;
  int pos= 0;
  int depth= 0;
  bool special= false;
  int math= 0;
  while (pos < N(s) && is_space (s[pos])) pos++;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special, math);
    if (bib_is_space (ch) && depth == 0) {
      while (pos < N(s) && is_space (s[pos])) pos++;
      curr >> words;
      curr= "";
    }
    else curr << ch;
  }
  if (curr != "") curr >> words;
  return words;
}

static bool
first_is_locase (string s) {
  int pos= 0;
  int depth= 0;
  bool special= false;
  int math= 0;
  while (pos < N(s) && is_space (s[pos])) pos++;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special, math);
    if (bib_is_normal (ch))
      return (special || depth == 0) && is_iso_locase (ch[0]);
  }
  return false;
}

static string
get_fvl (string sfvl) {
  string w, res, f, v, l;
  list<string> words= get_words (sfvl);
  if (!is_nil (words)) l << words;
  bool all_up= true;
  for (int i= 0; i<N(words); i++)
    all_up= all_up && !first_is_locase (words[i]);
  if (!all_up) {
    while (!is_nil (words) && !first_is_locase (words[0])) {
      w << words;
      l= w << " " << l;
    }
    words= reverse (words);
    if (!is_nil (words) && !first_is_locase (words[0])) f << words;
    while (!is_nil (words) && !first_is_locase (words[0])) {
      w << words;
      f << " " << w;
    }
    if (!is_nil (words)) v << words;
    while (!is_nil (words)) {
      w << words;
      v << " " << w;
    }
  }
  else {
    if (!is_nil (words)) f << words;
    while (!is_nil (words)) {
      w << words;
      f= w << " " << f;
    }
  }
  res << "\\nextbib{}" << bib_to_latex (f);
  res << "\\nextbib{}" << bib_to_latex (v);
  res << "\\nextbib{}" << bib_to_latex (l);
  return res;
}

static string
get_vl_f (string svl, string sf) {
  string w, res, f, v, l;
  list<string> words= get_words (svl);
  if (!is_nil (words)) l << words;
  while (!is_nil (words) && !first_is_locase (words[0])) {
    w << words;
    l= w <<  " " << l;
  }
  if (!is_nil (words)) v << words;
  while (!is_nil (words)) {
    w << words;
    v= w << " " << v;
  }
  words= get_words (sf);
  if (!is_nil (words)) f << words;
  while (!is_nil (words)) {
    w << words;
    f= w << " " << f;
  }
  res << "\\nextbib{}" << bib_to_latex (f);
  res << "\\nextbib{}" << bib_to_latex (v);
  res << "\\nextbib{}" << bib_to_latex (l);
  return res;
}

static string
get_vl_j_f (string svl, string sj, string sf) {
  string res, j;
  res << get_vl_f (svl, sf);
  string w;
  list<string> words= get_words (sj);
  if (!is_nil (words)) j << words;
  while (!is_nil (words)) {
    w << words;
    j= w << " " << j;
  }
  res << "\\nextbib{}" << bib_to_latex (j);
  return res;
}

static string
get_until_char (string s, int& pos, string c) {
  string res;
  int depth= 0;
  bool special= false;
  int math= 0;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special, math);
    if (ch == c && depth == 0 && !special && math == 0) return res;
    else res << ch;
  }
  return res;
}

static string
get_first_von_last (string s) {
  string a, b, c;
  int pos= 0;
  a= get_until_char (s, pos, ",");
  b= get_until_char (s, pos, ",");
  c= get_until_char (s, pos, "");
  if (a != "" && b == "" && c == "") {
    string res= get_fvl (a);
    res << "\\nextbib{}";
    return res;
  }
  if (a != "" && b != "" && c == "") {
    string res= get_vl_f (a, b);
    res << "\\nextbib{}";
    return res;
  }
  if (a != "" && b != "" && c != "") return get_vl_j_f (a, b, c);
  return "";
}

string
bib_names (string s, int& nbfields) {
  string res;
  int pos= 0;
  nbfields++;
  res << "\\nextbib{}{\\bibnames}";
  while (pos < N(s)) {
    string name;
    int deb= pos;
    search_and_keyword (s, pos);
    int pos2= pos;
    if (pos < N(s)) pos -= 5;
    if (pos <= N(s) && deb < pos) name= s (deb, pos);
    res << "\\nextbib{}{\\bibname}";
    res << get_first_von_last (name);
    nbfields += 5;
    pos= pos2;
  }
  return res;
}

/******************************************************************************
* BibTeX purify$
******************************************************************************/

void
bib_purify_tree (tree t, string& res) {
  if (is_atomic (t)) res << t->label;
  else if (L(t) == WITH) bib_purify_tree (t[N(t)-1], res);
  else if (L(t) == as_tree_label ("keepcase")) bib_purify_tree (t[0], res);
  else if (L(t) == CONCAT || L(t) == DOCUMENT) {
    for (int i= 0; i<N(t); i++) bib_purify_tree (t[i], res);
  }
}

string
bib_purify (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  string res;
  bib_purify_tree (t, res);
  return res; 
}

/*
string
bib_purify (string s) {
  string r;
  int pos= 0;
  int depth= 0;
  bool special= false;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special);
    if (bib_is_space (ch) || bib_is_char (ch, '~') || bib_is_char (ch, '-'))
      r << ' ';
    else if (bib_is_iso_alpha (ch) || bib_is_digit (ch)) r << ch;
    else if (ch == "\\{" || ch == "\\}") r << ch[1];
  }
  return r;
}
*/

/******************************************************************************
* BibTeX text.length$
******************************************************************************/

/*
int
bib_text_length (string s) {
  int length= 0;
  int pos= 0;
  int depth= 0;
  bool oldspecial= false;
  bool special= false;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special);
    if (N(ch) == 1 && ch[0] != '{' && ch[0] != '}') length++;
    else if (!oldspecial && special) length++;
    oldspecial= special;
  }
  return length;
}
*/

static int
bib_tree_length (tree t) {
  if (is_atomic (t)) return N(t->label);
  else if (L(t) == WITH) return bib_tree_length (t[N(t)-1]);
  else if (L(t) == CONCAT || L(t) == DOCUMENT) {
    int s= 0;
    for (int i= 0; i<N(t); i++) s += bib_tree_length (t[i]);
  }
  return 0;
}

int
bib_text_length (scheme_tree st) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  return bib_tree_length (t); 
}

/******************************************************************************
* BibTeX text.prefix$
******************************************************************************/

static void
bib_get_prefix (tree t, string& pre, int& i) {
  if (i > 0) {
    if (is_atomic (t)) {
      string s= t->label;
      int beg= 0;
      while ((beg < N(s)) && (i > 0)) {
        pre << s[beg];
        beg++;
        i--;
      }
      return;
    }
    else {
      int pos= 0;
      if (L(t) == WITH) pos= N(t)-1;
      else if (L(t) == CONCAT || L(t) == DOCUMENT)
        for (int j= pos; j<N(t); j++) bib_get_prefix (t[j], pre, i);
    }
  }
}

string
bib_prefix (scheme_tree st, int i) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  if (t == "others") return "+";
  string pre;
  int j= i;
  bib_get_prefix (t, pre, j);
  return pre; 
}

/*
string
bib_text_prefix (string s, int i) {
  string chars;
  int nb= 0;
  int pos= 0;
  int depth= 0;
  bool oldspecial= false;
  bool special= false;
  while (pos < N(s)) {
    string ch= bib_parse_char (s, pos, depth, special);
    chars << ch;
    bool not_braces= (N(ch) == 1 && ch[0] != '{' && ch[0] != '}');
    if (!special && (oldspecial || not_braces || ch == "\\{" || ch == "\\}")) {
      nb++;
      if (nb == i) break;
    }
    oldspecial= special;
  }
  for (int k=0; k<depth; k++) chars << '}';
  return chars;
}
*/

/******************************************************************************
* Abbreviate names
******************************************************************************/

static void
get_first_letters (tree t, tree s1, tree s2, tree& l) {
  if (is_atomic (t)) {
    string s= t->label;
    int beg= 0;
    while (beg < N(s)) {
      while (beg < N(s) && is_space (s[beg])) beg++;
      if (beg < N(s)) {
        if (N(l) > 0) l << s2;
	int start= beg;
	tm_char_forwards (s, beg);
        l << s(start, beg) << s1;
        while (beg < N(s) && !is_space (s[beg])) {
	  tm_char_forwards (s, beg);
          if (beg < N(s) && s[beg] == '-') {
	    while (beg < N(s) && s[beg] == '-') {
	      beg++;
	      l << "-";
	    }
	    if (beg < N(s)) {
	      int start= beg;
	      tm_char_forwards (s, beg);
	      l << s (start, beg) << s1;
	    }
          }
        }
      }
    }
  }
  else {
    if (L(t) == WITH) get_first_letters (t[N(t)-1], s1, s2, l);
    else
      for (int pos= 0; pos<N(t); pos++)
        get_first_letters (t[pos], s1, s2, l);
  }
}

scheme_tree
bib_abbreviate (scheme_tree st, scheme_tree s1, scheme_tree s2) {
  tree t= simplify_correct (scheme_tree_to_tree (st));
  tree ts1= simplify_correct (scheme_tree_to_tree (s1));
  tree ts2= simplify_correct (scheme_tree_to_tree (s2));
  tree l (CONCAT);
  get_first_letters (t, ts1, ts2, l);
  return tree_to_scheme_tree (l);
}

/******************************************************************************
* Convert all fields to texmacs tree
******************************************************************************/

tree
bib_field_pages (string p) {
  tree res= compound ("bib-pages");;
  int p1, p2;
  int pos= 0;
  if (read_int (p, pos, p1)) {
    res << as_string (p1);
    while (pos < N(p) && !is_digit (p[pos])) pos++;
    if (read_int (p, pos, p2))
      res << as_string (p2);
    return res;
  }
  res << "0";
  return res;
}

static int
bib_get_fields (tree t, string& latex) {
  int nbfields= 0;
  for (int i= 0; i<N(t); i++) {
    if (bib_is_entry (t[i])) {
      for (int j= 0; j<N(t[i][2]); j++) {
        tree f= t[i][2][j];
        if (bib_is_field (f)) {
          if ((f[0]->label == "author" || f[0]->label == "editor") &&
              is_atomic (f[1])) {
            latex << bib_names (f[1]->label, nbfields);
          }
          else if (is_atomic (f[1]) && f[0]->label != "pages") {
            if (!bib_is_blank_string (f[1]->label)) {
              latex << "\\nextbib{}" << bib_to_latex (f[1]->label);
              nbfields++;;
            }
            else {
              latex << "\\nextbib{}";
              nbfields++;
            }
          }
        }
      }
    }
    else if (bib_is_comment (t[i]))
      nbfields += bib_get_fields (t[i], latex);
  }
  return nbfields;
}

static void
bib_set_fields (tree& t, array<tree> latex, int& ind) {
  for (int i= 0; i<N(t); i++) {
    if (bib_is_entry (t[i])) {
      //cout << "Changing " << t[i] << "\n";
      for (int j= 0; j<N(t[i][2]); j++) {
        tree f= t[i][2][j];
        //cout << "  Field " << f << "\n";
        if (bib_is_field (f)) {
          if ((f[0]->label == "author" || f[0]->label == "editor") &&
              is_atomic (f[1])) {
            tree res= compound ("bib-names"); ind++;
            while (latex[ind] == compound ("bibname")) {
              tree name= compound ("bib-name"); ind++;
              name << latex[ind]; ind++;
              name << latex[ind]; ind++;
              name << latex[ind]; ind++;
              name << latex[ind]; ind++;
              res << name;
            }
            f[1]= res;
          }
          else if (f[0]->label == "pages" && is_atomic (f[1]))
            f[1]= bib_field_pages (f[1]->label);
          else {
            f[1]= latex[ind];
            ind++;
          }
        }
      }
    }
    else if (bib_is_comment (t[i]))
      bib_set_fields (t[i], latex, ind);
  }
}

static array<tree>
bib_latex_array (tree latex) {
  if (is_func(latex, WITH)) latex= latex[N(latex) - 1];
  int i= 0;
  array<tree> res;
  while (i < N(latex) && latex[i] == compound ("nextbib", "")) {
    i++;
    if (i < N(latex)) {
      if (latex[i] == compound ("bibnames")
          || latex[i] == compound ("bibname")) {
        res << simplify_correct (latex[i]);
        i++;
      }
      else {
        tree elt (CONCAT);
        while (i < N(latex) && latex[i] != compound ("nextbib", "")) {
          elt << latex[i];
          i++;
        }
        res << simplify_correct (elt);
      }
    }
  }
  return res;
}

static bool
is_hyper_link (string s) {
  if (occurs (" ", s)) return false;
  return starts (s, "http://") || starts (s, "https://") || starts (s, "ftp://");
}

void
bib_parse_fields (tree& t) {
  string fields;
  int nb= bib_get_fields (t, fields);
  array<tree> latex= bib_latex_array (
      latex_to_tree (parse_latex (cork_to_sourcecode (fields), false, false)));
  //cout << "<<< " << t << LF;
  //cout << ">>> " << latex << LF;
  for (int k=0; k<N(latex); k++)
    if (is_atomic (latex[k]) && is_hyper_link (latex[k]->label))
      latex[k]= compound ("slink", latex[k]);
  int i= 0;
  if (nb == N(latex)) bib_set_fields (t, latex, i);
}

/******************************************************************************
* Return the value of a field
******************************************************************************/

scheme_tree
bib_field (scheme_tree st, string field) {
  tree t= scheme_tree_to_tree (st);
  if (bib_is_entry (t)) {
    tree doc= t[2];
    for (int i= 0; i<N(doc); i++) {
      if (bib_is_field (doc[i]) && doc[i][0] == field)
        return tree_to_scheme_tree (doc[i][1]);
    }
  }
  return "";
}

/******************************************************************************
* Is a field empty?
******************************************************************************/

bool
bib_empty (scheme_tree st, string f) {
  return (bib_field (st, f) == "");
}

/******************************************************************************
* BibTeX preamble$
******************************************************************************/

string
bib_preamble (tree t) {
  string pre;
  if (bib_is_preamble (t)) {
    tree doc= t[0];
    for (int i= 0; i<N(doc); i++)
      if (bib_is_latex (doc[i]))
	pre << "\n" << doc[i][0]->label;
  }
  return pre;
}

/******************************************************************************
* Entries selection
******************************************************************************/

hashmap<string,string>
bib_strings_dict (tree t) {
  hashmap<string,string> dict ("");
  dict("acmcs")= "ACM Computing Surveys";
  dict("acta")= "Acta Informatica";
  dict("cacm")= "Communications of the ACM";
  dict("ibmjrd")= "IBM Journal of Research and Development";
  dict("ibmsj")= "IBM Systems Journal";
  dict("ieeese")= "IEEE Transactions on Software Engineering";
  dict("ieeetc")= "IEEE Transactions on Computers";
  dict("ieeetcad")= "IEEE Transactions on Computer-Aided Design of Integrated Circuits";
  dict("ipl")= "Information Processing Letters";
  dict("jacm")= "Journal of the ACM";
  dict("jcss")= "Journal of Computer and System Sciences";
  dict("scp")= "Science of Computer Programming";
  dict("sicomp")= "SIAM Journal on Computing";
  dict("tocs")= "ACM Transactions on Computer Systems";
  dict("tods")= "ACM Transactions on Database Systems";
  dict("tog")= "ACM Transactions on Graphics";
  dict("toms")= "ACM Transactions on Mathematical Software";
  dict("toois")= "ACM Transactions on Office Information Systems";
  dict("toplas")= "ACM Transactions on Programming Languages and Systems";
  dict("tcs")= "Theoretical Computer Science";
  if (L(t) == DOCUMENT) {
    tree str (DOCUMENT);
    for (int i= 0; i<N(t); i++) {
      if (bib_is_string (t[i])) {
        str= t[i][0];
        break;
      }
    }
    for (int i= 0; i<N(str); i++) {
      if (bib_is_assign (str[i])) {
        string key= locase_all (str[i][0]->label);
        tree val= str[i][1];
        if (L(val) == CONCAT) {
          string sval;
          for (int j= 0; j<N(val); j++) {
            if (is_atomic (val[j])) sval << val[j]->label;
            else if (bib_is_var (val[j])) sval << dict[val[j][0]->label];
          }
          dict(key)= sval;
        }
        else if (is_atomic (val))
          dict(key)= val->label;
      }
    }
  }
  return dict;
}

static string
bib_subst_str (tree t, hashmap<string,string> dict) {
  if (is_atomic (t)) return t->label;
  else if (L(t) == CONCAT) {
    string s;
    for (int i= 0; i<N(t); i++) {
      if (bib_is_var (t[i])) s << dict[locase_all (t[i][0]->label)];
      else if (is_atomic (t[i])) s << t[i]->label;
      else s << bib_subst_str (t[i], dict);
    }
    return s;
  }
  else if (bib_is_var (t)) {
    string key= locase_all (t[0]->label);
    if (dict->contains (key)) return dict[locase_all (t[0]->label)];
    else return t[0]->label;
  }
  else return "";
}

tree
bib_subst_vars (tree t, hashmap<string,string> dict) {
  if (is_atomic (t) || L(t) == CONCAT || bib_is_var (t))
    return tree (copy (bib_subst_str (t, dict)));
  else {
    tree r (L(t), N(t));
    for (int i= 0; i<N(t); i++)
      r[i]= bib_subst_vars (t[i], dict);
    return r;
  }
}

static tree
bib_select_entries (tree t, tree bib_t) {
  hashmap<string, tree> h;
  hashset<string> r;
  tree entries (DOCUMENT);
  tree bt= copy (bib_t);
  for (int i= 0; i<N(t); i++)
    if (bib_is_entry (t[i])) {
      if (h->contains (t[i][1]->label))
        bibtex_warning << "Duplicate entry '" << t[i][1]->label << "'\n";
      else h(t[i][1]->label)= t[i];
    }
  for (int i= 0; i < arity (bt); i++) {
    string b= as_string (bt[i]);
    if (!h->contains (b))
      bibtex_warning << "Missing reference '" << b << "'\n";
    else if (!r->contains (b)) {
      r->insert (b);
      entries << h[b];
      tree cr= bib_assoc (h[b], string ("crossref"));
      if (cr != "") bt << as_string (cr);
    }
  }
  return entries;
}

tree
bib_entries (tree t, tree bib_t) {
  hashmap<string,string> dict= bib_strings_dict (t);
  tree res= bib_select_entries (t, bib_t);
  return res;
}

