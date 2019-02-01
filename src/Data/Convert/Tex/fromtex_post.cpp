
/******************************************************************************
* MODULE     : fromtex.cpp
* DESCRIPTION: conversion of tex strings into texmacs trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "LaTeX_Preview/latex_preview.hpp"
#include "Tex/convert_tex.hpp"
#include "Bibtex/bibtex.hpp"
#include "metadata.hpp"
#include "scheme.hpp"
#include "vars.hpp"
#include "tree_correct.hpp"
#include "url.hpp"

tree upgrade_tex (tree t);
extern bool textm_class_flag;
//bool textm_class_flag= true;
extern bool textm_appendices;
extern bool textm_unicode;
extern bool textm_natbib;

tree kill_space_invaders (tree t);
tree set_special_fonts (tree t, string lan);
tree filter_preamble (tree t);
tree latex_fallback_on_pictures (string s, tree t);
tree parsed_latex_to_tree (tree t);
tree latex_command_to_tree (tree t);
bool is_var_compound (tree t, string s);
bool is_var_compound (tree t, string s, int n);
string latex_to_texmacs_languages (string s);

/******************************************************************************
* Final modifications to the converted tree
******************************************************************************/

static tree
finalize_returns (tree t) {
  if (is_atomic (t)) return t;
  int  i, n= N(t);
  tree u (t, n);
  for (i=0; i<n; i++) u[i]= finalize_returns (t[i]);
  if (is_func (u, CONCAT)) {
    tree r (CONCAT);
    for (i=0; i<n; i++) {
      if (is_func (u[i], CONCAT)) r << A(u[i]);
      else if (is_compound (u[i])) r << u[i];
      else {
	int j= 0;
	string s= u[i]->label;
	while (j<N(s)) {
	  int start= j;
	    while ((j<N(s)) && (s[j]!='\n')) j++;
	    if (j>start) r << s(start,j);
	    if (j<N(s)) { r << tree (FORMAT, "new line"); j++; }
	}
      }
    }
    if (N(r)==0) return "";
    if (N(r)==1) return r[0];
    return r;
  }
  else return u;
}

static tree
parse_matrix_params (tree t, string tr, string br, string hoff) {
  // cout << "parse_matrix_params: " << hoff << LF;
  tree tformat (TFORMAT);
  string s= string_arg (t);
  bool col_flag=true;
  int i, n= N(s), col= as_int (hoff);
  for (i=0; i<n; i++) {
    switch (s[i]) {
    case 'l':
    case 'c':
    case 'r':
      {
	string col_s = as_string (col);
	string halign= copy (CELL_HALIGN);
	string how   = s (i, i+1);
	tformat << tree (CWITH, tr, br, col_s, col_s, halign, how);
        if (col_flag && col == 1)
          tformat << tree (CWITH, tr, br, col_s, col_s, CELL_LBORDER, "0ln");
	col_flag= true;
	col++;
	break;
      }
    case 'p':
    case 'm':
    case 'b':
    case 'X':
      {
	string col_s = as_string (col);
	string halign= copy (CELL_HYPHEN);
	char how_c   = s[i];
        string how = (how_c == 'm')? 'c' :
          ((how_c == 'b')? 'b' : 't');
	tformat << tree (CWITH, tr, br, col_s, col_s, halign, how);
        if (how_c != 'X') {
          int start= ++i;
          while (i<n && (s[i] != ' ') && (s[i] != '|')
                 && (s[i] != '<') && (s[i] != '*')) i++;
          string width= s(start, i);
          tformat << tree (CWITH, tr, br, col_s, col_s, CELL_HMODE, "exact");
          tformat << tree (CWITH, tr, br, col_s, col_s, CELL_WIDTH, width);
        }
        else {
          tformat << tree (CWITH, tr, br, col_s, col_s, CELL_HALIGN, "l");
        }
        if (col_flag && col == 1)
          tformat << tree (CWITH, tr, br, col_s, col_s, CELL_LBORDER, "0ln");
	col_flag= true;
	col++;
	break;
      }
    case '*':
      {
	col_flag= true;
	break;
      }
    case '|':
      {
	col_flag= false;
	string col_s= col==1? as_string (col): as_string (col-1);
	string hbor = col==1? copy (CELL_LBORDER): copy (CELL_RBORDER);
        int howmany=1;
        while (i+1 < n && s[i+1] == '|') { howmany++; i++; }
	string how  = as_string (howmany) * "ln";
	tformat << tree (CWITH, tr, br, col_s, col_s, hbor, how);
	break;
      }
    case ';':
      {
        // TODO: for the moment, we ignore dashed lines        
        break;
      }
    case '{':
      {
        for (int j=i; j<n; j++)
          if (s[j] == '}') { i= j; }
        break;
      }
    case '@':
      // FIXME: emergency exit; parameters of @ no longer between {}
      return tformat;
    }
  }
  if (col_flag) {
    string col_s= col==1? as_string (col): as_string (col-1);
    tformat << tree (CWITH, tr, br, col_s, col_s, CELL_RBORDER, "0ln");
  }
  return tformat;
}

static tree
parse_matrix_params (tree t) {
  return parse_matrix_params (t, "1", "-1", "1");
}

static tree
parse_cline (tree t) {
  string s= as_string (t);
  tree r= tree(CONCAT, 2);
  int i=0, j=0, k=0;
  while (i<N(s) && !is_digit (s[i])) i++;
  j=i;
  while (j<N(s) && is_digit (s[j])) j++;
  if (j<N(s)) r[0]= s(i,j);
  while (j<N(s) && !is_digit (s[j])) j++;
  k=j;
  while (i<N(s) && is_digit (s[k])) k++;
  if (j<N(s)) r[1]= s(j,k);
  return r;
}

static tree
parse_matrix_valign (tree t) {
  if (t == "t") return tree (CWITH, "1", "-1", "1", "-1", CELL_VALIGN, "t");
  if (t == "b") return tree (CWITH, "1", "-1", "1", "-1", CELL_VALIGN, "b");
  return tree (CWITH, "1", "-1", "1", "-1", CELL_VALIGN, "c");
}

static tree
trim_cell_spaces (tree t) {
  if (!is_func (t, TABLE)) return t;
  for (int i=0; i<N(t); i++) {
    tree row;
    if (is_func (t[i], ROW)) row= t[i];
    for (int j=0; j<N(row); j++) {
      if (is_func (row[j], CELL, 1)) {
        t[i][j][0]= trim_spaces (row[j][0]);
      }
    }
  }
  return t;
}

static void
parse_pmatrix (tree& r, tree t, int& i, string lb, string rb, string fm) {
  tree tformat (TFORMAT);
  if (N(t[i]) == 2 && t[i][0] != "tabular") {
    tformat= parse_matrix_params (t[i][1]);
  }
  else if (N(t[i]) == 2 && t[i][0] == "tabular") {
    tformat= parse_matrix_params (t[i][1]);
    tformat << parse_matrix_valign ("c");
  }
  else if (N(t[i]) == 3 && t[i][0] == "tabular*") {
    tformat= parse_matrix_params (t[i][2]);
    tformat << parse_matrix_valign (t[i][1]);
  }
  else if (N(t[i]) == 3 && t[i][0] == "tabularx") {
    tformat= parse_matrix_params (t[i][2]);
    tformat << parse_matrix_valign ("c");
    tformat << tree (TWITH, TABLE_WIDTH, t[i][1]);
  }
  else if (N(t[i]) == 4 && t[i][0] == "tabularx*") {
    tformat= parse_matrix_params (t[i][3]);
    tformat << parse_matrix_valign (t[i][1]);
    tformat << tree (TWITH, TABLE_WIDTH, t[i][2]);
  }
  if (lb != "") r << tree (LEFT, lb);

  int rows=0, cols=0;
  tree V (CONCAT);
  tree L (CONCAT);
  tree E (CONCAT);
  tree F (CONCAT);
  for (i++; i<N(t); i++) {
    tree v= t[i];
    if (v == tree (FORMAT, "line separator")) {
      L << simplify_concat (E);
      for (int j=0; j<N(F); j++) L << F[j];
      E= tree (CONCAT);
      F= tree (CONCAT);
      continue;
    }
    else if (v == tree (FORMAT, "new line")) {
      continue;
    }
    else if (v == tree (FORMAT, "next line") ||
        is_apply (v, "tabularnewline", 0)) {
      L << simplify_concat (E);
      for (int j=0; j<N(F); j++) L << F[j];
      V << L;
      cols= max (cols, N(L));
      L= tree (CONCAT);
      E= tree (CONCAT);
      F= tree (CONCAT);
      while (i+1<N(t) && t[i+1] == " ") i++;
      continue;
    }
    else if (is_func (v, BEGIN) && (v[0] == "array" || v[0] == "tabular" ||
          v[0] == "tabular*" || v[0] == "tabularx" || v[0] == "tabularx*")) {
      parse_pmatrix (E, t, i, "", "", "tabular*");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "stack")) {
      parse_pmatrix (E, t, i, "", "", "stack");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "cases")) {
      parse_pmatrix (E, t, i, "", "", "choice");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "matrix")) {
      parse_pmatrix (E, t, i, "", "", "tabular*");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "pmatrix")) {
      parse_pmatrix (E, t, i, "", "", "matrix");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "bmatrix")) {
      parse_pmatrix (E, t, i, "[", "]", "tabular*");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "vmatrix")) {
      parse_pmatrix (E, t, i, "", "", "det");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "smallmatrix")) {
      parse_pmatrix (E, t, i, "", "", "matrix*");
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (END, "array")) break;
    else if (v == tree (END, "tabular")) break;
    else if (v == tree (END, "tabularx")) break;
    else if (v == tree (END, "cases")) break;
    else if (v == tree (END, "stack")) break;
    else if (v == tree (END, "matrix")) break;
    else if (v == tree (END, "pmatrix")) break;
    else if (v == tree (END, "bmatrix")) break;
    else if (v == tree (END, "vmatrix")) break;
    else if (v == tree (END, "smallmatrix")) break;
    else if (v == tree (APPLY, "hline")) {
      int howmany= 1;
      while (i+1<N(t) && (t[i+1] == tree (APPLY, "hline") || t[i+1] == " " )) {
        if (t[i+1] == tree (APPLY, "hline")) howmany++;
        i++;
      }
      while (i+1<N(t) && t[i+1] == " ") i++;
      string how  = as_string (howmany)*"ln";
      int    row  = N(V)+ (N(L)==0? 0: 1);
      string row_s= row==0? as_string (row+1): as_string (row);
      string vbor = row==0? copy (CELL_TBORDER): copy (CELL_BBORDER);
      tformat << tree (CWITH, row_s, row_s, "1", "-1", vbor, how);
    }
    else if (is_apply (v, "cline", 1)) {
      int    row  = N(V)+ (N(L)==0? 0: 1);
      tree arg= parse_cline (v[1]);
      string row_s= row==0? as_string (row+1): as_string (row);
      string vbor = row==0? copy (CELL_TBORDER): copy (CELL_BBORDER);
      string how  = "1ln";
      tformat << tree (CWITH, row_s, row_s, arg[0], arg[1], vbor, how);
      while (i+1<N(t) && t[i+1] == " ") i++;
    }
    else if (is_apply (v, "multirow", 3)) {
      string row_t= as_string (N(V) + 1);
      string col_s= as_string (N(L) + N(F) + 1);
      string height= as_string (v[1]);
      tformat << tree (CWITH, row_t, row_t, col_s, col_s, CELL_ROW_SPAN, height);
      if (as_string (v[2]) != "*") {
        tree width= tree (OVER, as_string (v[2]), height);
        string row_b= as_string (as_int (row_t) + as_int (height) - 1);
        tformat << tree (CWITH, row_t, row_b, col_s, col_s, CELL_HEIGHT, width);
        tformat << tree (CWITH, row_t, row_b, col_s, col_s, CELL_HMODE, "exact");
      }
      tformat << tree (CWITH, row_t, row_t, col_s, col_s, CELL_VALIGN, "c");
      E << v[3];
    }
    else if (is_apply (v, "multicolumn", 3)) {
      string row_s= as_string (N(V) + 1);
      string col_s= as_string (N(L) + N(F) + 1);
      string width= as_string (v[1]);
      tformat << tree (CWITH, row_s, row_s, col_s, col_s, CELL_COL_SPAN, width);
      tree tmp= parse_matrix_params (v[2], row_s, row_s, col_s);
      for (int j=0; j<N(tmp); j++) tformat << tmp[j];
      E << v[3];
      for (int j=1; j < as_int (width); j++) F << concat ();
    }
    else if (is_func (v, VAR_VSPACE)) {
      int row = N(V)+ (N(L)==0? 0: 1);
      string row_s= row==0? as_string (row+1): as_string (row);
      tformat << tree (CWITH, row_s, row_s, "1", "-1", "cell-valign", "top");
      tformat << tree (CWITH, row_s, row_s, "1", "-1", "cell-vmode", "exact");
      tformat << tree (CWITH, row_s, row_s, "1", "-1", "cell-height",
          tree (PLUS, "1fn", v[0]));
    }
    else E << v;
  }
  if ((N(L)>0) || (N(E)>0) || (N(F)>0)) {
    L << simplify_concat (E);
    for (int j=0; j<N(F); j++) L << F[j];
    F= concat ();
    V << L;
  }
  if ((max (cols, N(L)) * N(V)) == 0) {
    L= tree (CONCAT, "");
    V= tree (CONCAT, tree (CONCAT, ""));
  }
  cols= max (cols, N(L));
  rows= N(V);

  int x, y;
  tree M (TABLE);
  for (y=0; y<rows; y++) {
    tree R (ROW);
    for (x=0; x<cols; x++)
      if (x<N(V[y])) R << tree (CELL, V[y][x]);
      else R << tree (CELL, "");
    M << R;
  }
  tformat << trim_cell_spaces (M);
  r << compound (fm, tformat);
  if (rb != "") r << tree (RIGHT, rb);
}

static tree
finalize_pmatrix (tree t) {
  if (is_atomic (t)) return t;
  int  i, n= N(t);
  tree u (t, n);
  for (i=0; i<n; i++) u[i]= finalize_pmatrix (t[i]);
  if (is_func (u, CONCAT)) {
    tree r (CONCAT);
    for (i=0; i<n; i++)
      if (is_func (u[i], BEGIN)) {
	if (u[i][0] == "array")
	  parse_pmatrix (r, u, i, "", "", "tabular*");
	else if (u[i][0] == "tabular"  || u[i][0] == "tabular*" ||
	         u[i][0] == "tabularx" || u[i][0] == "tabularx*")
	  parse_pmatrix (r, u, i, "", "", "tabular*");
	else if (u[i][0] == "cases")
	  parse_pmatrix (r, u, i, "", "", "choice");
	else if (u[i][0] == "stack")
	  parse_pmatrix (r, u, i, "", "", "stack");
	else if (u[i][0] == "matrix")
	  parse_pmatrix (r, u, i, "", "", "tabular*");
	else if (u[i][0] == "pmatrix")
	  parse_pmatrix (r, u, i, "", "", "matrix");
	else if (u[i][0] == "bmatrix")
	  parse_pmatrix (r, u, i, "[", "]", "tabular*");
	else if (u[i][0] == "vmatrix")
	  parse_pmatrix (r, u, i, "", "", "det");
	else if (u[i][0] == "smallmatrix")
	  parse_pmatrix (r, u, i, "", "", "matrix*");
	else r << u[i];
      }
      else r << u[i];
    return r;
  }
  else if (is_func (u, APPLY, 2) && (u[0] == "matrix"))
    return tree (APPLY, "tabular*", u[1]);
  else if (is_func (u, APPLY, 2) && (u[0] == "smallmatrix"))
    return tree (APPLY, "matrix*", u[1]);
  else if (is_func (u, APPLY, 2) && (u[0] == "substack")) {
    tree cc (CONCAT);
    cc << tree (BEGIN, "stack");
    if (is_func (u[1], CONCAT)) cc << A(u[1]);
    else cc << u[1];
    cc << tree (END, "stack");
    return finalize_pmatrix (cc);
  }
  else return u;
}

static void
remove_space (tree& t) {
  if (arity (t) == 0) return;
  if (is_compound (t[N(t)-1])) return;
  string s= t[N(t)-1]->label;
  if ((N(s)>0) && (s[N(s)-1]==' '))
    t[N(t)-1]= s(0,N(s)-1);
}

static void
insert_return (tree& t) {
  remove_space (t);
  if ((arity(t)>0) && (t[N(t)-1]==tree (FORMAT, "new line"))) return;
  t << tree (FORMAT, "new line");
}

static bool
space_eater (tree t) {
  return is_func (t, FORMAT, 1) && (t[0]->label != "new line");
}

static bool
admissible_env (tree t) {
  string s= t[0]->label;
  if (ends (s, "*")) s= s (0, N(s)-1);
  string type= latex_type ("\\begin-" * s);
  return type == "list"        || type == "environment"     ||
         type == "enunciation" || type == "math-environment";
}

static bool
is_enunciation (tree t) {
  string s= t[0]->label;
  if (ends (s, "*")) s= s (0, N(s)-1);
  return latex_type ("\\begin-" * s) == "enunciation";
}

static string
translate_list (string s) {
  if (s == "itemizeminus") return "itemize-minus";
  if (s == "itemizedot") return "itemize-dot";
  if (s == "itemizearrow") return "itemize-arrow";
  if (s == "enumeratenumeric") return "enumerate-numeric";
  if (s == "enumerateroman") return "enumerate-roman";
  if (s == "enumerateromancap") return "enumerate-romancap";
  if (s == "enumeratealpha") return "enumerate-alpha";
  if (s == "enumeratealphacap") return "enumerate-alphacap";
  if (s == "asparaitem") return "itemize";
  if (s == "inparaitem") return "itemize";
  if (s == "compactitem") return "itemize";
  if (s == "asparaenum") return "enumerate";
  if (s == "inparaenum") return "enumerate";
  if (s == "compactenum") return "enumerate";
  if (s == "itemize*") return "itemize";
  if (s == "enumerate*") return "enumerate";
  if (s == "asparaitem*") return "itemize";
  if (s == "inparaitem*") return "itemize";
  if (s == "compactitem*") return "itemize";
  if (s == "asparaenum*") return "enumerate";
  if (s == "inparaenum*") return "enumerate";
  if (s == "compactenum*") return "enumerate";
  return s;
}

static tree
finalize_layout (tree t) {
  if (is_atomic (t)) return t;
  int  i, n= N(t);
  tree u (t, n);
  for (i=0; i<n; i++) u[i]= finalize_layout (t[i]);
  if (is_func (u, CONCAT)) {
    string lang= "code";
    bool spc_flag =false;
    bool item_flag=false;
    tree r (CONCAT);
    for (i=0; i<n; i++) {
      tree v= u[i];

      if (space_eater (v)) {
	remove_space (r);
	r << v;
	spc_flag = true;
	item_flag= false;
	continue;
      }

      if (is_func (v, BEGIN, 2) && is_enunciation (v)) {
        string s= as_string (v[0]);
        s= s(0, N(s)-1);
        r << tree (BEGIN, s) << compound ("dueto", v[1]);
        continue;
      }

      if ((is_func (v, BEGIN) || is_func (v, END)) && N(v) > 0 &&
          (v[0] == "tmpadded"     || v[0] == "tmpadded*"      ||
           v[0] == "tmframed"     || v[0] == "tmframed*"      ||
           v[0] == "tmunderlined" || v[0] == "tmunderlined*"  ||
           v[0] == "tmoverlined"  || v[0] == "tmoverlined*"   ||
           v[0] == "tmbothlined"  || v[0] == "tmbothlined*"   ||
           v[0] == "tmornamented" || v[0] == "tmornamented*" )) {
        string env= as_string (v[0]);
        if (env[N(env)-1] == '*' && N(v) > 1) {
          if (is_concat (v[1]))
            r << A (v[1]);
          else
            r << v[1];
          env= env (2, N(env)-1);
        }
        else
          env= env (2, N(env));
        r << tree (L(v), env);
        continue;
      }

      if (is_func (v, BEGIN) &&
          (v[0] == "algorithmic" || v[0] == "algorithmic*")) {
	//r << tree (BEGIN, "algorithm");
	continue;
      }

      if (is_func (v, END) &&
          (v[0] == "algorithmic" || v[0] == "algorithmic*")) {
	//r << tree (END, "algorithm");
	continue;
      }

      if (is_func (v, BEGIN) &&
          (v[0] == "algorithm"   || v[0] == "algorithm*" ||
           v[0] == "algorithm2e" || v[0] == "algorithm2e*" )) {
	r << tree (NEW_LINE) << tree (BEGIN, "algorithm");
	continue;
      }

      if (is_func (v, END) &&
          (v[0] == "algorithm"   || v[0] == "algorithm*" ||
           v[0] == "algorithm2e" || v[0] == "algorithm2e*" )) {
	r << tree (END, "algorithm") << tree (NEW_LINE);
	continue;
      }

      if (is_func (v, BEGIN) && (v[0] == "tmcode" || v[0] == "tmcode*")) {
        if (is_func (v, BEGIN, 2)) lang= string_arg (v[1]) * "-code";
        else lang= "code";
	r << tree (BEGIN, lang);
	continue;
      }

      if (is_func (v, END) && v[0] == "tmcode") {
	r << tree (END, lang);
	continue;
      }

      if (is_func (v, BEGIN) && v[0] == "tmindent") {
	r << tree (BEGIN, "indent");
	continue;
      }

      if (is_func (v, END) && v[0] == "tmindent") {
	r << tree (END, "indent");
	continue;
      }

      if (is_func (v, BEGIN, 2) && (v[0] == "otherlanguage" ||
                                    v[0] == "otherlanguage*")) {
        string lang= latex_to_texmacs_languages (string_arg (v[1]));
	r << tree (SET, "language", lang);
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "otherlanguage" ||
                                  v[0] == "otherlanguage*")) {
	r << tree (RESET, "language");
	continue;
      }

      if (is_func (v, BEGIN, 1) && (v[0] == "picture")) {
	for (; i<n; i++)
	  if (is_func (u[i], IMAGE)) r << u[i];
	  else if (is_func (u[i], END, 1) && (u[i][0] == "picture"))
	    break;
	continue;
      }

      if (is_func (v, BEGIN) && ((v[0] == "tmparsep"))) {
	r << tree (SET, "par-par-sep", v[1]);
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "tmparsep")) {
	r << tree (RESET, "par-par-sep");
	continue;
      }

      if (is_func (v, BEGIN, 3) && v[0] == "tmparmod") {
        // TODO: deal with par-left and par-right as well
        r << tree (SET, "par-first", v[3]);
	continue;
      }

      if (is_func (v, END) && (v[0] == "tmparmod")) {
	r << tree (RESET, "par-first");
	continue;
      }

      if (is_func (v, BEGIN, 2) && ((v[0] == "minipage"))) {
	r << tree (BEGIN, "minipage", "f", v[1]);
	continue;
      }
      if (is_func (v, BEGIN, 3) && ((v[0] == "minipage*"))) {
	r << tree (BEGIN, "minipage", v[1], v[2]);
	continue;
      }

      if (is_func (v, BEGIN) && ((v[0] == "multicols"))) {
	r << tree (SET, "par-columns", v[1]);
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "multicols")) {
	r << tree (RESET, "par-columns");
	continue;
      }

      if ((is_func (v, BEGIN, 1) && (v[0] == "figure" ))  ||
          (is_func (v, BEGIN, 2) && (v[0] == "figure*"))) {
	r << tree (NEW_LINE) << tree (BEGIN, "bigfigure");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "figure")) {
	r << tree (END, "bigfigure") << tree (NEW_LINE);
	continue;
      }

      if ((is_func (v, BEGIN, 1) && (v[0] == "figure*" ))  ||
          (is_func (v, BEGIN, 2) && (v[0] == "figure**"))) {
	r << tree (NEW_LINE) << tree (BEGIN, "bigfigure*");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "figure*")) {
	r << tree (END, "bigfigure*") << tree (NEW_LINE);
	continue;
      }

      if ((is_func (v, BEGIN, 1) && (v[0] == "teaserfigure" ))  ||
          (is_func (v, BEGIN, 2) && (v[0] == "teaserfigure*"))) {
	r << tree (NEW_LINE) << tree (BEGIN, "bigfigure*");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "teaserfigure")) {
	r << tree (END, "bigfigure*") << tree (NEW_LINE);
	continue;
      }

      if ((is_func (v, BEGIN, 1) && (v[0] == "table*" ))  ||
          (is_func (v, BEGIN, 2) && (v[0] == "table**"))) {
	r << tree (NEW_LINE) << tree (BEGIN, "bigtable*");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "table*")) {
	r << tree (END, "bigtable*") << tree (NEW_LINE);
	continue;
      }

      if ((is_func (v, BEGIN, 1) && (v[0] == "table" ))  ||
          (is_func (v, BEGIN, 2) && (v[0] == "table*"))) {
	r << tree (NEW_LINE) << tree (BEGIN, "bigtable");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "table")) {
	r << tree (END, "bigtable") << tree (NEW_LINE);
	continue;
      }

      if (is_func (v, BEGIN) && (v[0] == "thebibliography")) {
	r << tree (NEW_LINE) << v;
	continue;
      }

      /*
      if (is_func (v, BEGIN) && (v[0] == "hide-preamble")) {
	r << tree (BEGIN, "hide-preamble");
	continue;
      }

      if (is_func (v, END) && (v[0] == "hide-preamble")) {
	r << tree (END, "hide-preamble");
	continue;
      }
      */

      if ((is_func (v, BEGIN, 1) || is_func (v, BEGIN, 2))
          && admissible_env (v) && v[0] != "thebibliography") {
	if (v == tree (BEGIN, "verbatim")) {
	  r << v; i++;
	  if ((i<n) && (t[i] == tree (FORMAT, "new line"))) {
	    remove_space (r);
	    r << t[i]; i++;
	  }
	  while ((i<n) && (t[i] != tree (END, "verbatim"))) {
	    if ((t[i] == tree (FORMAT, "new line")) &&
		(((i+1) == n) || (t[i+1] != tree (END, "verbatim"))))
	      r << tree (FORMAT, "new line");
	    else r << t[i];
	    i++;
	  }
	  if (i<n) r << t[i];
	  spc_flag = i>0 && t[i-1] == tree (FORMAT, "new line");
	  item_flag= false;
	  continue;
	}

	if (v == tree (BEGIN, "displaymath"))
	  v= tree (BEGIN, "equation*");
	if (v == tree (BEGIN, "math")) {
	  r << tree (SET, MODE, "math");
	  spc_flag = item_flag= false;
	  continue;
	}

	insert_return (r);
        v[0]= translate_list (v[0]->label);
	r << v;
	spc_flag = true;
	item_flag= (latex_type ("\\begin-" * v[0]->label) == "list");
	continue;
      }

      if (is_func (v, END, 1) && admissible_env (v)) {
	if (v == tree (END, "displaymath"))
	  v= tree (END, "equation*");
	if (v == tree (END, "math")) {
	  r << tree (RESET, MODE);
	  spc_flag = item_flag= false;
	  continue;
	}

	remove_space (r);
	r << tree (END, translate_list (v[0]->label));
	if (i+1 == N(t) || t[i+1] != tree (FORMAT, "new line"))
	  insert_return (r);
	spc_flag = true;
	item_flag= false;
	continue;
      }

      // Needed for matching beginning/ending for unknown or user envs.
      // It will be restored in finalize_misc.
      if (is_func (v, BEGIN)) {
	string var= as_string (v[0]);
  if (var[N(var)-1] == '*') {
    var= var(0,N(var)-1);
    tree w (BEGIN, var);
    for (int j=1; j<N(v); j++) w << v[j];
    r << w;
    continue;
  }
      }

      // Needed to transform some modifiers, when they are written as
      // environments.
      if ((is_func (v, BEGIN, 1) || is_func (v, END, 1)) &&
          latex_type (as_string (v[0])) == "modifier") {
        if (is_func (v, BEGIN)) {
          r << parsed_latex_to_tree (tuple ("\\"*as_string (v[0])));
        }
        else {
          tree w= parsed_latex_to_tree (tuple ("\\"*as_string (v[0])));
          tree x = tree (RESET);
          x << copy (w[0]);
          r << x;
        }
	continue;
      }

      if ((v == tree (APPLY, "item")) ||
	  (is_func (v, APPLY, 2) && (v[0]->label == "item*"))) {
	if (!item_flag) insert_return (r);
	r << v;
	spc_flag = true;
	item_flag= false;
	continue;
      }

      if ((is_atomic (v)) && spc_flag &&
	  (N(v->label)>0) && (v->label[0]==' '))
	{
	  if (N(v->label)==1) continue;
	  r << v->label (1, N(v->label));
	}
      else r << v;
      spc_flag = false;
      item_flag= false;
    }
    return r;
  }
  else return u;
}

static tree
finalize_sections (tree t) {
  tree r (DOCUMENT);
  for (int i=0; i<N(t); i++) {
    tree u= t[i];
    if (is_concat (u) && N(u) >= 2 &&
	(is_var_compound (u[0], "part", 1)             ||
	 is_var_compound (u[0], "part*", 1)            ||
	 is_var_compound (u[0], "part*", 2)            ||
	 is_var_compound (u[0], "chapter", 1)          ||
	 is_var_compound (u[0], "chapter*", 1)         ||
	 is_var_compound (u[0], "chapter*", 2)         ||
	 is_var_compound (u[0], "section", 1)          ||
	 is_var_compound (u[0], "section*", 1)         ||
	 is_var_compound (u[0], "section*", 2)         ||
	 is_var_compound (u[0], "subsection", 1)       ||
	 is_var_compound (u[0], "subsection*", 1)      ||
	 is_var_compound (u[0], "subsection*", 2)      ||
	 is_var_compound (u[0], "subsubsection", 1)    ||
	 is_var_compound (u[0], "subsubsection*", 1)   ||
	 is_var_compound (u[0], "subsubsection*", 2)))
      {
	if (N(u) > 2 && u[1] == " ")
	  u= u (0, 1) * u (2, N(u));
	if (!is_func (u[1], LABEL) || (N(u) >= 3 && !is_func (u[2], LABEL))) {
	  if (!is_func (u[1], LABEL)) {
	    r << u[0];
	    if (N(u) == 2) u= u[1];
	    else u= u (1, N(u));
	  }
	  else {
	    r << u (0, 2);
	    if (N(u) == 3) u= u[2];
	    else u= u (2, N(u));
	  }
	  if (is_atomic (u) && starts (u->label, " "))
	    u= u->label (1, N(u->label));
	  if (is_concat (u) && is_atomic (u[0]) && starts (u[0]->label, " "))
	    u= tree (CONCAT, u[0]->label (1, N(u[0]->label))) * u (1, N(u));
	  r << u;
	}
	else r << u;
      }
    else r << u;
  }
  return r;
}

tree
env2macro (tree t, string from, string to) {
  if (is_atomic (t)) return t;
  tree r = concat ();
  for (int i=0; i<N(t); i++) {
    tree u= t[i];
    if (is_concat (u)) r << env2macro (u, from, to);
    else if (is_func (u, BEGIN, 1) && (u[0] == from)) {
      tree sub = concat ();
      i++;
      while (i < N(t) && !(is_func (t[i], END, 1) && t[i][0] == from)) {
        if (is_concat(t[i])) sub << env2macro (t[i], from, to);
        else sub << t[i];
        i++;
      }
      r << tree (APPLY, to, sub);
    }
    else r << u;
  }
  return r;
}

bool
textm_algorithm_break_after (tree t) {
  string var;
  if (is_func (t, APPLY, 2)) {
    var = "\\"*as_string (t[0]);
    var = (var[N(var)-1] == '*')? var(0, N(var)-1) : var;
  }
  return ((is_func (t, END) &&
            (t[0] == "algo-inputs"     || t[0] == "algo-outputs"   ||
             t[0] == "algo-for"        || t[0] == "algo-while"     ||
             t[0] == "algo-function"   || t[0] == "algo-procedure" ||
             t[0] == "algo-if-else-if" || t[0] == "algo-loop"      ||
             t[0] == "algo-repeat"     || t[0] == "algo-body"))    ||
         (is_apply (t, "algo-data") || is_apply (t, "algo-result")) ||
         (latex_type (var) == "algorithm2e"));
}

bool
textm_algorithm_space_after (tree t) {
  return is_apply (t, "algo-to") || is_apply (t, "algo-true") ||
         is_apply (t, "algo-false");
}

bool
textm_algorithm_need_arg (tree t) {
  return is_apply (t, "algo-ensure", 0)  ||  is_apply (t, "algo-globals", 0) ||
         is_apply (t, "algo-require", 0) ||  is_apply (t, "algo-return", 0)  ||
         is_apply (t, "algo-state", 0)   ||  is_apply (t, "algo-print", 0);
}

bool
textm_algorithm_begin_algo (tree t) {
  return (is_func (t, BEGIN) &&
           (t[0] == "algorithm"   || t[0] == "algorithm*"    ||
            t[0] == "algorithmic" || t[0] == "algorithmic*"  ||
            t[0] == "algorithm2e" || t[0] == "algorithm2e*"));
}

bool
textm_algorithm_end_algo (tree t) {
  return (is_func (t, END) && (t[0] == "algorithmic" ||
        t[0] == "algorithm2e" || t[0] == "algorithm"));
}

bool
textm_algorithm_end_arg (tree t) {
  return textm_algorithm_need_arg (t) || textm_algorithm_end_algo (t) ||
         is_apply (t, "algo-else")    || is_apply (t, "algo-else-if") ||
         (is_func (t, BEGIN) &&
            (t[0] == "algo-inputs"    || t[0] == "algo-outputs"   ||
             t[0] == "algo-for"       || t[0] == "algo-while"     ||
             t[0] == "algo-function"  || t[0] == "algo-procedure" ||
             t[0] == "algo-if-else-if"|| t[0] == "algo-loop"      ||
             t[0] == "algo-repeat"    || t[0] == "algo-body"))    ||
         textm_algorithm_break_after (t);
}

bool after_linefeed (tree r) {
  return N(r) > 0 && (r[N(r)-1] == "\n");
}

tree
textm_algorithm_parse_arg (tree t, int &i) {
  tree r = copy (t[i++]), arg= concat ();
  while (i<N(t) && !textm_algorithm_end_arg (t[i]))
    arg << t[i++];
  i--;
  r << trim_spaces (arg);
  return r;
}

tree
complete_algorithm_args (tree t, bool &in) {
  if (is_atomic (t)) return t;
  tree r = tree (L(t));
  for (int i=0, n=N(t); i<n; i++) {
    if (textm_algorithm_begin_algo (t[i])) in = true;
    else if (in && textm_algorithm_end_algo (t[i])) {
      in = false;
      r << t[i];
      continue;
    }

    if (in && textm_algorithm_need_arg (t[i])) {
      r << textm_algorithm_parse_arg (t, i) << "\n";
    }
    else if (in && textm_algorithm_space_after (t[i]) && i != n-1)
      r << t[i] << " ";
    else if (in && textm_algorithm_break_after (t[i]) && i != n-1)
      r << t[i] << "\n";
    else if (in && is_func (t[i], SPACE, 1) && t[i][0] == "0.75spc")
      r << "\n";
    else if (!(in && after_linefeed (r)
          && (t[i] == " " || t[i] == "\t" || t[i] == "\n")))
      r << complete_algorithm_args (t[i], in);
    }
  return r;
}

bool
textm_algorithm_end_if (tree t) {
  return textm_algorithm_end_algo (t) ||
    (is_func (t, END) && t[0] == "algo-if-else-if");
}

bool
textm_algorithm_end_if_arg (tree t) {
  return textm_algorithm_end_if (t) ||
    is_apply (t, "algo-else") || is_apply (t, "algo-else-if");
}

tree
complete_algorithm_if_else_if (tree t, bool in) {
  if (is_atomic (t)) return t;
  tree r= tree (L(t)), arg, ifelse;
  int c=0;
  for (int i=0; i<N(t); i++) {
    if      (textm_algorithm_begin_algo (t[i])) in = true;
    else if (textm_algorithm_end_algo (t[i]))   in = false;

    if (in && is_func (t[i], BEGIN) && t[i][0] == "algo-if-else-if") {
      ifelse= copy (t[i]);
      i++;
      while (i<N(t) && !textm_algorithm_end_algo (t[i])
          && !(textm_algorithm_end_if (t[i]) && c==0)) {
        arg= concat();
	      if (i>0 && is_apply (t[i-1], "algo-else") && N(t[i-1])>1)
	        arg << t[i-1][1] << "\n";
        while (i<N(t) && !textm_algorithm_end_algo (t[i])
            && !(textm_algorithm_end_if_arg (t[i]) && c==0)) {
          if (is_func (t[i], BEGIN) && t[i][0] == "algo-if-else-if") c++;
          if (is_func (t[i], END) && t[i][0] == "algo-if-else-if") c--;
          if ((t[i] == " " || t[i] == "\n") && (i==0 || i==N(t)-1 ||
                (i+1<N(t) && textm_algorithm_end_if_arg (t[i+1]))))
            i++;
          else
            arg << complete_algorithm_if_else_if (t[i++], in);
        }
        if (arg != concat ())
          ifelse << arg;
        else
          i++;
        if (i<N(t) && is_apply (t[i], "algo-else-if") && N(t[i])>1)
          ifelse << t[i][1];
      }
      r << complete_algorithm_if_else_if (ifelse, in)
        << tree (END, "algo-if-else-if");
      if (i<N(t) && textm_algorithm_end_algo (t[i])) r << t[i];
    }
    else
      r << complete_algorithm_if_else_if (t[i], in);
  }
  return r;
}

tree
concatenate_algorithm_if_else_if (tree t, bool &in) {
  if (is_atomic (t)) return t;
  tree r = tree (L(t));
  for (int i=0; i<N(t); i++) {
    if (textm_algorithm_begin_algo (t[i])) in = true;
    else if (in && textm_algorithm_end_algo (t[i])) {
      in = false;
      r << t[i];
      continue;
    }

    if (in && is_apply (t[i], "algo-if")) {
      tree ifelse = tree (APPLY, "algo-if-else-if");
      for (int j=1; j< N(t[i]); j++)
        ifelse << concatenate_algorithm_if_else_if (t[i][j], in);
      while (i+1<N(t) && (t[i+1] == " " || t[i+1] == "\n")) i++;
      while (i+1<N(t) && is_apply (t[i+1], "algo-else-if")) {
        i++;
        for (int j=1; j< N(t[i]); j++)
          ifelse << concatenate_algorithm_if_else_if (t[i][j], in);
      }
      while (i+1<N(t) && (t[i+1] == " " || t[i+1] == "\n")) i++;
      if (i+1<N(t) && is_apply (t[i+1], "algo-else")) {
        i++;
        for (int j=1; j< N(t[i]); j++)
          ifelse << concatenate_algorithm_if_else_if (t[i][j], in);
      }
      while (i+1<N(t) && (t[i+1] == " " || t[i+1] == "\n")) i++;
      r << ifelse << "\n";
    }
    else if (!(in && after_linefeed (r)
          && (t[i] == " " || t[i] == "\t" || t[i] == "\n")))
      r << concatenate_algorithm_if_else_if (t[i], in);
    }
  return r;
}

tree
finalize_algorithms (tree t) {
  bool in = false;
  t = complete_algorithm_args (t, in);
  // cout << "complete_algorithm_args: " << LF << t << LF << LF;
  in = false;
  t = complete_algorithm_if_else_if (t, in);
  // cout << "complete_algorithm_if_else_if: " << LF << t << LF << LF;
  in = false;
  t = concatenate_algorithm_if_else_if (t, in);
  // cout << "concatenate_algorithm_if_else_if: " << LF << t << LF << LF;
  return t;
}

static tree
finalize_returns_bis (tree t) {
  if (L(t) == RAW_DATA) return t;
  if (is_atomic (t)) {
    tree r (CONCAT);
    int j= 0;
    string s= get_label (t);
    while (j<N(s)) {
      int start= j;
      while ((j<N(s)) && (s[j]!='\n')) j++;
      if (j>start) r << s(start,j);
      if (j<N(s)) { r << tree (FORMAT, "new line"); j++; }
    }
    if (N(r)==0) return "";
    if (N(r)==1) return r[0];
    return r;
  }
  int i, n= N(t);
  tree u (L(t));
  for (i=0; i<n; i++) u << finalize_returns_bis (t[i]);
  return u;
}

static array<tree>
clean_paragraph_concat (tree t) {
  int n= N(t);
  array<tree> r;
  if (!is_concat (t)) r << t;
  else if (n == 0) r << tree ("");
  else if (n == 1) r << t[0];
  else r << t;
  return r;
}

static tree
make_paragraphs (tree t, tree r) {
  int i, n= N(t);
  tree s (CONCAT);
  for (i=0; i<n; i++) {
    if (is_document (r) && t[i] == tree (FORMAT, "new line")) {
      r << clean_paragraph_concat (s);
      s= tree (CONCAT);
    }
    else if (is_func (t[i], BEGIN) && N(t[i]) > 0 && t[i][0] != "document") {
      tree name= t[i][0];
      int start= i++, count= 1;
      while (i<n && count != 0) {
        if (is_func (t[i], BEGIN) && N(t[i]) > 0 && name == t[i][0]) count++;
        if (is_func (t[i], END)   && N(t[i]) > 0 && name == t[i][0]) count--;
        i++;
      }
      if (is_func (t[i-1], END) && N(t[i-1]) > 0 && name == t[i-1][0]) {
        tree tmp= make_paragraphs (t(start+1, i-1), tree (CONCAT));
        s << t[start] << A(tmp) << t[i-1];
      }
      else
        s << t(start, i);
      i--;
    }
    else
      s << t[i];
  }
  if (s != concat ())
    r << clean_paragraph_concat (s);
  return r;
}

static tree
finalize_document (tree t) {
  if (is_atomic (t)) t= tree (CONCAT, t);
  t= finalize_algorithms (t);
  t= finalize_returns  (t);
  t= finalize_pmatrix  (t);
  t= finalize_layout   (t);
  if (!is_func (t, CONCAT)) return tree (DOCUMENT, t);
  t= make_paragraphs (t, tree (DOCUMENT));
  t= finalize_sections (t);
  t= finalize_returns_bis (t);
  return t;
}

bool
is_preamble_command (tree t, tree& doc, string& style) {
  (void) doc;
  if (is_func (t, APPLY, 2)) {
    if (t[0] == "usepackage") return true;
    if ((t[0] == "documentstyle") ||
	(t[0] == "documentclass")) {
      style= get_latex_style (t);
      return true;
    }
  }
  if (is_func (t, APPLY, 3)) {
    if (t[0] == "usepackage*") return true;
    if ((t[0] == "documentstyle*") ||
	(t[0] == "documentclass*")) {
      style= get_latex_style (t);
      return true;
    }
  }
  if (is_func (t, BEGIN, 1) && (t[0] == "document")) return true;
  if (is_func (t, END, 1) && (t[0] == "document")) return true;
  return false;
}

bool
is_bibliography_command (tree t, tree& doc, string& bib_style) {
  if (is_func (t, APPLY, 2)) {
    if (t[0] == "bibliographystyle") {
      bib_style= t[1]->label;
      return true;
    }
    if (t[0] == "bibliography") {
      tree begin (BEGIN, "bibliography");
      tree end (END, "bibliography");
      tree bib;
      begin << "bib" << bib_style << string_arg (t[1]);
      url bbl= get_file_focus ();
      bbl= glue (head (bbl) * basename (bbl, "tex"), "bbl");
      if (exists (bbl))
        bib= bibtex_load_bbl ("bib", bbl);
      else
        bib= tree (DOCUMENT, compound ("bib-list", "[99]", ""));
      doc << begin << bib << end;
      return true;
    }
  }
  return false;
}

tree
finalize_preamble (tree t, string& style) {
  int i, j;
  tree u (DOCUMENT);
  style= "generic";
  string bib_style= "plain";
  for (i=0; i<N(t); i++) {
    if (is_concat (t[i])) {
      tree v (CONCAT);
      for (j=0; j<N(t[i]); j++)
	if (is_preamble_command (t[i][j], v, style));
	else if (is_bibliography_command (t[i][j], v, bib_style));
	else v << t[i][j];
      if (N(v)==1) u << v[0];
      if (N(v)>=2) u << v;
    }
    else if (is_preamble_command (t[i], u, style));
    else if (is_bibliography_command (t[i], u, bib_style));
    else u << t[i];
  }
  return u;
}

/******************************************************************************
* Improper matches
******************************************************************************/

void
handle_improper_matches (tree& r, tree t, int& pos) {
  while (pos < N(t)) {
    if (is_func (t[pos], SET)) {
      tree b= t[pos++]; r << b;
      handle_improper_matches (r, t, pos);
      if ((pos < N(t)) && (t[pos][0] == b[0])) r << t[pos++];
      else {
	r << tree (RESET, copy (b[0]));
	return;
      }
    }
    else if (is_func (t[pos], RESET)) return;
    else r << t[pos++];
  }
}

tree
remove_env_spaces (tree t, bool& done) {
  if (is_atomic (t)) {
    done= done || (t != "" && t != " ");
    return t;
  }
  else if (is_func (t, CONCAT)) {
    array<tree> r;
    for (int i=0; i<N(t); i++)
      if (!done && t[i] == "");
      else if (!done && t[i] == " ");
      else r << remove_env_spaces (t[i], done);
    return simplify_concat (tree (CONCAT, r));
  }
  else if (is_func (t, WITH)) {
    tree r= t (0, N(t));
    r[N(t)-1]= remove_env_spaces (r[N(t)-1], done);
    return r;
  }
  else return t;
}

tree
env_hacks (tree t) {
  int i, n= N(t);
  bool done= false;
  tree beg= remove_env_spaces (t[n-2], done);
  tree end= remove_env_spaces (t[n-1], done);
  if (beg == "") beg= tree (CONCAT);
  else if (!is_concat (beg)) beg= tree (CONCAT, beg);
  if (end == "") end= tree (CONCAT);
  else if (!is_concat (end)) end= tree (CONCAT, end);
  array<tree> ba;
  array<tree> ea;
  for (i=0; i<N(beg); i++)
    if (is_func (beg[i], VSPACE))
      ba << tree (VAR_VSPACE, A(beg[i]));
    else if (is_func (beg[i], PAGE_BREAK))
      ba << tree (VAR_PAGE_BREAK, A(beg[i]));
    else if (is_func (beg[i], NO_PAGE_BREAK))
      ba << tree (VAR_NO_PAGE_BREAK, A(beg[i]));
    else if (is_func (beg[i], NEW_PAGE))
      ba << tree (VAR_NEW_PAGE, A(beg[i]));
    else ba << beg[i];
  for (i=0; i<N(end); i++)
    if (is_func (end[i], NO_INDENT))
      ea << tree (VAR_NO_INDENT, A(end[i]));
    else if (is_func (end[i], YES_INDENT))
      ea << tree (VAR_YES_INDENT, A(end[i]));
    else ea << end[i];
  beg= tree (CONCAT, ba);
  end= tree (CONCAT, ea);
  for (i=N(beg); i>0 && is_func (beg[i-1], RESET, 1); i--) ;
  bool ok= (i<<1) >= N(beg);
  for (int k=0; k<N(beg)-i; k++) {
    ok= ok && is_func (beg[i-k-1], SET, 2) && beg[i-k-1][0] == beg[i+k][0];
    //cout << "Matched " << beg[i-k-1] << " and " << beg[i+k] << "\n";
  }
  if (ok && i<N(beg)) {
    tree r= t (0, n);
    r[n-2]= simplify_concat (beg (0, i));
    r[n-1]= simplify_concat (beg (i, N(beg)) * end);
    //cout << "<< " << t << "\n";
    //cout << ">> " << r << "\n";
    t= r;
  }
  return t;
}

tree
handle_improper_matches (tree t) {
  if (is_atomic (t)) return t;
  else if (is_apply (t, "begingroup") || is_apply (t, "endgroup")) return "";
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= handle_improper_matches (t[i]);
    if (is_concat (r)) {
      int pos;
      tree u (r, 0);
      for (pos=0; pos<N(r); pos++)
	handle_improper_matches (u, r, pos);
      if (N(u)==0) return "";
      if (N(u)==1) return u[0];
      return u;
    }
    else if (is_func (r, ENV) && N(r) >= 2)
      return env_hacks (r);
    return r;
  }
}

static bool
repeat_envs (tree env) {
  return env == FONT_SERIES || env == FONT_SHAPE || env == FONT_SIZE
      || env == FONT_FAMILY || env == COLOR;
}

static array<tree>
get_envs (array< array<tree> > envs, array<tree> done) {
  int i, m, n= N(envs);
  array<tree> r, env;
  if (N(envs) > 0) env= envs[n-1];
  m= N(env);
  for (i=m-1; i>=0; i--)
    if (!contains (env[i][0], done)) {
      r << copy (env[i]);
      done << copy (env[i][0]);
    }
  return r;
}

static void
set_envs (tree e, array< array<tree> > &envs) {
  int i, m, n= N(envs);
  array<tree> env;
  if (N(envs) > 0) env= envs[n-1];
  m= N(env);
  for (i=m-1; i>=0; i--)
    if (env[i][0] == e[0]) {
      envs[n-1][i]= e;
      return;
    }
  if (N(envs) > 0)
    envs[n-1] << e;
  else {
    env << e;
    envs << env;
  }
}

static void
reset_envs (tree e, array< array<tree> > &envs) {
  int i, m, n= N(envs);
  array<tree> env;
  if (N(envs) > 0) env= envs[n-1];
  m= N(env);
  for (i=m-1; i>=0; i--)
    if (env[i][0] == e[0]) {
      env= range (envs[n-1], 0, i);
      env << range (envs[n-1], i+1, m);
      envs[n-1]= env;
      return;
    }
}

static tree
reopen_envs (tree t, array< array<tree> > &envs) {
  if (is_atomic (t)) {
    tree r (CONCAT);
    r << get_envs (envs, array<tree> ());
    if (r != concat ()) {
      r << t;
      return r;
    }
  }
  if (!is_concat (t)) return t;
  bool begin= true;
  int i, n= N(t), m= N(envs);
  tree r (CONCAT);
  array<tree> done;
  int direct_level= 0;
  for (i=0; i<n; i++) {
    if (is_apply (t[i], "begingroup")) {
      array<tree> tmp;
      if (m > 0) tmp= envs[m-1];
      envs << copy (tmp);
      m= N(envs);
      direct_level++;
    }
    else if (is_apply (t[i], "endgroup")) {
      if (m > 0) envs= range (envs, 0, m-1);
      m= N(envs);
      if (direct_level == 0) r << get_envs (envs, array<tree> ());
      direct_level= max (direct_level-1, 0);
    }
    else if (is_func (t[i], SET, 2) && repeat_envs (t[i][0])) {
      r << t[i];
      done << t[i][0];
      set_envs (t[i], envs);
      direct_level= 0;
    }
    else if (is_func (t[i], SET, 2) && t[i][0] == MODE && t[i][1] == "text") {
      r << t[i];
      r << get_envs (envs, array<tree> ());
      direct_level= 0;
    }
    else if (is_func (t[i], RESET, 1)) {
      r << t[i];
      reset_envs (t[i], envs);
      direct_level= 0;
    }
    else if (begin) {
      r << get_envs (envs, done);
      r << t[i];
      begin= false;
    }
    else
      r << t[i];
  }
  return r;
}

tree
reopen_long_matches (tree t) {
  if (!is_document (t)) return t;
  int i, n= N(t);
  tree r (DOCUMENT);
  array< array<tree> > envs;
  for (i=0; i<n; i++) {
    tree tmp= reopen_envs (t[i], envs);
    r << tmp;
  }
  return r;
}

static tree
handle_matches (tree t) {
  t= reopen_long_matches (t);
  t= handle_improper_matches (t);
  return t;
}

/******************************************************************************
* Further finalization after upgrading
******************************************************************************/

tree
float_body (tree t) {
  if (is_atomic (t)) return t;
  else if (is_var_compound (t, "caption", 1)) return "";
  else if (is_var_compound (t, "center", 1)) return float_body (t[N(t)-1]);
  else if (is_var_compound (t, "algorithm", 1)) return float_body (t[N(t)-1]);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= float_body (t[i]);
    if (is_document (r) && (n>0) && (r[n-1] == "")) r= r (0, n-1);
    return r;
  }
}

tree
find_caption (tree t) {
  if (is_atomic (t)) return "";
  else if (is_var_compound (t, "caption", 1)) return t[N(t)-1];
  else {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      tree r= find_caption (t[i]);
      if (r != "") return r;
    }
    return "";
  }
}

tree
finalize_floats (tree t) {
  if (is_atomic (t)) return t;
  else if (is_var_compound (t, "bigfigure", 1)) {
    tree body= float_body (t[N(t)-1]);
    tree capt= find_caption (t[N(t)-1]);
    return tree (make_tree_label ("big-figure"), body, capt);
  }
  else if (is_var_compound (t, "bigtable", 1)) {
    tree body= float_body (t[N(t)-1]);
    tree capt= find_caption (t[N(t)-1]);
    return tree (make_tree_label ("big-table"), body, capt);
  }
  else if (is_var_compound (t, "bigfigure*", 1)) {
    tree body= float_body (t[N(t)-1]);
    tree capt= find_caption (t[N(t)-1]);
    return tree (WITH, "par-columns", "1",
                 tree (make_tree_label ("big-figure"), body, capt));
  }
  else if (is_var_compound (t, "bigtable*", 1)) {
    tree body= float_body (t[N(t)-1]);
    tree capt= find_caption (t[N(t)-1]);
    return tree (WITH, "par-columns", "1",
                 tree (make_tree_label ("big-table"), body, capt));
  }
  else if (is_var_compound (t, "algorithm", 1)) {
    tree body= float_body (t[N(t)-1]);
    tree capt= find_caption (t[N(t)-1]);
    if (capt == "")
      return t;
    else
      return tree (make_tree_label ("specified-algorithm"), capt, body);
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= finalize_floats (t[i]);
    return r;
  }
}

static bool
is_hyper_link (string s) {
  return starts (s, "http://") || starts (s, "https://") || starts (s, "ftp://");
}

tree
finalize_misc (tree t) {
  if (is_atomic (t)) return t;
  // Fixme: to be improved when TeXmacs will allow easy personalisation
  else if (is_compound (t, "enumerate", 2))
    return compound ("enumerate", t[1]);
  else if (is_compound (t, "verbatim", 1) &&
           is_atomic (t[0]) && is_hyper_link (t[0]->label)) {
    return compound ("slink", finalize_misc (t[0]));
  }
  else if (is_func (t, WITH, 3) && t[0] == FONT_FAMILY && t[1] == "tt" &&
           is_atomic (t[2]) && is_hyper_link (t[2]->label)) {
    return compound ("slink", finalize_misc (t[2]));
  }
  else if (is_compound (t, "flushleft", 1) ||
           is_compound (t, "leftaligned", 1))
    return compound ("left-aligned", finalize_misc (t[0]));
  else if (is_compound (t, "flushright", 1) ||
           is_compound (t, "rightaligned", 1))
    return compound ("right-aligned", finalize_misc (t[0]));
  else if (is_compound (t, "acknowledgments", 1))
    return compound ("acknowledgments*", finalize_misc (t[0]));
  else if (is_compound (t, "text", 1) &&
           is_func (t[0], WITH, 3) &&
           t[0][0] == "font-family" &&
           t[0][1] == "rm")
    return compound ("math-up", finalize_misc (t[0][2]));
  else if (is_var_compound (t, "algo-for", 4)) {
    return compound ("algo-for-all",
        finalize_misc (t[N(t)-2]), finalize_misc (t[N(t)-1]));
  }
  else if (is_compound (t, "minipage", 3)) {
    if (is_document (t[2]) && N(t[2]) == 1)
      t[2]= t[2][0];
    return t;
  }
  else {
    int i, n= N(t);
    string l= as_string (L(t));
    // Restore name of users envs, munged in finalize_layout.
    if (latex_type ("\\begin-"*l) == "user" && latex_arity ("\\begin-"*l) < 0
        && N(t) == abs (latex_arity ("\\begin-"*l))+1) {
      tree r= compound (l*"*");
      for (int i=0; i<n; i++)
        r << finalize_misc (t[i]);
      return r;
    }
    if (n > 1 && is_var_compound (t, "algo-if-else-if")
        && ((t[n-1] == "" || t[n-1] == document () || t[n-1] == concat ())))
      n--;
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= finalize_misc (t[i]);
    return r;
  }
}

/******************************************************************************
* Final changes
******************************************************************************/

/**************************** Modernize newlines *****************************/

static tree
clean_concat (tree t) {
  if (!is_concat (t)) return concat (t);
  if (N(t) == 0) return "";
  if (N(t) == 1 && is_atomic (t[0])) return get_label (t[0]);
  if (N(t) == 1 && !is_atomic (t[0])) return t[0];
  return t;
}

static array<tree>
flatten_documents (array<tree> a) {
  array<tree> r;
  for (int i=0; i<N(a); i++) {
    if (is_document (a[i]))
      r << flatten_documents (A(a[i]));
    else r << a[i];
  }
  return r;
}

static tree
make_document (array<tree> a) {
  a= flatten_documents (a);
  if (N(a) == 0) return "";
  else return tree (DOCUMENT, a);
}

static inline bool
contains_document (tree t) {
  for (int i=0; i<N(t); i++)
    if (is_document (t[i])) return true;
  return false;
}

static inline bool
contains_newline (tree t) {
  return contains (compound ("new-line"), A(t));
}

tree
modernize_newlines (tree t, bool skip) {
  if (is_atomic (t)) return t;
  if (is_compound (t, "doc-data") || is_compound (t, "abstract-data"))
    skip= true;
  tree r= tree (L(t));
  for (int i=0; i<N(t); i++)
    r << modernize_newlines (t[i], skip);
  if (is_concat (r)) {
    if (contains_newline (r)) {
      array<tree> tmp=
        tokenize_concat (r, A(concat (compound ("new-line"))), true);
      for (int j=0; j<N(tmp); j++)
        tmp[j]= clean_concat (tmp[j]);
      r= make_document (tmp);
    }
  }
  else if (is_document (r))
    r= make_document (A(r));
  else if (!skip && contains_document (r) && !contains_document (t))
    r= tree (DOCUMENT, r);
  return r;
}

/************************* Eat space around control **************************/

static bool
is_ending_by_space (string s, bool rev) {
  if (N(s) == 0) return false;
  int n= rev? 0 : N(s) - 1;
  return s[n] == ' ';
}

static bool
is_beginning_by_space (string s, bool rev) {
  return is_ending_by_space (s, !rev);
}

static string
remove_next_spaces (string s, bool rev) {
  if (rev) {
    int start= N(s) - 1;
    while (start >= 0 && s[start] == ' ') start--;
    return s(0, start + 1);
  }
  else {
    int start= 0;
    while (start < N(s) && s[start] == ' ') start++;
    return s(start, N(s));
  }
}

static inline bool
is_control (tree t) {
  return is_compound (t, "label") || is_compound (t, "index");
}

static inline bool
is_multiline (tree t) {
  return is_document (t) || is_func (t, ROW) || is_func (t, CELL);
}

static tree
eat_space_around_control (tree t, char &state, bool &ctrl, bool rev) {
  if (is_control (t)) {
    ctrl= true;
    return t;
  }
  if (is_atomic (t)) {
    string s= as_string (t);
    if (rev) {
      if (state == 'n' || state == 's' || ctrl) {
        if (is_beginning_by_space (s, rev))
          s= remove_next_spaces (s, rev);
      }
    }
    else {
      if (state == 's')
        if (is_beginning_by_space (s, rev))
          s= remove_next_spaces (s, rev);
    }
    if (N(s) > 0) {
      state= is_ending_by_space (s, rev)? 's' : '*';
      ctrl= false;
    }
    if (s == as_string (t)) return t;
    else return tree (s);
  }
  ctrl= false;
  int n= N(t);
  int start= rev? n - 1 : 0;
  int inc= rev? -1 : 1;
  array<tree> a;
  if (is_multiline (t)) state= 'n';
  else if (!is_concat (t)) state= '*';
  for (int i=start; i>= 0 && i<n; i+=inc) {
    a << eat_space_around_control (t[i], state, ctrl, rev);
    if (is_multiline (t)) state= 'n';
    else if (!is_concat (t)) state= '*';
  }
  tree r;
  if (rev) r= tree (L(t), reverse (a));
  else r= tree (L(t), a);
  return r;
}

static tree
eat_space_around_control (tree t) {
  bool rev= true, ctrl= false;
  char state= '*';
  t= eat_space_around_control (t, state, ctrl, rev);
  state= '*', rev= false, ctrl= false;
  t= eat_space_around_control (t, state, ctrl, rev);
  return t;
}

/************************ Remove superfluous newlines ************************/

bool
is_verbatim (tree t) {
  return is_compound (t, "cpp-code") || is_compound (t, "mmx-code")   ||
         is_compound (t, "scm-code") || is_compound (t, "shell-code") ||
         is_compound (t, "code")     || is_compound (t, "verbatim")   ||
         is_compound (t, "scilab-code") || is_compound (t, "scala-code") ||
         is_compound (t, "latex_preview") ||
         is_compound (t, "picture-mixed");
}

static tree
remove_superfluous_newlines (tree t) {
  if (is_verbatim (t) || is_atomic (t)) return t;
  if (is_compound (t, "!emptyline")) return "";
  tree r (L(t));
  for (int i=0; i<N(t); i++) {
    if (!is_document (t) || t[i] != "")
      r << remove_superfluous_newlines (t[i]);
    }
  if (is_document (r) && N(r) == 0) r << "";
  return r;
}

/************************** Concat document correct **************************/

tree
concat_document_correct (tree t) {
  if (is_atomic (t)) return t;
  tree r (L(t));
  for (int i=0; i<N(t); i++)
    r << concat_document_correct (t[i]);
  if (is_concat (r) && contains_document (r)) {
    t= r;
    tree tmp (CONCAT);
    r= tree (DOCUMENT);
    for (int i=0; i<N(t); i++) {
      if (is_document (t[i])) {
        if (tmp != concat ()) {
          r << tmp;
          tmp= concat ();
        }
        r << t[i];
      }
      else
        tmp << t[i];
    }
    if (tmp != concat ()) {
      r << tmp;
      tmp= concat ();
    }
    r= make_document (A(r));
  }
  else if (is_document (r) && contains_document (r))
    r= make_document (A(r));
  return r;
}

/****************************** Finalize textm *******************************/

static bool
is_section (tree t) {
  return is_compound (t, "part")     || is_compound (t, "part*")          ||
    is_compound (t, "chapter")       || is_compound (t, "chapter*")       ||
    is_compound (t, "section")       || is_compound (t, "section*")       ||
    is_compound (t, "subsection")    || is_compound (t, "subsection*")    ||
    is_compound (t, "subsubsection") || is_compound (t, "subsubsection*") ||
    is_compound (t, "paragraph")     || is_compound (t, "paragraph*")     ||
    is_compound (t, "subparagraph")  || is_compound (t, "subparagraph*");
}

static bool
is_label (tree t) {
  return is_compound (t, "label");
}

static tree
remove_labels_from_sections (tree t, bool in_section, array<tree> labels) {
  if (is_atomic (t)) return t;
  if (!in_section && is_section (t))
    return remove_labels_from_sections (t, true, labels);
  int i, n= N(t);
  tree r (L(t));
  for (i=0; i<n; i++) {
    if (in_section && is_label (t[i])) labels << t[i];
    else r << remove_labels_from_sections (t[i], in_section, labels);
    if (!in_section && (is_concat (t) || is_document (t)) && N(labels) > 0) {
      r << labels;
      labels= array<tree> ();
    }
  }
  return r;
}

static tree
remove_labels_from_sections (tree t) {
  bool in_section= false;
  array<tree> labels;
  return remove_labels_from_sections (t, in_section, labels);
}

static tree
associate_sections_and_labels (tree t, array<int> &path,
    array<array<int> > &paths, array<tree> &labels, array<int> &sec_path) {
  if (!is_concat (t) && !is_document (t)) {
    sec_path= array<int> ();
    return t;
  }
  int i, n= N(t);
  tree r (L(t));
  path << 0;
  for (i=0; i<n; i++) {
    if (is_section (t[i])) {
      r << t[i];
      sec_path= copy (path);
      path[N(path)-1]++;
    }
    else if (N(sec_path)>0 && is_label (t[i])) {
      labels << t[i];
      paths << sec_path;
    }
    else {
      r << associate_sections_and_labels (t[i], path, paths, labels, sec_path);
      path[N(path)-1]++;
    }
  }
  path= range(path, 0, N(path)-1);
  return r;
}

static tree
insert_label (tree t, tree label, array<int> path) {
  int i= path[0];
  path= range(path, 1, N(path));
  if (N(t) <= i) return t;
  else if (N(path) > 0) t[i]= insert_label (t[i], label, path);
  else if (is_concat (t) || is_document (t))
    t[i]= concat (t[i], label);
  return t;
}

tree
concat_sections_and_labels (tree t) {
  array<int> path, sec_path;
  array<array<int> > paths;
  array<tree> labels;
  t= associate_sections_and_labels (t, path, paths, labels, sec_path);
  int i, n=N(labels);
  for (i=0; i<n; i++)
    t= insert_label (t, labels[i], paths[i]);
  return t;
}

tree
remove_empty_withs (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, WITH)) {
    int n= N(t);
    if (t[n-1] == "" || t[n-1] == document () || t[n-1] == concat ()
        || t[n-1] == document ("") || t[n-1] == concat (""))
      return "";
  }
  int i, n= N(t);
  tree r(L(t));
  for (i=0; i<n; i++) {
    tree tmp= remove_empty_withs (t[i]);
    if (tmp != "" || !is_func (t[i], WITH)) {
      r << tmp;
    }
  }
  return r;
}

tree
merge_successive_withs (tree t, bool force_concat= false) {
  (void) force_concat;
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r(L(t));
  if (!is_concat (t) && !is_document (t)) {
    r= tree (t, n);
    for (i=0; i<n; i++)
      r[i]= merge_successive_withs (t[i]);
  }
  else {
    for (i=0; i<n; i++) {
      if (N(t[i]) > 0 && is_func (t[i], WITH)) {
        tree with= t[i](0, N(t[i])-1);
        int start= i++;
        while (i < n && N(t[i]) > 0 && is_func (t[i], WITH)
            && t[i](0, N(t[i])-1) == with)
          i++;
        if (i == start+1) r << merge_successive_withs (t[start]);
        else {
          tree tmp= t(start,i);
          int j, m= N(tmp);
          for (j=0; j<m; j++) {
            tmp[j]= merge_successive_withs (tmp[j][N(tmp[j])-1]);
          }
          with << concat_document_correct (tmp);
          r << with;
        }
        i--;
      }
      else
        r << merge_successive_withs (t[i]);
    }
  }
  return r;
}

tree
unnest_withs (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, WITH) && N(t) > 0) {
    int n= N(t);
    if (is_func (t[n-1], WITH)) {
      tree r= t(0, n-1);
      r << A(t[n-1]);
      return r;
    }
    else if ((is_func (t[n-1], CONCAT, 1)   && is_func (t[n-1][0], WITH)) ||
             (is_func (t[n-1], DOCUMENT, 1) && is_func (t[n-1][0], WITH))) {
      t[n-1]= t[n-1][0];
      return unnest_withs (t);
    }
  }
  int i, n= N(t);
  tree r(t,n);
  for (i=0; i<n; i++)
    r[i]= unnest_withs (t[i]);
  return r;
}

/****************************** Remove geometry ******************************/

static tree
remove_geometry (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r(L(t));
  for (i=0; i<n; i++)
    if (!is_compound (t[i], "geometry"))
      r << remove_geometry (t[i]);
  return r;
}

/****************************** Finalize textm *******************************/

tree
finalize_textm (tree t) {
  t= remove_geometry (t);
  t= modernize_newlines (t, false);
  t= merge_successive_withs (t);
  t= unnest_withs (t);
  t= remove_empty_withs (t);
  t= nonumber_to_eqnumber (t);
  t= eat_space_around_control (t);
  t= remove_superfluous_newlines (t);
  t= concat_document_correct (t);
  t= remove_labels_from_sections (t);
  t= concat_sections_and_labels (t);
  return simplify_correct (t);
}

/******************************************************************************
* Page geometry
******************************************************************************/

static array<tree>
filter_geometry (tree t) {
  array<tree> r;
  if (is_atomic (t));
  else if (is_compound (t, "geometry", 1)) {
    int i, n= N(t[0]);
    for (i=0; i<n; i++)
      if (is_compound (t[0][i], "associate"))
        r << t[0][i];
  }
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      r << filter_geometry (t[i]);
  }
  return r;
}

/******************************************************************************
* Interface
******************************************************************************/

tree
latex_to_tree (tree t0) {
  // cout << "\n\nt0= " << t0 << "\n\n";
  tree t1= kill_space_invaders (t0);
  string style, lan= "";
  bool is_document= is_compound (t1, "!file", 1);
  if (is_document) t1= t1[0];
  if (is_compound (t1, "!language", 2)) {
    lan= t1[1]->label;
    t1 = t1[0];
  }
  if (!is_document)
    t1= set_special_fonts (t1, lan);
  textm_appendices= false;
  textm_unicode   = false;
  textm_natbib    = false;
  command_type ("!em") = "false";
  // cout << "\n\nt1= " << t1 << "\n\n";
  tree t2= is_document? filter_preamble (t1): t1;
  // cout << "\n\nt2= " << t2 << "\n\n";
  tree t3= parsed_latex_to_tree (t2);
  // cout << "\n\nt3= " << t3 << "\n\n";
  tree t4= finalize_document (t3);
  // cout << "\n\nt4= " << t4 << "\n\n";
  tree t5= is_document? finalize_preamble (t4, style): t4;
  // cout << "\n\nt5= " << t5 << "\n\n";
  tree t6= handle_matches (t5);
  // cout << "\n\nt6= " << t6 << "\n\n";
  if ((!is_document) && is_func (t6, DOCUMENT, 1)) t6= t6[0];
  tree t7= upgrade_tex (t6);
  // cout << "\n\nt7= " << t7 << "\n\n";
  tree t8= finalize_floats (t7);
  // cout << "\n\nt8= " << t8 << "\n\n";
  tree t9= finalize_misc (t8);
  // cout << "\n\nt9= " << t9 << "\n\n";

  tree initial (COLLECTION), mods (WITH);
  if (is_document) initial << filter_geometry (t9);

  tree t10= finalize_textm (t9);
  // cout << "\n\nt10= " << t10 << "\n\n";
  tree t11= drd_correct (std_drd, t10);
  // cout << "\n\nt11= " << t11 << "\n\n";

  if (!exists (url ("$TEXMACS_STYLE_PATH", style * ".ts")))
    style= "generic";

  if (lan != "") {
    initial << tree (ASSOCIATE, LANGUAGE, lan);
    mods << tree (LANGUAGE) << tree (lan);
  }

  string name= "";
  if (lan == "chinese") name= "fireflysung";
  // if (lan == "japanese") name= "ipa";
  // if (lan == "korean") name= "unbatang";
  if (lan == "taiwanese") name= "fireflysung";
  if (lan == "russian") name= "cyrillic";
  if (name != "") {
    textm_unicode = true;
    initial << tree (ASSOCIATE, FONT, name);
    mods << tree (FONT) << tree (name);
  }

  tree t12= t11;
  if (is_document) t12= simplify_correct (t11);
  else if (N (mods) > 0) { t12= mods; t12 << t11; }
  // cout << "\n\nt12= " << t12 << "\n\n";
  tree t13= latex_correct (t12);
  // cout << "\n\nt13= " << t13 << "\n\n";

  if (is_document) {
    tree the_version= compound ("TeXmacs", TEXMACS_VERSION);
    tree the_style  = compound ("style", tuple (style));
    tree the_body   = compound ("body", t13);
    if (textm_natbib)
      the_style= compound ("style", tuple (style, "cite-author-year"));
    if (style != "acmart" && style != "acmsmall" && style != "acmlarge" &&
        style != "acmtog" && style != "sigconf" && style != "sigchi" &&
        style != "sigplan")
      the_style[0] << "std-latex";
    tree r= tree (DOCUMENT, the_version, the_style, the_body);
    if (N (initial) > 0) r << compound ("initial", initial);
    // cout << "\n\nr= " << r << "\n\n";
    return r;
  }
  else return t13;
}

tree
latex_document_to_tree (string s, bool as_pic) {
  tree r;
  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();
  tree t= parse_latex_document (s, true, as_pic);
  if (as_pic) t= latex_fallback_on_pictures (s, t);
  r= latex_to_tree (t);
  command_type ->shorten ();
  command_arity->shorten ();
  command_def  ->shorten ();
  return r;
}
