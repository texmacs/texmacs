
/******************************************************************************
* MODULE     : fromtex.cpp
* DESCRIPTION: conversion of tex strings into texmacs trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "scheme.hpp"
#include "vars.hpp"

tree upgrade_tex (tree t);
static bool textm_appendices= false;

/******************************************************************************
* Preprocess preamble
******************************************************************************/

tree
filter_preamble (tree t) {
  int i, n=N(t);
  bool in_preamble= true;
  tree r (CONCAT);
  array<tree> preamble;

  for (i=0; i<n; i++) {
    tree u= t[i];
    if (in_preamble) {
      if (u == tuple ("\\begin-document")) {
	r << u << preamble;
	in_preamble= false;
      }
      else if (is_tuple (u, "\\documentclass") ||
	       is_tuple (u, "\\documentclass*") ||
	       is_tuple (u, "\\documentstyle") ||
	       is_tuple (u, "\\documentstyle*"))
	r << u;
      else if (is_tuple (u, "\\def") || is_tuple (u, "\\def*") ||
	       is_tuple (u, "\\title") || is_tuple (u, "\\author") ||
	       is_tuple (u, "\\address"))
	preamble << u;
    }
    else r << u;
  }
  if (in_preamble) return t;
  return r;
}

/******************************************************************************
* Transform parsed tex/latex trees into texmacs trees
******************************************************************************/

#define l2e parsed_latex_to_tree
#define t2e parsed_text_to_tree
#define m2e latex_modifier_to_tree
#define var_m2e var_latex_modifier_to_tree
tree l2e (tree);
tree latex_command_to_tree (tree t);

tree
latex_symbol_to_tree (string s) {
  if (s == "") return "";
  if (s[0] == '\\') {
    if (latex_type[s] == "command") {
      if (s == "\\ ") return " ";
      if (s == "\\-") return "";
      if (s == "\\/") return "";
      if (s == "\\i") return "\020";
      if (s == "\\j") return "\021";
      if (s == "\\oe") return "\367";
      if (s == "\\ae") return "\346";
      if (s == "\\ss") return "\377";
      if (s == "\\OE") return "\327";
      if (s == "\\AE") return "\306";
      if (s == "\\SS") return "\337";
      if (s == "\\\\") return tree (FORMAT, "next line");
      if (s == "\\cr") return tree (FORMAT, "next line");
      if (s == "\\noindent")  return tree (FORMAT, "no first indentation");
      if (s == "\\linebreak")  return tree (FORMAT, "line break");
      if (s == "\\newline")  return tree (FORMAT, "new line");
      if (s == "\\nolinebreak")  return tree (FORMAT, "no line break");
      if (s == "\\pagebreak")  return tree (FORMAT, "page break");
      if (s == "\\nopagebreak")  return tree (FORMAT, "no page break after");
      if (s == "\\newpage")  return tree (FORMAT, "new page");
      if (s == "\\newdoublepage")  return tree (FORMAT, "new double page");
      if (s == "\\clearpage")  return tree (FORMAT, "new page");
      if (s == "\\cleardoublepage")  return tree (FORMAT, "new double page");
      if (s == "\\!")  return tree (SPACE, "-0.25spc");
      if (s == "\\,")  return tree (SPACE, "0.25spc");
      if (s == "\\:")  return tree (SPACE, "0.5spc");
      if (s == "\\;")  return tree (SPACE, "0.75spc");
      if (s == "\\*")  return "*";
      if (s == "\\|")  return "<||>";
      if (s == "\\quad")  return tree (SPACE, "1em");
      if (s == "\\qquad")  return tree (SPACE, "2em");
      if (s == "\\par")  return tree (VSPACE, "1fn");
      if (s == "\\smallskip")  return tree (VSPACE, "0.5fn");
      if (s == "\\medskip")  return tree (VSPACE, "1fn");
      if (s == "\\bigskip")  return tree (VSPACE, "2fn");
      if (s == "\\hfill")  return tree (HTAB, "1fn");
      if (s == "\\hline")  return tree (APPLY, "hline");
      if (s == "\\appendix") { textm_appendices= true; return ""; }
      if (s == "\\nolimits") return ""; // temporarily
      if (s == "\\vert") return "|";
      if (s == "\\Vert") return "<||>";
      if (s == "\\notin") return "<nin>";
      if (s == "\\addots") return "<udots>";
    }

    if (latex_type[s] == "texmacs") {
      if (s == "\\tmdummy")  return "";
    }

    if ((latex_type[s] == "modifier") && (latex_arity[s] == 0)) {
      s= s(1,N(s));
      if (s == "rmfamily") return tree (SET, FONT_FAMILY, "rm");
      if (s == "ttfamily") return tree (SET, FONT_FAMILY, "tt");
      if (s == "sffamily") return tree (SET, FONT_FAMILY, "sf");
      if (s == "mdseries") return tree (SET, FONT_SERIES, "medium");
      if (s == "bfseries") return tree (SET, FONT_SERIES, "bold");
      if (s == "upshape")  return tree (SET, FONT_SHAPE , "right");
      if (s == "itshape")  return tree (SET, FONT_SHAPE , "italic");
      if (s == "slshape")  return tree (SET, FONT_SHAPE , "slanted");
      if (s == "scshape")  return tree (SET, FONT_SHAPE , "small-caps");

      if (s == "cal")      return tree (SET, MATH_FONT  , "cal");
      if (s == "frak")     return tree (SET, MATH_FONT  , "Euler");
      if (s == "Bbb")      return tree (SET, MATH_FONT  , "Bbb*");
      if (s == "displaystyle") return tree (SET, MATH_DISPLAY, "true");
      if (s == "textstyle") return tree (SET, MATH_DISPLAY, "false");
      if (s == "scriptstyle") return tree (SET, MATH_LEVEL, "1");
      if (s == "scriptscriptstyle") return tree (SET, MATH_LEVEL, "2");
      if (s == "operatorname") return tree (SET, "dummy", "dummy");
      if (s == "boldsymbol") return tree (SET, MATH_FONT_SERIES, "bold");

      if (s == "rm") return tree (SET, FONT_FAMILY, "rm");
      if (s == "tt") return tree (SET, FONT_FAMILY, "tt");
      if (s == "sf") return tree (SET, FONT_FAMILY, "ss");
      if (s == "md") return tree (SET, FONT_SERIES, "right");
      if (s == "bf") return tree (SET, FONT_SERIES, "bold");
      if (s == "it") return tree (SET, FONT_SHAPE, "italic");
      if (s == "sl") return tree (SET, FONT_SHAPE, "slanted");
      if (s == "sc") return tree (SET, FONT_SHAPE, "small-caps");
      if (s == "em") {
	if (command_type ["!em"] == "false") {
	  command_type ("!em")= "true";
	  return tree (SET, FONT_SHAPE, "italic");
	}
	else {
	  command_type ("!em")= "false";
	  return tree (SET, FONT_SHAPE, "right");
	}
      }

      if (s == "tiny") return tree (SET, FONT_SIZE, "0.59");
      if (s == "scriptsize") return tree (SET, FONT_SIZE, "0.71");
      if (s == "footnotesize") return tree (SET, FONT_SIZE, "0.71");
      if (s == "small") return tree (SET, FONT_SIZE, "0.84");
      if (s == "normalsize") return tree (SET, FONT_SIZE, "1");
      if (s == "large") return tree (SET, FONT_SIZE, "1.19");
      if (s == "Large") return tree (SET, FONT_SIZE, "1.41");
      if (s == "LARGE") return tree (SET, FONT_SIZE, "1.41");
      if (s == "huge") return tree (SET, FONT_SIZE, "1.68");
      if (s == "Huge") return tree (SET, FONT_SIZE, "2");
      
      if (s == "black") return tree (SET, COLOR, "black");
      if (s == "white") return tree (SET, COLOR, "white");
      if (s == "grey") return tree (SET, COLOR, "grey");
      if (s == "red") return tree (SET, COLOR, "red");
      if (s == "blue") return tree (SET, COLOR, "blue");
      if (s == "yellow") return tree (SET, COLOR, "yellow");
      if (s == "green") return tree (SET, COLOR, "green");
      if (s == "orange") return tree (SET, COLOR, "orange");
      if (s == "magenta") return tree (SET, COLOR, "magenta");
      if (s == "brown") return tree (SET, COLOR, "brown");
      if (s == "pink") return tree (SET, COLOR, "pink");

      cerr << "The symbol was " << s << "\n";
      fatal_error ("unexpected situation", "latex_symbol_to_tree");
    }
    if (latex_type[s] == "operator")
      return s(1,N(s));
    if (latex_type[s] == "control") return s(1,N(s));
    if ((s == "\\ldots") && (command_type ("!mode") != "math")) return "...";
    if (latex_type[s] == "symbol")  return "<" * s(1,N(s)) * ">";
    if (latex_type[s] == "big-symbol") {
      if (s == "\\bignone") return tree (BIG, ".");
      else if (s(0,4)=="\\big") return tree (BIG, s(4,N(s)));
      else return tree (BIG, s(1,N(s)));
    }

    if ((N(s) > 7) && (s(0,7) == "\\begin-"))
      return tree (BEGIN, s(7,N(s)));
    if ((N(s) > 5) && (s(0,5) == "\\end-"))
      return tree (END, s(5,N(s)));

    return tree (APPLY, s(1,N(s)));
  }
  if ((N(s) == 2) && (s[0] == '#') && (s[1] >= '0') && (s[1] <= '9'))
    return tree (APPLY, s(1,2));
  if (s == "&") return tree (FORMAT, "line separator");
  return copy (s);
}

tree
t2e (tree t, bool flag= true) {
  string old_mode= command_type ["!mode"];
  command_type ("!mode") = "text";
  tree r= l2e (t);
  command_type ("!mode") = old_mode;
  while (flag && (arity(r)>0)) r= r[0];
  return r;
}

bool
test_alpha_on_end (tree t) {
  if (is_atomic (t) && (N(t->label) >= 1))
    return is_alpha (t->label[N(t->label)-1]);
  if (is_concat (t) && (N(t)>=1))
    return test_alpha_on_end (t[N(t)-1]);
  return false;
}

tree
latex_concat_to_tree (tree t, bool& new_flag) {
  int i, n=N(t);
  tree r (CONCAT), env (CONCAT);

  if ((n > 0) && (command_type ["!mode"] == "math") &&
      (is_tuple (t[0], "\\rm", 0) || is_tuple (t[0], "\\tt", 0)))
    {
      command_type ("!mode") = "text";
      tree u= latex_concat_to_tree (t, new_flag);
      command_type ("!mode") = "math";
      return tree (CONCAT, tree (SET, MODE, "text"), u, tree (RESET, MODE));
    }

  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();

  for (i=0; i<n; i++) {
    if (is_tuple (t[i]) && (N(t[i])==1)) {
      string s= t[i][0]->label;
      if (latex_type[s] == "math-environment") {
	if (s(0,4)=="\\end") command_type ("!mode") = "text";
	else command_type ("!mode") = "math";
      }
      if (s == "\\begin-verbatim") command_type ("!verbatim") = "true";
      else if (s == "\\end-verbatim") command_type ("!verbatim") = "false";
    }
    if (is_atomic (t[i]) && (command_type["!verbatim"] == "true")) {
      r << tm_encode (t[i]->label);
      continue;
    }

    bool operator_flag=
      is_tuple (t[i]) && (N(t[i])==1) &&
      (latex_type[t[i][0]->label]=="operator");
    bool cc_flag= is_concat (t[i]);
    tree u= (cc_flag? latex_concat_to_tree (t[i], new_flag): l2e (t[i]));
    if (is_atomic (u)) {
      if (u == " ") {
	if (command_type ["!mode"] == "math") {
	  if ((i==0) || (!is_tuple (t[i-1])) || (N(t[i-1])!=1) ||
	      (latex_type[t[i-1][0]->label] != "operator"))
	    continue;
	}
	else {
	  if ((t[i] != tree (TUPLE, "\\ ")) && (i>0) && (is_tuple (t[i-1]))) {
	    string s= t[i-1][0]->label;
	    if ((s[0]=='\\') && (latex_type[s]=="command") &&
		(s!="\\end-math") && (s!="\\end-displaymath"))
	      if ((arity(t[i-1])==1) || (s=="\\label"))
		continue;
	  }
	}
      }

      string s= u->label;
      bool old_flag= new_flag;
      if (!cc_flag) new_flag= ((N(s)==1) && is_alpha(s));
      if ((command_type ["!mode"] == "math") &&
	  (!cc_flag) && old_flag && (new_flag || operator_flag)) s= "*" * s;
      if ((N(r)>0) && is_atomic (r[N(r)-1])) r[N(r)-1]->label << s;
      else r << s;
    }
    else {
      if (is_func (u, SET, 2)) {
	env << u;
	if (((i+1)<n) && (t[i+1]==tree(" "))) i++;
      }
      r << u;
      if (!cc_flag) new_flag= false;
    }
  }
  
  for (i=N(env)-1; i>=0; i--)
    r << tree (RESET, copy (env[i][0]));

  command_type ->shorten ();
  command_arity->shorten ();
  command_def  ->shorten ();

  if (N(r)==0) return "";
  if (N(r)==1) return r[0];
  return r;
}

tree
m2e (tree t, string var, string val) {
  return tree (CONCAT,
	       tree (SET, copy (var), copy (val)),
	       l2e (t[1]),
	       tree (RESET, copy (var)));
}

tree
var_m2e (tree t, string var, string val) {
  return tree (CONCAT,
	       tree (SET, copy (var), copy (val)),
	       t2e (t[1], false),
	       tree (RESET, copy (var)));
}

tree
latex_verbarg_to_tree (tree t) {
  t= l2e (t);
  if (is_concat (t)) {
    string s;
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_atomic (t[i])) s << t[i]->label;
      else if (is_func (t[i], RSUB, 1) && is_atomic (t[i][0]))
	s << "_" << t[i][0]->label;
    return s;
  }
  else return "";
}

static bool
is_left_type (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return
    (s == "(") || (s == "[") || (s == "\\{") ||
    (s == "\\lfloor") || (s == "\\lceil") || (s == "\\langle");
}

static bool
is_right_type (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return (s == ")") || (s == "]") || (s == "\\}") ||
    (s == "\\rfloor") || (s == "\\rceil") || (s == "\\rangle");
}

static bool
is_large_delimiter (tree t, int& type) {
  if ((!is_tuple (t)) || (N(t) != 2) || is_compound (t[0])) return false;
  string s= t[0]->label;
  if (starts (s, "\\Big")) s= "\\big" * s(4,N(s));
  if (starts (s, "\\bigg")) s= "\\big" * s(5,N(s));
  if ((s == "\\left") || (s == "\\bigl") ||
      ((s == "\\big") && is_left_type (t[1]))) {
    type= -1;
    return true;
  }
  if ((s == "\\right") || (s == "\\bigr") ||
      ((s == "\\big") && is_right_type (t[1]))) {
    type= 1;
    return true;
  }
  if (s == "\\bigm") {
    type= 0;
    return true;
  }
  return false;
}

tree
latex_cite_to_tree (string cite_type, string s) {
  tree r (APPLY, cite_type);
  int i, last, n=N(s);
  for (last=0, i=0; i<n; i++) {
    while ((i<n) && (s[i]!=',')) i++;
    r << s (last, i);
    if (i<n) i++;
    while ((i<n) && (s[i]==' ')) i++;
    last= i;
  }
  if (N(r) == 1) return "";
  return r;
}

tree
latex_index_to_tree (string s) {
  int i, start, n= N(s);
  array<tree> a (0);
  for (start= i= 0; i<n; i++)
    if (s[i] == '!' && N(a) < 3) {
      a << tree (s (start, i));
      start= i+1;
    }
  a << tree (s (start, i));
  if (N(a) == 1) return compound ("index", a);
  if (N(a) == 2) return compound ("subindex", a);
  if (N(a) == 3) return compound ("subsubindex", a);
  return compound ("subsubsubindex", a);
}

tree
latex_command_to_tree (tree t) {
  if (is_tuple (t, "\\def", 2)) {
    string var= as_string (t[1]);
    if ((N(var)>0) && (var[0]=='\\')) var= var (1, N(var));
    return tree (ASSIGN, var, tree (FUNC, l2e (t[2])));
  }
  if (is_tuple (t, "\\def*", 3)) {
    string var= as_string (t[1]);
    if ((N(var)>0) && (var[0]=='\\')) var= var (1, N(var));
    int i, arity= as_int (l2e(t[2]));
    tree f (FUNC);
    for (i=1; i<=arity; i++) f << as_string (i);
    f << l2e (t[3]);
    return tree (ASSIGN, var, f);
  }

  if (is_tuple (t, "\\newenvironment", 3)) {
    string var= l2e(t[1])->label;
    return tree (ASSIGN, var, tree (ENV, l2e (t[2]), l2e (t[3])));
  }
  if (is_tuple (t, "\\newenvironment*", 4)) {
    string var= l2e(t[1])->label;
    int i, arity= as_int (l2e(t[2])->label);
    tree e (ENV);
    for (i=1; i<=arity; i++) e << as_string (i);
    e << l2e (t[3]);
    e << l2e (t[4]);
    return tree (ASSIGN, var, e);
  }

  if (is_tuple (t, "\\setcounter", 2)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    else return tree (ASSIGN, u->label * "nr", l2e (t[2]));
  }
  if (is_tuple (t, "\\arabic", 1)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    return tree (APPLY, u->label * "nr");
  }
  if (is_tuple (t, "\\equal", 2))
    return tree (EQUAL, l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\ifthenelse", 3))
    return tree (IF, l2e (t[1]), l2e (t[2]), l2e (t[3]));

  if (textm_appendices && is_tuple (t, "\\chapter", 1))
    return tree (APPLY, "appendix", l2e (t[1]));

  if (is_tuple (t, "\\textrm", 1)) return m2e (t, FONT_FAMILY, "rm");
  if (is_tuple (t, "\\texttt", 1)) return m2e (t, FONT_FAMILY, "tt");
  if (is_tuple (t, "\\textsf", 1)) return m2e (t, FONT_FAMILY, "ss");
  if (is_tuple (t, "\\textmd", 1)) return m2e (t, FONT_SERIES, "medium");
  if (is_tuple (t, "\\textbf", 1)) return m2e (t, FONT_SERIES, "bold");
  if (is_tuple (t, "\\textup", 1)) return m2e (t, FONT_SHAPE, "right");
  if (is_tuple (t, "\\textit", 1)) return m2e (t, FONT_SHAPE, "italic");
  if (is_tuple (t, "\\textsl", 1)) return m2e (t, FONT_SHAPE, "slanted");
  if (is_tuple (t, "\\textsc", 1)) return m2e (t, FONT_SHAPE, "small-caps");
  if (is_tuple (t, "\\emph", 1))   return m2e (t, FONT_SHAPE, "italic");
  if (is_tuple (t, "\\operatorname", 1))
    return var_m2e (t, MATH_FONT_FAMILY, "rm");
  if (is_tuple (t, "\\boldsymbol", 1))
    return var_m2e (t, MATH_FONT_SERIES, "bold");
  if (is_tuple (t, "\\mathnormal", 1)) return m2e (t, MATH_FONT_FAMILY, "mr");
  if (is_tuple (t, "\\mathrm", 1)) return var_m2e (t, MATH_FONT_FAMILY, "rm");
  if (is_tuple (t, "\\mathtt", 1)) return var_m2e (t, MATH_FONT_FAMILY, "tt");
  if (is_tuple (t, "\\mathsf", 1)) return var_m2e (t, MATH_FONT_FAMILY, "ss");
  if (is_tuple (t, "\\mathbf", 1)) return var_m2e (t, MATH_FONT_FAMILY, "bf");
  if (is_tuple (t, "\\mathit", 1)) return var_m2e (t, MATH_FONT_FAMILY, "it");
  if (is_tuple (t, "\\mathsl", 1)) return var_m2e (t, MATH_FONT_FAMILY, "sl");
  if (is_tuple (t, "\\mathup", 1)) return var_m2e (t, MATH_FONT_FAMILY, "up");
  if (is_tuple (t, "\\mathcal", 1))  return m2e (t, MATH_FONT, "cal");
  if (is_tuple (t, "\\mathfrak", 1)) return m2e (t, MATH_FONT, "Euler");
  if (is_tuple (t, "\\mathbb", 1))   return m2e (t, MATH_FONT, "Bbb");
  if (is_tuple (t, "\\mathbbm", 1))   return m2e (t, MATH_FONT, "Bbb*");

  if (is_tuple (t, "\\prime", 1))  return tree (RPRIME, as_string (t[1]));
  if (is_tuple (t, "\\frac", 2))  return tree (FRAC, l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\sqrt", 1))  return tree (SQRT, l2e (t[1]));
  if (is_tuple (t, "\\sqrt*", 2)) return tree (SQRT, l2e (t[2]), l2e (t[1]));
  if (is_tuple (t, "\\<sub>", 1)) return tree (RSUB, l2e (t[1]));
  if (is_tuple (t, "\\not", 1))   return tree (NEG, l2e (t[1]));
  if (is_tuple (t, "\\bar", 1))  return tree (WIDE, l2e (t[1]), "<bar>");
  if (is_tuple (t, "\\hat", 1))  return tree (WIDE, l2e (t[1]), "^");
  if (is_tuple (t, "\\tilde", 1))  return tree (WIDE, l2e (t[1]), "~");
  if (is_tuple (t, "\\widehat", 1))  return tree (WIDE, l2e (t[1]), "^");
  if (is_tuple (t, "\\widetilde", 1))  return tree (WIDE, l2e (t[1]), "~");
  if (is_tuple (t, "\\dot", 1))  return tree (WIDE, l2e (t[1]), "<dot>");
  if (is_tuple (t, "\\ddot", 1))  return tree (WIDE, l2e (t[1]), "<ddot>");
  if (is_tuple (t, "\\dddot", 1))  return tree (WIDE, l2e (t[1]), "<dddot>");
  if (is_tuple (t, "\\ddddot", 1))  return tree (WIDE, l2e (t[1]), "<ddddot>");
  if (is_tuple (t, "\\check", 1))  return tree (WIDE, l2e (t[1]), "<check>");
  if (is_tuple (t, "\\grave", 1))  return tree (WIDE, l2e (t[1]), "<grave>");
  if (is_tuple (t, "\\acute", 1))  return tree (WIDE, l2e (t[1]), "<acute>");
  if (is_tuple (t, "\\vec", 1))  return tree (WIDE, l2e (t[1]), "<vect>");
  if (is_tuple (t, "\\breve", 1))  return tree (WIDE, l2e (t[1]), "<breve>");
  if (is_tuple (t, "\\abovering", 1))
    return tree (WIDE, l2e (t[1]), "<abovering>");
  if (is_tuple (t, "\\hspace", 1) || is_tuple (t, "\\hspace*", 1)) {
    tree r= t2e (t[1]);
    if (is_compound (r, "fill", 0)) return tree (HTAB, "1fn");
    return tree (SPACE, r);
  }
  if (is_tuple (t, "\\vspace", 1) || is_tuple (t, "\\vspace*", 1))
    return tree (VSPACE, t2e (t[1]));
  if (is_tuple (t, "\\label", 1)) return tree (LABEL, t2e (t[1]));
  if (is_tuple (t, "\\ref", 1))   return tree (REFERENCE, t2e (t[1]));
  if (is_tuple (t, "\\mathop", 1)) return l2e (t[1]);
  if (is_tuple (t, "\\mathrel", 1)) return l2e (t[1]);
  if (is_tuple (t, "\\overbrace", 1))
    return tree (WIDE, l2e (t[1]), "<wide-overbrace>");
  if (is_tuple (t, "\\underbrace", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<wide-underbrace>");

  if (is_tuple (t, "\\text", 1) ||
      is_tuple (t, "\\mbox", 1) || is_tuple (t, "\\hbox", 1))
    return var_m2e (t, MODE, "text");

  if (is_tuple (t, "\\<sup>", 1)) {
    if (is_tuple (t[1], "\\prime", 0))
      return tree (RPRIME, "'");
    else return tree (RSUP, l2e (t[1]));
  }
  if (is_tuple (t, "\\stackrel", 2))
    return tree (ABOVE, l2e (t[2]), l2e (t[1]));

  int dtype= 0;
  if (is_large_delimiter (t, dtype)) {
    string s= t[1]->label;
    if ((N(s)>1) && (s[0]=='\\')) s=s(1,N(s));
    if (s == "vert") s= "|";
    if (s == "Vert") s= "||";
    if (dtype == -1) return tree (LEFT, s);
    else if (dtype == 1) return tree (RIGHT, s);
    else return tree (MID, s);
  }
  if (is_tuple (t, "\\cite", 1) || is_tuple (t, "\\nocite", 1)) {
    string cite_type= t[0]->label (1, N(t[0]->label));
    string s= as_string (t2e(t[1]));
    return latex_cite_to_tree (cite_type, s);
  }
  if (is_tuple (t, "\\cite*", 2)) {
    tree   ot= t2e(t[1])->label;
    string s = as_string (t2e(t[2]));
    tree   ct= latex_cite_to_tree ("cite", s);
    return tree (CONCAT, ct, " (", ot, ")");
  }
  if (is_tuple (t, "\\index", 1)) {
    string s= as_string (t2e (t[1]));
    return latex_index_to_tree (s);
  }
  if (is_tuple (t, "\\displaylines", 1)) {
    tree u= l2e (t[1]);
    return tree (CONCAT, tree (BEGIN, "matrix"), u, tree (END, "matrix"));
  }
  if (is_tuple (t, "\\cases", 1)) {
    tree u= l2e (t[1]);
    tree r= tree (CONCAT);
    r << tree (LEFT, "\{") << tree (BEGIN, "array", "lll") << u
      << tree (END, "array") << tree (RIGHT, ".");
    return r;
  }
  if (is_tuple (t, "\\includegraphics", 1)) {
    tree name= latex_verbarg_to_tree (t[1]);
    if (name == "") return "";
    else {
      tree g (POSTSCRIPT, 7);
      g[0]= name;
      return g;
    }
  }

  // Start TeXmacs specific markup
  if (is_tuple (t, "\\tmmathbf", 1))
    return tree (CONCAT,
		 tree (SET, MATH_FONT_SERIES, "bold"),
		 l2e (t[1]),
		 tree (RESET, MATH_FONT_SERIES));
  if (is_tuple (t, "\\tmop", 1)) return tree (APPLY, "op", l2e (t[1]));
  if (is_tuple (t, "\\tmstrong", 1)) return tree (APPLY, "strong", l2e (t[1]));
  if (is_tuple (t, "\\tmem", 1)) return tree (APPLY, "em", l2e (t[1]));
  if (is_tuple (t, "\\tmtt", 1)) return tree (APPLY, "tt", l2e (t[1]));
  if (is_tuple (t, "\\tmname", 1)) return tree (APPLY, "name", l2e (t[1]));
  if (is_tuple (t, "\\tmsamp", 1)) return tree (APPLY, "samp", l2e (t[1]));
  if (is_tuple (t, "\\tmabbr", 1)) return tree (APPLY, "abbr", l2e (t[1]));
  if (is_tuple (t, "\\tmdfn", 1)) return tree (APPLY, "dfn", l2e (t[1]));
  if (is_tuple (t, "\\tmkbd", 1)) return tree (APPLY, "kbd", l2e (t[1]));
  if (is_tuple (t, "\\tmvar", 1)) return tree (APPLY, "var", l2e (t[1]));
  if (is_tuple (t, "\\tmacronym", 1))
    return tree (APPLY, "acronym", l2e (t[1]));
  if (is_tuple (t, "\\tmperson", 1)) return tree (APPLY, "person", l2e (t[1]));
  if (is_tuple (t, "\\tmscript", 1)) return l2e (t[1]);
  if (is_tuple (t, "\\tmhlink", 1))
    return tree (HLINK, l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\tmaction", 1))
    return tree (ACTION, l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\foldtext", 2))
    return compound ("fold-text", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\foldproof", 2))
    return compound ("fold-proof", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\foldalgorithm", 2))
    return compound ("fold-algorithm", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\foldexercise", 2))
    return compound ("fold-exercise", l2e (t[1]), l2e (t[2]));
  // End TeXmacs specific markup

  int i;
  string s= t[0]->label;
  tree r (APPLY, s(1,N(s)));
  if ((N(s)>7) && (s(0,7)=="\\begin-"))
    r= tree (BEGIN, s(7,N(s)));
  for (i=1; i<N(t); i++)
    r << l2e(t[i]);
  return r;
}

tree
l2e (tree t) {
  if (is_atomic (t)) return latex_symbol_to_tree (t->label);
  if (L(t) == CONCAT) {
    bool new_flag= false;
    return latex_concat_to_tree (t, new_flag);
  }
  if (is_tuple (t) && (N(t)==1)) return latex_symbol_to_tree (t[0]->label);
  return latex_command_to_tree (t);
}

#undef var_m2e
#undef m2e
#undef t2e
#undef l2e

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
parse_matrix_params (tree t) {
  tree tformat (TFORMAT);
  if (N(t) <= 1) return tformat;
  string s= as_string (t[1]);
  int i, n= N(s), col=1;
  for (i=0; i<n; i++) {
    switch (s[i]) {
    case 'l':
    case 'c':
    case 'r':
      {
	string col_s = as_string (col);
	string halign= copy (CELL_HALIGN);
	string how   = s (i, i+1);
	tformat << tree (CWITH, "1", "-1", col_s, col_s, halign, how);
	col++;
	break;
      }
    case '|':
      {
	string col_s= col==1? as_string (col): as_string (col-1);
	string hbor = col==1? copy (CELL_LBORDER): copy (CELL_RBORDER);
	string how  = "1ln";
	tformat << tree (CWITH, "1", "-1", col_s, col_s, hbor, how);
	break;
      }
    case '@':
      // FIXME: emergency exit; parameters of @ no longer between {}
      return tformat;
    }
  }
  return tformat;
}

static void
parse_pmatrix (tree& r, tree t, int& i, bool bracket) {
  tree tformat= parse_matrix_params (t[i]);
  if (bracket) r << tree (LEFT, "(");

  int rows=0, cols=0;
  tree V (CONCAT);
  tree L (CONCAT);
  tree E (CONCAT);
  for (i++; i<N(t); i++) {
    tree v= t[i];
    if (v == tree (FORMAT, "line separator")) {
      L << simplify_concat (E);
      E= tree (CONCAT);
      continue;
    }
    else if (v == tree (FORMAT, "next line")) {
      L << simplify_concat (E);
      V << L;
      cols= max (cols, N(L));
      L= tree (CONCAT);
      E= tree (CONCAT);
      continue;
    }
    else if (is_func (v, BEGIN) && ((v[0] == "array") ||(v[0] == "tabular"))) {
      parse_pmatrix (E, t, i, false);
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "matrix")) {
      parse_pmatrix (E, t, i, false);
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (BEGIN, "pmatrix")) {
      parse_pmatrix (E, t, i, true);
      if (i<N(t)) continue;
      break;
    }
    else if (v == tree (END, "array")) break;
    else if (v == tree (END, "tabular")) break;
    else if (v == tree (END, "matrix")) break;
    else if (v == tree (END, "pmatrix")) break;
    else if (v == tree (APPLY, "hline")) {
      int    row  = N(V)+ (N(L)==0? 0: 1);
      string row_s= row==0? as_string (row+1): as_string (row);
      string vbor = row==0? copy (CELL_TBORDER): copy (CELL_BBORDER);
      string how  = "1ln";
      tformat << tree (CWITH, row_s, row_s, "1", "-1", vbor, how);
    }
    else E << v;
  }
  if ((N(L)>0) || (N(E)>0)) {
    L << simplify_concat (E);
    V << L;
  }
  if ((max (cols, N(L)) * N(V)) == 0) {
    L= tree (CONCAT, "");
    V= tree (CONCAT, tree (CONCAT, ""));
  }
  cols= max (cols, N(L));
  rows= N(V);

  int x, y;
  tree M (OLD_MATRIX);
  for (y=0; y<rows; y++)
    for (x=0; x<cols; x++)
      if (x<N(V[y])) M << V[y][x];
      else M << "";
  M << as_string (cols) << as_string (rows);
  if (tformat != tree (TFORMAT)) {
    tformat << M;
    M= tformat;
  }
  r << M;
  if (bracket) r << tree (RIGHT, ")");
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
	if (u[i][0] == "array") parse_pmatrix (r, u, i, false);
	else if (u[i][0] == "tabular") parse_pmatrix (r, u, i, false);
	else if (u[i][0] == "matrix") parse_pmatrix (r, u, i, false);
	else if (u[i][0] == "pmatrix") parse_pmatrix (r, u, i, true);
	else r << u[i];
      }
      else r << u[i];
    return r;
  }
  else if (is_func (u, APPLY, 2) && (u[0] == "matrix"))
    return tree (APPLY, "tabular*", u[1]);
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
  if (latex_type["\\begin-" * s] == "list") return true;
  if (latex_type["\\begin-" * s] == "environment") return true;
  if (latex_type["\\begin-" * s] == "math-environment") return true;
  return false;
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
  return s;
}

static tree
finalize_layout (tree t) {
  if (is_atomic (t)) return t;
  int  i, n= N(t);
  tree u (t, n);
  for (i=0; i<n; i++) u[i]= finalize_layout (t[i]);
  if (is_func (u, CONCAT)) {
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

      if (is_func (v, BEGIN, 1) && (v[0] == "picture")) {
	for (; i<n; i++)
	  if (is_func (u[i], POSTSCRIPT)) r << u[i];
	  else if (is_func (u[i], END, 1) && (u[i][0] == "picture"))
	    break;
	continue;
      }

      if (is_func (v, BEGIN) && ((v[0] == "figure") || (v[0] == "figure*"))) {
	r << tree (BEGIN, "bigfigure");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "figure")) {
	r << tree (END, "bigfigure");
	continue;
      }

      if (is_func (v, BEGIN) && ((v[0] == "table") || (v[0] == "table*"))) {
	r << tree (BEGIN, "bigtable");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "table")) {
	r << tree (END, "bigtable");
	continue;
      }

      if (is_func (v, BEGIN, 1) && admissible_env (v)) {
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
	  spc_flag = (t[i-1] == tree (FORMAT, "new line"));
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
	r << tree (BEGIN, translate_list (v[0]->label));
	spc_flag = true;
	item_flag= (latex_type ["\\begin-" * v[0]->label] == "list");
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
	if (((i+1) == N(t)) || (t[i+1] != tree (FORMAT, "new line")))
	  insert_return (r);
	spc_flag = true;
	item_flag= false;
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
finalize_document (tree t) {
  if (is_atomic (t)) t= tree (CONCAT, t);
  t= finalize_returns (t);
  t= finalize_pmatrix (t);
  t= finalize_layout (t);
  if (!is_func (t, CONCAT)) return tree (DOCUMENT, t);

  int i;
  tree r (DOCUMENT);
  for (i=0; i<N(t); i++) {
    int start= i;
    while ((i<N(t)) && (t[i]!=tree (FORMAT, "new line"))) i++;
    if (i==start) r << "";
    else if (i==(start+1)) r << t[start];
    else r << t(start,i);
    if (i==(N(t)-1)) r << "";
  }
  return r;
}

bool
is_preamble_command (tree t, tree& doc, string& style) {
  (void) doc;
  if (is_func (t, APPLY, 2)) {
    if (t[0] == "usepackage") return true;
    if ((t[0] == "documentstyle") ||
	(t[0] == "documentclass")) {
      style= t[1]->label;
      return true;
    }
  }
  if (is_func (t, APPLY, 3)) {
    if (t[0] == "usepackage*") return true;
    if ((t[0] == "documentstyle*") ||
	(t[0] == "documentclass*")) {
      style= t[2]->label;
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
      begin << "bib" << bib_style << t[1]->label;
      doc << begin << end;
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
handle_improper_matches (tree t) {
  if (is_atomic (t)) return t;
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
    return r;
  }
}

/******************************************************************************
* Further finalization after upgrading
******************************************************************************/

tree
float_body (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "caption", 1)) return "";
  else if (is_compound (t, "center", 1)) return float_body (t[0]);
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
  else if (is_compound (t, "caption", 1)) return t[0];
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
  else if (is_compound (t, "bigfigure", 1)) {
    tree body= float_body (t[N(t)-1]);
    tree capt= find_caption (t[N(t)-1]);
    return tree (make_tree_label ("big-figure"), body, capt);
  }
  else if (is_compound (t, "bigtable", 1)) {
    tree body= float_body (t[N(t)-1]);
    tree capt= find_caption (t[N(t)-1]);
    return tree (make_tree_label ("big-table"), body, capt);
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= finalize_floats (t[i]);
    return r;
  }
}

/******************************************************************************
* Final changes programmed in Guile
******************************************************************************/

tree
finalize_textm (tree t) {
  tree u= stree_to_tree (call ("textm-finalize", tree_to_stree (t)));
  return simplify_correct (u);
}

/******************************************************************************
* Interface
******************************************************************************/

tree
latex_to_tree (tree t1) {
  string style;
  bool is_document= is_compound (t1, "!file", 1);
  if (is_document) t1= t1[0];
  textm_appendices= false;
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
  tree t6= handle_improper_matches (t5);
  // cout << "\n\nt6= " << t6 << "\n\n";
  if ((!is_document) && is_func (t6, DOCUMENT, 1)) t6= t6[0];
  tree t7= upgrade_tex (t6);
  // cout << "\n\nt7= " << t7 << "\n\n";
  tree t8= finalize_floats (t7);
  // cout << "\n\nt8= " << t8 << "\n\n";
  tree t9= finalize_textm (t8);
  // cout << "\n\nt9= " << t9 << "\n\n";
  tree t10= simplify_correct (t9);
  // cout << "\n\nt10= " << t10 << "\n\n";
  if (is_document)
    return tree (DOCUMENT, compound ("body", t10), compound ("style", style));
  else return t10;
}
