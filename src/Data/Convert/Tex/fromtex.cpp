
/******************************************************************************
* MODULE     : fromtex.cpp
* DESCRIPTION: conversion of tex strings into texmacs trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "scheme.hpp"
#include "vars.hpp"
#include "tree_correct.hpp"
#include "url.hpp"

tree upgrade_tex (tree t);
bool textm_class_flag= false;
//bool textm_class_flag= true;
static bool textm_appendices= false;
static bool textm_unicode   = false;
static bool textm_natbib    = false;

/*
bool
check_tree (tree t) {
  if (is_atomic (t)) {
    for (int i=0; i<N(t->label); i++)
      if (((int) t->label[i]) == 0) {
	cout << "t= " << t << "\n";
	return true;
      }
  }
  else {
    for (int i=0; i<N(t); i++)
      if (check_tree (t[i])) {
	if (is_atomic (t[i])) cout << "t= " << t << "\n";
	return true;
      }
  }
  return false;
}
*/

static bool
is_var_compound (tree t, string s) {
  return
    is_compound (t, s) ||
    (is_func (t, APPLY) && t[0] == s) ||
    (is_func (t, EXPAND) && t[0] == s);
}

static bool
is_var_compound (tree t, string s, int n) {
  return
    is_compound (t, s, n) ||
    (is_func (t, APPLY, n+1) && t[0] == s) ||
    (is_func (t, EXPAND, n+1) && t[0] == s);
}

static bool
is_apply (tree t, string s) {
  return (L(t) == APPLY) && (N(t) > 0) && (t[0] == s);
}

static bool
is_apply (tree t, string s, int n) {
  return (L(t) == APPLY) && (N(t) == n+1) && (t[0] == s);
}

/******************************************************************************
* Clean extra spaces and linefeed (inspired from TeX tokenizing rules)
******************************************************************************/
bool
is_vertical_space (tree t) {
  return (is_func (t, TUPLE, 2) && t[0] == "\\vspace")     || 
         (is_func (t, TUPLE, 2) && t[0] == "\\vspace*")    || 
         (is_func (t, TUPLE, 1) && t[0] == "\\bigskip")    || 
         (is_func (t, TUPLE, 1) && t[0] == "\\medskip")    || 
         (is_func (t, TUPLE, 1) && t[0] == "\\smallskip");
}

bool
might_not_be_typesetted (tree t) {
  return (is_func (t, TUPLE) && t[0] == "\\\\")              || 
         (is_func (t, TUPLE) && t[0] == "\\author")          || 
         (is_func (t, TUPLE) && t[0] == "\\begin-document")  || 
         (is_func (t, TUPLE) && t[0] == "\\date")            || 
         (is_func (t, TUPLE) && t[0] == "\\declaretheorem")  || 
         (is_func (t, TUPLE) && t[0] == "\\declaretheorem*") || 
         (is_func (t, TUPLE) && t[0] == "\\def")             || 
         (is_func (t, TUPLE) && t[0] == "\\def*")            || 
         (is_func (t, TUPLE) && t[0] == "\\def**")           || 
         (is_func (t, TUPLE) && t[0] == "\\hspace")          || 
         (is_func (t, TUPLE) && t[0] == "\\label")           || 
         (is_func (t, TUPLE) && t[0] == "\\newdef")          || 
         (is_func (t, TUPLE) && t[0] == "\\newenvironment")  || 
         (is_func (t, TUPLE) && t[0] == "\\newenvironment*") || 
         (is_func (t, TUPLE) && t[0] == "\\newenvironment**")|| 
         (is_func (t, TUPLE) && t[0] == "\\newtheorem")      || 
         (is_func (t, TUPLE) && t[0] == "\\newtheorem*")     || 
         (is_func (t, TUPLE) && t[0] == "\\noindent*")       || 
         (is_func (t, TUPLE) && t[0] == "\\setcounter")      || 
         (is_func (t, TUPLE) && t[0] == "\\setlength")       || 
         (is_func (t, TUPLE) && t[0] == "\\maketitle")       || 
         (is_func (t, TUPLE) && t[0] == "\\SetKw")           || 
         (is_func (t, TUPLE) && t[0] == "\\SetKwData")       || 
         (is_func (t, TUPLE) && t[0] == "\\SetKwInOut")      || 
         (is_func (t, TUPLE) && t[0] == "\\SetKwInput")      || 
         (is_func (t, TUPLE) && t[0] == "\\SetKwFunction")   || 
          is_vertical_space (t);
}

bool
is_sectionnal (tree t) {
  return (is_func (t, TUPLE, 2) && t[0] == "\\section")            || 
         (is_func (t, TUPLE, 2) && t[0] == "\\section*")           || 
         (is_func (t, TUPLE, 2) && t[0] == "\\subsection")         || 
         (is_func (t, TUPLE, 2) && t[0] == "\\subsection*")        || 
         (is_func (t, TUPLE, 2) && t[0] == "\\subsubsection")      || 
         (is_func (t, TUPLE, 2) && t[0] == "\\subsubsection*")     || 
         (is_func (t, TUPLE, 2) && t[0] == "\\part")               || 
         (is_func (t, TUPLE, 2) && t[0] == "\\part*")              || 
         (is_func (t, TUPLE, 2) && t[0] == "\\chapter")            || 
         (is_func (t, TUPLE, 2) && t[0] == "\\chapter*")           || 
         (is_func (t, TUPLE) && t[0] == "\\end-thebibliography") || 
         (is_func (t, TUPLE) && t[0] == "\\bibliography");
}

tree
kill_space_invaders (tree t, char &status) {
  // cout << "kill_space_invaders (" << status << "): " << t << LF;
  if (is_atomic (t)) return t;
  if (is_tuple (t)) {
    tree r= copy(t);
    for (int i=1; i<N(t); i++)
      r[i]= kill_space_invaders (t[i], status);
    return r;
  }
  tree r = concat ();
  for (int i=0; i<N(t); i++) {
    tree u= t[i];
    if (is_concat (u)) r << kill_space_invaders (u, status);
    else if (is_tuple (u) && is_sectionnal (u)) {
        if (status != 'N') {
          r << "\n";
          status = 'N';
        }
        r << kill_space_invaders (u, status);
        i++;
        while (i<N(t) && (might_not_be_typesetted (t[i]) ||
             t[i] == " "  || t[i] == "\t" || t[i] == "\n")) {
          if (might_not_be_typesetted (t[i]))
            r << kill_space_invaders (t[i], status);
          i++;
        }
        r << "\n";
        status = 'N';
        i--;
    }
    else {
      switch (status) {
        case 'N':
          if (u == " " || u == "\t" || u == "\n");
          else if (might_not_be_typesetted (u))
            r << u;
          else {
            r << u;
            status = 'M';
          }
          break;
        case 'M':
          r << u;
          if (u == " " || u == "\t") status = 'S';
          if (u == "\n") status = 'N';
          break;
        case 'S':
          if (u == " " || u == "\t");
          else if (u == "\n") {
            r << u;
            status = 'N';
          }
          else if (might_not_be_typesetted (u))
            r << u;
          else {
            r << u;
            status = 'M';
          }
          break;
      }
    }
  }
  return r;
}

tree
kill_space_invaders (tree t) {
  char status = 'N';
  return kill_space_invaders (t, status);
}

/******************************************************************************
* Preprocess preamble
******************************************************************************/

tree
filter_preamble (tree t) {
  int i, n=N(t);
  bool in_preamble= true;
  tree r (CONCAT);
  tree doc (CONCAT);
  tree preamble (CONCAT);
  tree metadata (CONCAT);
  tree latex_classe;

  for (i=0; i<n; i++) {
    tree u= t[i];
    if (in_preamble) {
      if (u == tuple ("\\begin-document")) {
	r << u;
	if (N(preamble) > 0)
	  r << tuple ("\\begin-hide-preamble") << A(preamble)
	    << tuple ("\\end-hide-preamble");
	in_preamble= false;
      }
      else if (is_tuple (u, "\\documentclass") ||
	       is_tuple (u, "\\documentclass*") ||
	       is_tuple (u, "\\documentstyle") ||
	       is_tuple (u, "\\documentstyle*"))
      {
	      doc << u;
        latex_classe = u;
      }
      else if (is_tuple (u, "\\def") ||
	       is_tuple (u, "\\def*") || is_tuple (u, "\\def**"))
	preamble << u << "\n" << "\n";
      else if (is_tuple (u, "\\newdef", 2))
	preamble << tuple("\\newtheorem", u[1], u[2]) << "\n" << "\n";
      else if (is_tuple (u, "\\declaretheorem", 1) ||
          is_tuple (u, "\\declaretheorem*", 2))
	preamble << tuple("\\newtheorem", u[N(u)-1], u[N(u)-1]) << "\n" << "\n";
      else if (is_tuple (u, "\\newtheorem") ||
	       is_tuple (u, "\\newtheorem*"))
	preamble << u << "\n" << "\n";
      else if (is_tuple (u, "\\newenvironment") ||
	       is_tuple (u, "\\newenvironment*")      ||
	       is_tuple (u, "\\newenvironment**"))
	preamble << u << "\n" << "\n";
      else if (is_tuple (u, "\\SetKw", 2)          || 
               is_tuple (u, "\\SetKwData", 2)      || 
               is_tuple (u, "\\SetKwInOut", 2)     || 
               is_tuple (u, "\\SetKwInput", 2)     || 
               is_tuple (u, "\\SetKwFunction", 2))
	preamble << u << "\n" << "\n";
    }
    else if (!is_metadata (u))
      doc << u;
  }
  metadata = collect_metadata (t, latex_classe);
  r << A(kill_space_invaders (metadata));
  // cout << "Parsed metadatas: " << kill_space_invaders (metadata) << "\n";
  r << A(kill_space_invaders (doc));
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
#define v2e latex_verbarg_to_string
tree l2e (tree);
tree latex_command_to_tree (tree t);

tree
latex_symbol_to_tree (string s) {
  if (s == "") return "";
  if (s[0] == '\\') {
    s= s(1,N(s));
    if (latex_type ('\\' * s) == "command") {
      if (s == " ")      return " ";
      if (s == "-")      return "";
      if (s == "/")      return "";
      if (s == "AA")     return "\xC5";
      if (s == "AE")     return "\xC6";
      if (s == "DH")     return "\xD0";
      if (s == "L")      return "\x8A";
      if (s == "NG")     return "\x8D";
      if (s == "O")      return "\xD8";
      if (s == "OE")     return "\xD7";
      if (s == "S")      return "\x9F";
      if (s == "SS")     return "\xDF";
      if (s == "TH")     return "\xDE";
      if (s == "aa")     return "\xE5";
      if (s == "ae")     return "\xE6";
      if (s == "dh")     return "\xF0";
      if (s == "dj")     return "\x9E";
      if (s == "i")      return "\x19";
      if (s == "j")      return "\x1A";
      if (s == "l")      return "\xAA";
      if (s == "ng")     return "\xAD";
      if (s == "o")      return "\xF8";
      if (s == "oe")     return "\xF7";
      if (s == "ss")     return "\xFF";
      if (s == "th")     return "\xFE";
      if (s == "pounds") return "\xBF";
      if (s == "BlankLine")    return "\n";
      if (s == "AND")          return concat (tree (APPLY, "algo-and"), " ");
      if (s == "NOT")          return concat (tree (APPLY, "algo-not"), " ");
      if (s == "OR")           return concat (tree (APPLY, "algo-or"), " ");
      if (s == "XOR")          return concat (tree (APPLY, "algo-xor"), " ");
      if (s == "ENSURE")       return tree (APPLY, "algo-ensure");
      if (s == "Ensure")       return tree (APPLY, "algo-ensure");
      if (s == "FALSE")        return tree (APPLY, "algo-false");
      if (s == "GLOBALS")      return tree (APPLY, "algo-globals");
      if (s == "PRINT")        return tree (APPLY, "algo-print");
      if (s == "REQUIRE")      return tree (APPLY, "algo-require");
      if (s == "Require")      return tree (APPLY, "algo-require");
      if (s == "RETURN")       return tree (APPLY, "algo-return");
      if (s == "STATE")        return tree (APPLY, "algo-state");
      if (s == "State")        return tree (APPLY, "algo-state");
      if (s == "STMT")         return tree (APPLY, "algo-state");
      if (s == "TO")           return tree (APPLY, "algo-to");
      if (s == "KwTo")         return tree (APPLY, "algo-to");
      if (s == "TRUE")         return tree (APPLY, "algo-true");
      if (s == "BODY")         return tree (BEGIN, "algo-body");
      if (s == "INPUTS")       return tree (BEGIN, "algo-inputs");
      if (s == "OUTPUTS")      return tree (BEGIN, "algo-outputs");
      if (s == "ELSE")         return tree (APPLY, "algo-else");
      if (s == "Else")         return tree (APPLY, "algo-else");
      if (s == "LOOP")         return tree (BEGIN, "algo-loop");
      if (s == "Loop")         return tree (BEGIN, "algo-loop");
      if (s == "REPEAT")       return tree (BEGIN, "algo-repeat");
      if (s == "Repeat")       return tree (BEGIN, "algo-repeat");
      if (s == "ENDBODY")      return tree (END, "algo-body");
      if (s == "EndFor")       return tree (END, "algo-for");
      if (s == "EndFunction")  return tree (END, "algo-function");
      if (s == "ENDFOR")       return tree (END, "algo-for");
      if (s == "ENDIF")        return tree (END, "algo-if-else-if");
      if (s == "EndIf")        return tree (END, "algo-if-else-if");
      if (s == "ENDINPUTS")    return tree (END, "algo-inputs");
      if (s == "ENDLOOP")      return tree (END, "algo-loop");
      if (s == "EndLoop")      return tree (END, "algo-loop");
      if (s == "ENDOUTPUTS")   return tree (END, "algo-outputs");
      if (s == "EndProcedure") return tree (END, "algo-procedure");
      if (s == "ENDWHILE")     return tree (END, "algo-while");
      if (s == "EndWhile")     return tree (END, "algo-while");
      if (s == "\\")              return tree (FORMAT, "next line");
      if (s == "cr")              return tree (FORMAT, "next line");
      if (s == "noindent")        return tree (FORMAT, "no first indentation");
      if (s == "linebreak")       return tree (FORMAT, "line break");
      if (s == "newline")         return tree (FORMAT, "new line");
      if (s == "nobreak")         return tree (FORMAT, "no line break");
      if (s == "nolinebreak")     return tree (FORMAT, "no line break");
      if (s == "pagebreak")       return tree (FORMAT, "page break");
      if (s == "nopagebreak")     return tree (FORMAT, "no page break after");
      if (s == "newpage")         return tree (FORMAT, "new page");
      if (s == "newdoublepage")   return tree (FORMAT, "new double page");
      if (s == "clearpage")       return tree (FORMAT, "new page");
      if (s == "cleardoublepage") return tree (FORMAT, "new double page");
      if (s == "strut") return tree (APPLY, "resize", "",
          "0pt", "-0.3bls", "0pt", "0.7bls");
      if (s == "!")          return tree (SPACE, "-0.25spc");
      if (s == ",")          return tree (SPACE, "0.25spc");
      if (s == "thinspace")  return tree (SPACE, "0.25spc");
      if (s == ":")          return tree (SPACE, "0.5spc");
      if (s == "enspace")    return tree (SPACE, "0.5spc");
      if (s == ";")          return tree (SPACE, "0.75spc");
      if (s == "quad")       return tree (SPACE, "1em");
      if (s == "qquad")      return tree (SPACE, "2em");
      if (s == "par")        return tree (VSPACE, "1fn");
      if (s == "smallskip")  return tree (VSPACE, "0.5fn");
      if (s == "medskip")    return tree (VSPACE, "1fn");
      if (s == "bigskip")    return tree (VSPACE, "2fn");
      if (s == "hfil")       return tree (HTAB, "0pt");
      if (s == "hfill")      return tree (HTAB, "0pt");
      if (s == "hfilll")     return tree (HTAB, "0pt");
      if (s == "hline")      return tree (APPLY, "hline");
      if (s == "toprule")    return tree (APPLY, "hline");
      if (s == "midrule")    return tree (APPLY, "hline");
      if (s == "bottomrule") return tree (APPLY, "hline");
      if (s == "appendix") { textm_appendices= true; return ""; }
      if (s == "limits")   return ""; // tree (FORMAT, "with limits");
      if (s == "nolimits") return ""; // temporarily
      if (s == "*")        return "*";
      if (s == "vert")     return "|";
      if (s == "|")        return "<||>";
      if (s == "Vert")     return "<||>";
      if (s == "notin")    return "<nin>";
      if (s == "addots")   return "<udots>";
      if (s == "dots")     return "<ldots>";
      if (s == "infin")    return "<infty>";
      if (s == "rang")     return "<rangle>";
      if (s == "today")    return compound ("date", "");
      if (s == "tableofcontents")
	return compound ("table-of-contents", "toc", tree (DOCUMENT, ""));
      if (s == "bgroup")     return "";
      if (s == "egroup")     return "";
      if (s == "colon")      return ":";
      if (s == "dotsc")      return "<ldots>";
      if (s == "dotsb")      return "<cdots>";
      if (s == "dotsm")      return "<cdots>";
      if (s == "dotsi")      return "<cdots>";
      if (s == "dotso")      return "<ldots>";
      if (s == "lvert")      return "|";
      if (s == "rvert")      return "|";
      if (s == "lVert")      return "<||>";
      if (s == "rVert")      return "<||>";
      if (s == "qed")        return compound ("math", "<Box>");
      if (s == "implies")    return "<Longrightarrow>";
      if (s == "iff")        return "<Longleftrightarrow>";
      if (s == "gets")       return "<leftarrow>";
      if (s == "printindex") return compound ("the-index", "idx", "");
      if (s == "twocolumn")
        return tree (SET, "par-columns", "2");
      if (s == "onecolumn")
        return tree (SET, "par-columns", "1");
    }

    if (latex_type (s) == "symbol") {
      if (s == "lnot")          return "<neg>";
      if (s == "land")          return "<wedge>";
      if (s == "lor")           return "<vee>";
      if (s == "textbackslash") return "\\";
      if (s == "hdots")         return "<ldots>";
      if (s == "arrowvert")     return "|";
      if (s == "Arrowvert")     return "<||>";
      if (s == "lbrace")        return "{";
      if (s == "rbrace")        return "}";
                                return "<" * s * ">";
    }

    if (latex_type (s) == "texmacs") {
      if (s == "tmdummy") return "";
      if (s == "tmbsl")   return "\\";
    }

    if ((latex_type (s) == "modifier") && (latex_arity (s) == 0)) {
      if (s == "centering")
        return tree (SET, PAR_MODE, "center");
      if (s == "raggedright" || s == "flushleft")
        return tree (SET, PAR_MODE, "left");
      if (s == "raggedleft" || s == "flushright")
        return tree (SET, PAR_MODE, "right");

      if (s == "rmfamily") return tree (SET, FONT_FAMILY, "rm");
      if (s == "ttfamily") return tree (SET, FONT_FAMILY, "tt");
      if (s == "sffamily") return tree (SET, FONT_FAMILY, "sf");
      if (s == "mdseries") return tree (SET, FONT_SERIES, "medium");
      if (s == "bfseries") return tree (SET, FONT_SERIES, "bold");
      if (s == "upshape")  return tree (SET, FONT_SHAPE , "right");
      if (s == "itshape")  return tree (SET, FONT_SHAPE , "italic");
      if (s == "slshape")  return tree (SET, FONT_SHAPE , "slanted");
      if (s == "scshape")  return tree (SET, FONT_SHAPE , "small-caps");

      if (s == "cal")              return tree (SET, MATH_FONT  , "cal");
      if (s == "frak")             return tree (SET, MATH_FONT  , "Euler");
      if (s == "Bbb")              return tree (SET, MATH_FONT  , "Bbb*");
      if (s == "displaystyle")     return tree (SET, MATH_DISPLAY, "true");
      if (s == "textstyle")        return tree (SET, MATH_DISPLAY, "false");
      if (s == "scriptstyle")      return tree (SET, MATH_LEVEL, "1");
      if (s == "scriptscriptstyle")return tree (SET, MATH_LEVEL, "2");
      if (s == "operatorname")     return tree (SET, "dummy", "dummy");
      if (s == "boldsymbol")       return tree (SET, MATH_FONT_SERIES, "bold");

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

      if (s == "tiny")         return tree (SET, FONT_SIZE, "0.59");
      if (s == "scriptsize")   return tree (SET, FONT_SIZE, "0.71");
      if (s == "footnotesize") return tree (SET, FONT_SIZE, "0.71");
      if (s == "small")        return tree (SET, FONT_SIZE, "0.84");
      if (s == "normalsize")   return tree (SET, FONT_SIZE, "1");
      if (s == "large")        return tree (SET, FONT_SIZE, "1.19");
      if (s == "Large")        return tree (SET, FONT_SIZE, "1.41");
      if (s == "LARGE")        return tree (SET, FONT_SIZE, "1.41");
      if (s == "huge")         return tree (SET, FONT_SIZE, "1.68");
      if (s == "Huge")         return tree (SET, FONT_SIZE, "2");
      
      if (s == "black")        return tree (SET, COLOR, "black");
      if (s == "white")        return tree (SET, COLOR, "white");
      if (s == "grey")         return tree (SET, COLOR, "grey");
      if (s == "red")          return tree (SET, COLOR, "red");
      if (s == "blue")         return tree (SET, COLOR, "blue");
      if (s == "yellow")       return tree (SET, COLOR, "yellow");
      if (s == "green")        return tree (SET, COLOR, "green");
      if (s == "orange")       return tree (SET, COLOR, "orange");
      if (s == "magenta")      return tree (SET, COLOR, "magenta");
      if (s == "brown")        return tree (SET, COLOR, "brown");
      if (s == "pink")         return tree (SET, COLOR, "pink");

			if (s == "boldmath") return tree (SET, MATH_FONT_SERIES, "bold");

      cerr << "The symbol was " << "\\"*s << "\n";
      FAILED ("unexpected situation");
    }
    
    if (latex_type (s) == "length") {
      if (s == "@vpt")      return  "5"   ;
      if (s == "@vipt")     return  "6"   ;
      if (s == "@viipt")    return  "7"   ;
      if (s == "@viiipt")   return  "8"   ;
      if (s == "@ixpt")     return  "9"   ;
      if (s == "@xpt")      return "10"   ;
      if (s == "@xipt")     return "10.95";
      if (s == "@xiipt")    return "12"   ;
      if (s == "@xivpt")    return "14.4" ;
      if (s == "@xviipt")   return "17.28";
      if (s == "@xxpt")     return "20.74";
      if (s == "@xxvpt")    return "24.88";

      if (s == "@bls")      return "par-sep";
      if (s == "p@")        return "pt";
      if (s == "z@")        return "0pt";

      if (s == "abovedisplayshortskip") return "tex-above-display-short-skip";
      if (s == "abovedisplayskip")      return "tex-above-display-skip";
      if (s == "belowdisplayshortskip") return "tex-below-display-short-skip";
      if (s == "belowdisplayskip")      return "tex-below-display-skip";
      if (s == "columnsep")             return "tex-column-sep";
      if (s == "evensidemargin")        return "tex-even-side-margin";
      if (s == "footnotesep")           return "tex-footnote-sep";
      if (s == "footskip")              return "tex-foot-skip";
      if (s == "headheight")            return "tex-head-height";
      if (s == "headsep")               return "tex-head-sep";
      if (s == "jot")                   return "tex-jot";
      if (s == "marginparwidth")        return "tex-margin-par-width";
      if (s == "mathindent")            return "tex-math-indent";
      if (s == "oddsidemargin")         return "tex-odd-side-margin";
      if (s == "parindent")             return "par-first";
      if (s == "textheight")            return "tex-text-height";
      if (s == "textwidth")             return "tex-text-width";
      if (s == "topmargin")             return "tex-top-margin";
      if (s == "topskip")               return "tex-top-skip";
      if (s == "smallskipamount")       return "0.5fn";
      if (s == "medskipamount")         return "1fn";
      if (s == "bigskipamount")         return "2fn";
    }   

    // FIXME: avoid redefinition of command_type in parsetex.cpp
    if (latex_type (s) == "name" || latex_type (s) == "user") {
      if (s == "abstractname")	  return "abstract-text";
      if (s == "appendixname")	  return "appendix-text";
      if (s == "contentsname")	  return "table-of-contents-text";
      if (s == "figurename")	  return "figure-text";
      if (s == "indexname")	  return "index-text";
      if (s == "listfigurename")  return "list-of-figures-text";
      if (s == "listtablename")	  return "list-of-tables-text";
      if (s == "partname")	  return "part-text";
      if (s == "refname")	  return "bibliography-text";
      if (s == "tablename")	  return "table-text";
    }

		if (latex_type (s) == "ignore") return "";

    if (latex_type (s) == "operator" || latex_type (s) == "control") return s;
    if ((s == "ldots") && (command_type ("!mode") != "math")) return "...";
    if (s == "bignone") return tree (BIG, ".");
    if (s == "Return")  return tree (APPLY, "algo-return");
    if (latex_type (s) == "big-symbol") {
      if (s(0,3)=="big") return tree (BIG, s(3,N(s)));
      else return tree (BIG, s);
    }

    if ((N(s) > 6) && (s(0,6) == "begin-"))
      return tree (BEGIN, s(6,N(s)));
    if ((N(s) > 4) && (s(0,4) == "end-"))
      return tree (END, s(4,N(s)));

    if (starts (s, "#") && s != "#") {
      textm_unicode= true;
      return "<" * s * ">";
    }
    return tree (APPLY, s);
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

string
string_arg (tree t) {
  if (is_atomic (t)) return t->label;
  else if (is_concat (t)) {
    string r;
    int i, n= N(t);
    for (i=0; i<n; i++)
      r << string_arg (t[i]);
    return r;
  }
  else if (is_func (t, RSUB, 1))
    return "_" * string_arg (t[0]);
  else if (is_func (t, RSUP, 1))
    return "^" * string_arg (t[0]);
  else if (is_func (t, APPLY, 1) && t[0] == "nbsp")
    return " ";
  else {
    //cout << "t= " << t << "\n";
    return "";
  }
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
      if (latex_type (s) == "math-environment") {
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
      (latex_type (t[i][0]->label) == "operator");
    bool cc_flag= is_concat (t[i]);
    tree u= (cc_flag? latex_concat_to_tree (t[i], new_flag): l2e (t[i]));
    if (is_atomic (u)) {
      if (u == " ") {
	if (command_type ["!mode"] == "math") {
	  if ((i==0) || (!is_tuple (t[i-1])) || (N(t[i-1])!=1) ||
	      (latex_type (t[i-1][0]->label) != "operator"))
	    continue;
	}
	else {
	  if ((t[i] != tree (TUPLE, "\\ "))) {
	    if (i>0 && is_tuple (t[i-1])) {
	      string s= t[i-1][0]->label;
	      if ((s[0] == '\\') && (latex_type (s) == "command") &&
		  (s!="\\end-math") && (s!="\\end-displaymath"))  {
          r << u;
		if ((arity(t[i-1])==1) || (s=="\\label")) continue;
	      if (starts (s, "\\begin-") &&
		  (command_type["!verbatim"] != "true"))
		continue;
        }
	    }
	    if (i+1<N(t) && is_tuple (t[i+1])) {
	      string s= t[i+1][0]->label;
	      if (starts (s, "\\end-") &&
		  (command_type["!verbatim"] != "true"))
		continue;
	    }
	  }
	}
      }

      string s= u->label;
      if (operator_flag && i+1<n
          && !(is_tuple (t[i+1], "\\<sub>") || is_tuple (t[i+1], "\\<sup>")))
        s << " ";
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

string
v2e (tree t) {
  return string_arg (t2e (t, false));
}

static bool
is_left_type (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return
    (s == "(") || (s == "[") || (s == "\\{") ||
    (s == "\\lvert") || (s == "lVert") ||
    (s == "\\lfloor") || (s == "\\lceil") || (s == "\\langle");
}

static bool
is_right_type (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return
    (s == ")") || (s == "]") || (s == "\\}") ||
    (s == "\\rvert") || (s == "\\rVert") ||
    (s == "\\rfloor") || (s == "\\rceil") || (s == "\\rangle");
}

static bool
is_mid_type (tree t) {
  if (is_compound (t)) return false;
  string s= t->label;
  return
    (s == "|")             || (s == "||")            || (s == "\\|")         || 
    (s == "\\vert")        || (s == "\\Vert")        || (s == "\\lvert")     || 
    (s == "\\rvert")       || (s == "\\lVert")       || (s == "\\rVert")     || 
    (s == "/")             || (s == "\\arrowvert")   || (s == "\\backslash") || 
    (s == "\\Arrowvert")   || (s == "\\bracevert")   || (s == "\\Uparrow")   || 
    (s == "\\downarrow")   || (s == "\\uparrow")     || (s == "\\Downarrow") || 
    (s == "\\updownarrow") || (s == "\\Updownarrow");
}

static bool
is_large_delimiter (tree t, int& type) {
  if ((!is_tuple (t)) || (N(t) != 2) ||
      is_compound (t[0]) || is_compound (t[1])) return false;
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
  if ((s == "\\bigm") ||
      ((s == "\\big") && is_mid_type (t[1]))) {
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
latex_accent (tree t, string acc) {
  return tree (WITH, MODE, "math",
	       tree (WIDE, tree (WITH, MODE, "text", l2e (t)), acc));
}

tree
abs_length(tree t) {
  string s;
  tree r;
  if (is_atomic(t)) {
    s = as_string(t);
    if (s[0] == '-')
      return tree(s(1,N(s)));
    else
      return s;
  }
  else {
    r = tree(L(t));
    for (int i=0 ; i < N(t) ; i++)
      r << abs_length(t[i]);
    return r;
  }
}

tree
find_next_length(tree t) {
  if (is_atomic(t))
    return t;
  else
    if(is_func(t, APPLY)) 
      if (t[0] == "tex-len") 
        return find_next_length (t[1]);
  return tree();
}

bool
is_negative_length(tree t) {
  t = find_next_length(t);
  string s= t->label;
    for (int i=0 ; i < N(s) ; i++){
      if (s[i] == '-') return true;
      else if (is_space(s[i]));
      else if (is_numeric(s[i]) || is_alpha(s[i]) || 
          s[i] == '+' || s[i] == '.' || s[i] == ',') return false;
    }
  return false;
}

tree
latex_eps_get (tree t, string var) {
  if (!is_atomic (t)) return "";
  string s= t->label;
  int start=0, i, n=N(s);
  for (i=0; i <= n; i++)
    if (i == n || s[i] == ',') {
      string ss= s (start, i);
      while (starts (ss, " ")) ss= ss (1, N(ss));
      while (ends (ss, " ")) ss= ss (0, N(ss) - 1);
      int j, k= N(ss);
      for (j=0; j<k; j++)
	if (ss[j] == '=') break;
      string v= ss (0, j);
      while (ends (v, " ")) v= v (0, N(v) - 1);
      if (j < k && v == var) {
	string val= ss (j+1, N(ss));
	while (starts (val, " ")) val= val (1, N(val));
	return val;
      }
      start= i+1;
    }
  return "";
}

tree
latex_command_to_tree (tree t) {
  if (is_tuple (t) && N(t)>1) {
    string s= as_string (t[0]);
    s= s(1,N(s));
    if (latex_type (s) == "ignore")
      return tree ();
  }
  if (is_tuple (t, "\\def", 2)) {
    string var= string_arg (t[1]);
    if ((N(var)>0) && (var[0]=='\\')) var= var (1, N(var));
    if (is_func (t, TUPLE, 3) && is_func (t[2], TUPLE, 1) && 
        latex_type (as_string (t[2][0])) != "undefined"   && 
        latex_arity (as_string (t[2][0])) != 0) {
      string s = as_string (t[2][0]);
      tree f (FUNC), def = tuple (s), l2edef;
      for (int a=1; a <= abs (latex_arity (s)) - (latex_arity (s) < 0); a++){
        f << as_string (a);
        def << tree (APPLY, as_string (a));
      }
      l2edef = l2e (def);
      if (N(l2edef) != 0 && ! is_concat (l2edef))
        f << l2edef;
      else if (is_tuple (def)) {
        tree adef (APPLY);
        if (N(def) > 0 && N(def[0])>0) {
          string cmd = def[0]->label;
          if ((N(cmd)>0) && (cmd[0]=='\\')) cmd= cmd (1, N(cmd));  
          adef << cmd;
        }
        for (int a=1; a < N(def); a++) adef << def[a];
        f << adef;
      }
      else
        f << def;
      if (latex_arity (s) > 0)
        return tree (ASSIGN, var, f);
      else {
        tree r = concat ();
        r << tree (ASSIGN, var, f);
        def = tuple (s * "*");
        var = var * "*";
        f = tree (FUNC);
        for (int a=1; a <= abs (latex_arity (s)); a++){
          f << as_string (a);
          def << tree (APPLY, as_string (a));
        }
        l2edef = l2e (def);
        if (N(l2edef) != 0 && ! is_concat (l2edef))
          f << l2edef;
        else if (is_tuple (def)) {
          tree adef (APPLY);
          if (N(def) > 0 && N(def[0])>0) {
            string cmd = def[0]->label;
            if ((N(cmd)>0) && (cmd[0]=='\\')) cmd= cmd (1, N(cmd));  
            adef << cmd;
          }
          for (int a=1; a < N(def); a++) adef << def[a];
          f << adef;
        }
        else
          f << def;
        r << tree (ASSIGN, var, f);
        return r;
      }
    }
    else {
      return tree (ASSIGN, var, tree (FUNC, l2e (t[2])));
    }
  }
  if (is_tuple (t, "\\def*", 3)) {
    string var= string_arg (t[1]);
    if ((N(var)>0) && (var[0]=='\\')) var= var (1, N(var));
    int i, arity= as_int (l2e(t[2]));
    tree f (FUNC);
    for (i=1; i<=arity; i++) f << as_string (i);
    f << l2e (t[3]);
    return tree (ASSIGN, var, f);
  }
  if (is_tuple (t, "\\def**", 4)) {
    string var= string_arg (t[1]);
    if ((N(var)>0) && (var[0]=='\\')) var= var (1, N(var));
    int i, arity= as_int (l2e(t[2]));
    tree default_option= l2e(t[3]);
    tree f1 (FUNC), f2 (MACRO), f3 (APPLY);
    f3 << var*"*";
    for (i=1; i<=arity; i++) {
      f1 << as_string (i);
      if (i > 1) {
        f2 << as_string (i);
        f3 << tree (ARG, as_string (i));
      }
      else
        f3 << default_option;
    }
    f1 << l2e (t[4]);
    f2 << f3;
    return concat (tree (ASSIGN, var*"*", f1), tree (ASSIGN, var, f2));
  }

  if (is_tuple (t, "\\newtheorem", 2) || is_tuple (t, "\\newdef", 2) ||
      is_tuple (t, "\\newtheorem*", 2)) {
    string var= l2e(t[1])->label;
    string val= l2e(t[2])->label;
    return compound ("new-theorem", var, val);
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
  if (is_tuple (t, "\\newenvironment**", 5)) {
    string var= l2e(t[1])->label;
    int i, arity= as_int (l2e(t[2])->label);
    tree default_option= l2e(t[3]);
    tree e1 (ENV), e2 (MACRO), e3 (APPLY);
    e3 << var*"*";
    for (i=1; i<=arity; i++) {
      e1 << as_string (i);
      if (i > 1) {
        e2 << as_string (i);
        e3 << tree (ARG, as_string (i));
      }
      else
        e3 << default_option;
    }
    e1 << l2e (t[4]);
    e1 << l2e (t[5]);
    e3 << tree (ARG, "body");
    e2 << "body" << e3;
    return concat (tree (ASSIGN, var*"*", e1), tree (ASSIGN, var, e2));
  }

  if (is_tuple (t, "\\Roman", 1)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    return tree (NUMBER, tree(APPLY, u->label*"-nr"), "Roman");
  }

   if (is_tuple (t, "\\roman", 1)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    return tree (NUMBER, tree(APPLY, u->label*"-nr"), "roman");
  }
  
  if (is_tuple (t, "\\Alph", 1)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    return tree (NUMBER, tree(APPLY, u->label*"-nr"), "Alpha");
  }

  if (is_tuple (t, "\\alph", 1)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    return tree (NUMBER, tree(APPLY, u->label*"-nr"), "alpha");
  }
  
  if (is_tuple (t, "\\fnsymbol", 1)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    return tree (NUMBER, tree(APPLY, u->label*"-nr"), "fnsymbol");
  }
  
  if (is_tuple (t, "\\arabic", 1)) {
    tree u= l2e (t[1]);
    if (is_compound (u)) return "";
    return tree (NUMBER, tree(APPLY, u->label*"-nr"), "arabic");
  }
  
  if (is_tuple (t, "\\equal", 2))
    return tree (EQUAL, l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\ifthenelse", 3))
    return tree (IF, l2e (t[1]), l2e (t[2]), l2e (t[3]));

  if (textm_appendices && is_tuple (t, "\\chapter", 1))
    return tree (APPLY, "appendix", l2e (t[1]));

  if (is_tuple (t, "\\^", 1)) return latex_accent (t[1], "^");
  if (is_tuple (t, "\\~", 1)) return latex_accent (t[1], "~");
  if (is_tuple (t, "\\`", 1)) return latex_accent (t[1], "<grave>");
  if (is_tuple (t, "\\'", 1)) return latex_accent (t[1], "<acute>");
  if (is_tuple (t, "\\\"", 1)) return latex_accent (t[1], "<ddot>"); // diaeresis
  if (is_tuple (t, "\\.", 1)) return latex_accent (t[1], "<dot>");
  if (is_tuple (t, "\\u", 1)) return latex_accent (t[1], "<breve>");
  if (is_tuple (t, "\\v", 1)) return latex_accent (t[1], "<check>"); // caron
  if (is_tuple (t, "\\=", 1)) return latex_accent (t[1], "<bar>");   // macron


  if (is_tuple (t, "\\textrm", 1)) return m2e (t, FONT_FAMILY, "rm");
  if (is_tuple (t, "\\texttt", 1)) return m2e (t, FONT_FAMILY, "tt");
  if (is_tuple (t, "\\textsf", 1)) return m2e (t, FONT_FAMILY, "ss");
  if (is_tuple (t, "\\textmd", 1)) return m2e (t, FONT_SERIES, "medium");
  if (is_tuple (t, "\\textbf", 1)) return m2e (t, FONT_SERIES, "bold");
  if (is_tuple (t, "\\textup", 1)) return m2e (t, FONT_SHAPE, "right");
  if (is_tuple (t, "\\textit", 1)) return m2e (t, FONT_SHAPE, "italic");
  if (is_tuple (t, "\\textsl", 1)) return m2e (t, FONT_SHAPE, "slanted");
  if (is_tuple (t, "\\textsc", 1)) return m2e (t, FONT_SHAPE, "small-caps");
  if (is_tuple (t, "\\tmrsub", 1)) return tree (RSUB, l2e (t[1]));
  if (is_tuple (t, "\\tmrsup", 1)) return tree (RSUP, l2e (t[1]));
  if (is_tuple (t, "\\textsubscript", 1)) return tree (RSUB, l2e (t[1]));
  if (is_tuple (t, "\\textsuperscript", 1)) return tree (RSUP, l2e (t[1]));
  if (is_tuple (t, "\\lowercase", 1) || is_tuple (t, "\\MakeLowercase", 1))
    return tree (CHANGE_CASE, l2e (t[1]), "locase");
  if (is_tuple (t, "\\uppercase", 1) || is_tuple (t, "\\MakeUppercase", 1))
    return tree (CHANGE_CASE, l2e (t[1]), "UPCASE");
  if (is_tuple (t, "\\tmtextrm", 1)) return m2e (t, FONT_FAMILY, "rm");
  if (is_tuple (t, "\\tmtexttt", 1)) return m2e (t, FONT_FAMILY, "tt");
  if (is_tuple (t, "\\tmtextsf", 1)) return m2e (t, FONT_FAMILY, "ss");
  if (is_tuple (t, "\\tmtextmd", 1)) return m2e (t, FONT_SERIES, "medium");
  if (is_tuple (t, "\\tmtextbf", 1)) return m2e (t, FONT_SERIES, "bold");
  if (is_tuple (t, "\\tmtextup", 1)) return m2e (t, FONT_SHAPE, "right");
  if (is_tuple (t, "\\tmtextit", 1)) return m2e (t, FONT_SHAPE, "italic");
  if (is_tuple (t, "\\tmtextsl", 1)) return m2e (t, FONT_SHAPE, "slanted");
  if (is_tuple (t, "\\tmtextsc", 1)) return m2e (t, FONT_SHAPE, "small-caps");
  if (is_tuple (t, "\\emph", 1))   return compound ("em", l2e (t[1]));
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
  if (is_tuple (t, "\\mathcal", 1)) return m2e (t, MATH_FONT, "cal");
  if (is_tuple (t, "\\mathfrak", 1)) return m2e (t, MATH_FONT, "Euler");
  if (is_tuple (t, "\\mathbb", 1)) return m2e (t, MATH_FONT, "Bbb");
  if (is_tuple (t, "\\bm", 1)) return m2e (t, MATH_FONT_SERIES, "bold");
  if (is_tuple (t, "\\mathbbm", 1)) return m2e (t, MATH_FONT, "Bbb*");
  if (is_tuple (t, "\\mathbbmss", 1)) return m2e (t, MATH_FONT, "Bbb**");
  if (is_tuple (t, "\\mathds", 1)) return m2e (t, MATH_FONT, "Bbb****");
  if (is_tuple (t, "\\mathscr", 1)) return m2e (t, MATH_FONT, "cal*");
  if (is_tuple (t, "\\EuScript", 1)) return m2e (t, MATH_FONT, "cal**");

  if (is_tuple (t, "\\COMMENT", 1) || is_tuple (t, "\\Comment", 1) ||
      is_tuple (t, "\\tcp*", 1) || is_tuple (t, "\\tcp", 1) ||
      is_tuple (t, "\\tcc*", 1) || is_tuple (t, "\\tcc", 1))
    return tree (APPLY, "algo-comment", l2e (t[1]));
  if (is_tuple (t, "\\tcp**", 2) || is_tuple (t, "\\tcp*", 2))
    return tree (APPLY, "algo-comment", l2e (t[2]));
  if (is_tuple (t, "\\UNTIL", 1) || is_tuple (t, "\\Until", 1))
    return tree (END,   "algo-repeat", l2e (t[1]));
  if (is_tuple (t, "\\If", 2) || is_tuple (t, "\\lIf", 2) ||
      is_tuple (t, "\\uIf", 2))
    return tree (APPLY, "algo-if", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\If*", 3) || is_tuple (t, "\\lIf*", 3) ||
      is_tuple (t, "\\uIf*", 3))
    return tree (APPLY, "algo-if", l2e (t[1]),
        concat (tree (APPLY, "algo-comment", l2e (t[2])), "\n", l2e (t[3])));
  if (is_tuple (t, "\\Else", 1) || is_tuple (t, "\\lElse", 1) ||
      is_tuple (t, "\\uElse", 1))
    return tree (APPLY, "algo-else", l2e (t[1]));
  if (is_tuple (t, "\\Else*", 2) || is_tuple (t, "\\lElse*", 2) ||
      is_tuple (t, "\\uElse*", 2))
    return tree (APPLY, "algo-else",
        concat (tree (APPLY, "algo-comment", l2e (t[1])), "\n", l2e (t[2])));
  if (is_tuple (t, "\\eIf", 3))
    return tree (APPLY, "algo-if-else-if", l2e (t[1]), l2e (t[2]), l2e (t[3]));
  if (is_tuple (t, "\\ElseIf", 2) || is_tuple (t, "\\lElseIf", 2) ||
      is_tuple (t, "\\uElseIf", 2))
    return tree (APPLY, "algo-else-if", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\ElseIf*", 3) || is_tuple (t, "\\lElseIf*", 3) ||
      is_tuple (t, "\\uElseIf*", 3))
    return tree (APPLY, "algo-else-if", concat (l2e (t[2]), " ",
          tree (APPLY, "algo-comment", l2e (t[1]))), l2e (t[3]));
  if (is_tuple (t, "\\ELSIF", 1) || is_tuple (t, "\\ElsIf", 1))
    return tree (APPLY, "algo-else-if", l2e (t[1]));
  if (is_tuple (t, "\\ELSIF*", 2))
    return tree (APPLY, "algo-else-if",
        concat (l2e (t[2]), " ", tree (APPLY, "algo-comment", l2e (t[1]))));
  if (is_tuple (t, "\\FOR", 1) || is_tuple (t, "\\For", 1)) {
    return tree (BEGIN, "algo-for", l2e (t[1]));
  }
  if (is_tuple (t, "\\FOR*", 2))
    return tree (BEGIN, "algo-for",
        concat (l2e (t[2]), " ", tree (APPLY, "algo-comment", l2e (t[1]))));
  if (is_tuple (t, "\\For", 2) || is_tuple (t, "\\lFor", 2)) {
    return tree (APPLY, "algo-for", l2e (t[1]), l2e (t[2]));
  }
  if (is_tuple (t, "\\For*", 3) || is_tuple (t, "\\lFor*", 3)) {
    return tree (APPLY, "algo-for", l2e (t[2]),
        concat (tree (APPLY, "algo-comment", l2e (t[1])), " ", l2e (t[2])));
  }
  if (is_tuple (t, "\\ForEach", 2) || is_tuple (t, "\\lForEach", 2))
    return tree (APPLY, "algo-for-each", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\ForEach*", 3) || is_tuple (t, "\\lForEach*", 3))
    return tree (APPLY, "algo-for-each", concat (l2e (t[2]), " ",
          tree (APPLY, "algo-comment", l2e (t[1]))), l2e (t[3]));
  if (is_tuple (t, "\\ForAll", 2) || is_tuple (t, "\\lForAll", 2))
    return tree (APPLY, "algo-for-all", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\ForAll*", 3) || is_tuple (t, "\\lForAll*", 3))
    return tree (APPLY, "algo-for-all", concat (l2e (t[2]), " ",
          tree (APPLY, "algo-comment", l2e (t[1]))), l2e (t[3]));
  // Since \FOR and \FORALL are together closed by a \ENDFOR macro, there is no
  // easy way to match here the begin/end environment. So we hack the arity.
  if (is_tuple (t, "\\FORALL", 1) || is_tuple (t, "\\ForAll", 1))
    return tree (BEGIN, "algo-for", "*", l2e (t[1]));
  if (is_tuple (t, "\\FORALL*", 2))
    return tree (BEGIN, "algo-for", "", "",
        concat (l2e (t[2]), " ", tree (APPLY, "algo-comment", l2e (t[1]))));
  if (is_tuple (t, "\\IF", 1) || is_tuple (t, "\\If", 1))
    return tree (BEGIN, "algo-if-else-if", l2e (t[1]));
  if (is_tuple (t, "\\IF*", 2))
    return tree (BEGIN, "algo-if-else-if",
        concat (l2e (t[2]), " ", tree (APPLY, "algo-comment", l2e (t[1]))));
  if (is_tuple (t, "\\KwData", 1))
    return tree (APPLY, "algo-data", l2e (t[1]));
  if (is_tuple (t, "\\KwResult", 1))
    return tree (APPLY, "algo-result", l2e (t[1]));
  if (is_tuple (t, "\\WHILE", 1) || is_tuple (t, "\\While", 1))
    return tree (BEGIN, "algo-while", l2e (t[1]));
  if (is_tuple (t, "\\WHILE*", 2))
    return tree (BEGIN, "algo-while",
        concat (l2e (t[2]), " ", tree (APPLY, "algo-comment", l2e (t[1]))));
  if (is_tuple (t, "\\While", 2))
    return tree (APPLY, "algo-while", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\While*", 3))
    return tree (APPLY, "algo-while", l2e (t[2]),
        concat (tree (APPLY, "algo-comment", l2e (t[1])), "\n", l2e (t[2])));
  if (is_tuple (t, "\\Begin", 1))
    return tree (APPLY, "algo-begin", l2e (t[1]));
  if (is_tuple (t, "\\Begin*", 2))
    return tree (APPLY, "algo-begin",
        concat (tree (APPLY, "algo-comment", l2e (t[1])), "\n", l2e (t[2])));
  if (is_tuple (t, "\\BODY*", 1))
    return concat (tree (BEGIN, "algo-body"),
        tree (APPLY, "algo-comment", l2e (t[1])), "\n");
  if (is_tuple (t, "\\Call", 2))
    return tree (BEGIN, "algo-call", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\ELSE*", 1))
    return tree (APPLY, "algo-else", tree (APPLY, "algo-comment", l2e (t[1])));
  if (is_tuple (t, "\\Function", 2))
    return tree (BEGIN, "algo-function", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\LOOP*", 1))
    return concat (tree (BEGIN, "algo-loop"),
        tree (APPLY, "algo-comment", l2e (t[1])), "\n");
  if (is_tuple (t, "\\KwIn", 1))
    return tree (APPLY, "algo-inputs", l2e (t[1])); 
  if (is_tuple (t, "\\INPUTS*", 1))
    return concat (tree (BEGIN, "algo-inputs"),
        tree (APPLY, "algo-comment", l2e (t[1])), "\n");
  if (is_tuple (t, "\\KwOut", 1))
    return tree (APPLY, "algo-outputs", l2e (t[1]));
  if (is_tuple (t, "\\OUTPUTS*", 1))
    return concat (tree (BEGIN, "algo-outputs"),
        tree (APPLY, "algo-comment", l2e (t[1])), "\n");
  if (is_tuple (t, "\\Procedure", 2))
    return tree (BEGIN, "algo-procedure", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\REPEAT*", 1))
    return concat (tree (BEGIN, "algo-repeat"),
        tree (APPLY, "algo-comment", l2e (t[1])), "\n");
  if (is_tuple (t, "\\Return", 1) || is_tuple (t, "\\KwRet", 1))
    return tree (APPLY, "algo-return", l2e (t[1]));
  if (is_tuple (t, "\\SetKw", 2))
      return tree (APPLY, "algo-new-keyword", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\SetKwData", 2))
      return tree (APPLY, "algo-new-data", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\SetKwInput", 2))
      return tree (APPLY, "algo-new-input", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\SetKwInOut", 2))
      return tree (APPLY, "algo-new-in-out", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\SetKwFunction", 2))
      return tree (APPLY, "algo-new-function", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\nllabel", 1))
    return tree (APPLY, "algo-label", t2e (t[1]));
  if (is_tuple (t, "\\lnl", 1))
    return tree (APPLY, "algo-number-label", t2e (t[1]));

  if (is_tuple (t, "\\mod", 1)) return tree (APPLY, "modulo", l2e (t[1]));
  if (is_tuple (t, "\\prime", 1)) return tree (RPRIME, string_arg (t[1]));
  if (is_tuple (t, "\\frac", 2)) return tree (FRAC, l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\choose", 2))
    return compound ("choose", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\atop", 2))
    return compound ("atop", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\sqrt", 1))  return tree (SQRT, l2e (t[1]));
  if (is_tuple (t, "\\sqrt*", 2)) return tree (SQRT, l2e (t[2]), l2e (t[1]));
  if (is_tuple (t, "\\<sub>", 1)) return tree (RSUB, l2e (t[1]));
  if (is_tuple (t, "\\not", 1)) return tree (NEG, l2e (t[1]));
  if (is_tuple (t, "\\bar", 1)) return tree (WIDE, l2e (t[1]), "<bar>");
  if (is_tuple (t, "\\overline", 1))
    return tree (WIDE, l2e (t[1]), "<bar>");
  if (is_tuple (t, "\\underline", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<bar>");
  if (is_tuple (t, "\\hat", 1)) return tree (WIDE, l2e (t[1]), "^");
  if (is_tuple (t, "\\tilde", 1)) return tree (WIDE, l2e (t[1]), "~");
  if (is_tuple (t, "\\widehat", 1)) return tree (WIDE, l2e (t[1]), "^");
  if (is_tuple (t, "\\widetilde", 1)) return tree (WIDE, l2e (t[1]), "~");
  if (is_tuple (t, "\\dot", 1)) return tree (WIDE, l2e (t[1]), "<dot>");
  if (is_tuple (t, "\\ddot", 1)) return tree (WIDE, l2e (t[1]), "<ddot>");
  if (is_tuple (t, "\\dddot", 1)) return tree (WIDE, l2e (t[1]), "<dddot>");
  if (is_tuple (t, "\\ddddot", 1)) return tree (WIDE, l2e (t[1]), "<ddddot>");
  if (is_tuple (t, "\\check", 1)) return tree (WIDE, l2e (t[1]), "<check>");
  if (is_tuple (t, "\\grave", 1)) return tree (WIDE, l2e (t[1]), "<grave>");
  if (is_tuple (t, "\\acute", 1)) return tree (WIDE, l2e (t[1]), "<acute>");
  if (is_tuple (t, "\\vec", 1)) return tree (WIDE, l2e (t[1]), "<vect>");
  if (is_tuple (t, "\\breve", 1)) return tree (WIDE, l2e (t[1]), "<breve>");
  if (is_tuple (t, "\\abovering", 1) || is_tuple (t, "\\mathring", 1))
    return tree (WIDE, l2e (t[1]), "<abovering>");
  if (is_tuple (t, "\\hspace", 1) || is_tuple (t, "\\hspace*", 1)) {
    if (is_tuple (t[1], "\\tex-len", 3))
          return tree (SPACE, l2e (t[1]));
    else {
      tree r= t2e (t[1]);
      if (is_var_compound (r, "fill", 0)) return tree (HTAB, "1fn");
      return tree (SPACE, r);
    }
  }
  if (is_tuple (t, "\\\\*", 1))
    return concat(tree (FORMAT, "next line"), tree (VAR_VSPACE, t2e (t[1])));
  if (is_tuple (t, "\\vspace", 1) || is_tuple (t, "\\vspace*", 1)) {
    if (is_tuple (t[1], "\\tex-len", 3))
      return tree (VSPACE, l2e (t[1]));
    return tree (VSPACE, t2e (t[1]));
  }
  if (is_tuple (t, "\\label", 1)) return tree (LABEL, t2e (t[1]));
  if (is_tuple (t, "\\ref", 1)) return tree (REFERENCE, t2e (t[1]));
  if (is_tuple (t, "\\newcounter", 1))
    return compound ("new-counter", v2e (t[1]));
  if (is_tuple (t, "\\value", 1))
    return compound ("value-counter", v2e (t[1]));
  if (is_tuple (t, "\\stepcounter", 1))
    return compound ("inc-counter", v2e (t[1]));
  if (is_tuple (t, "\\refstepcounter", 1))
    return compound ("next-counter", v2e (t[1]));
  if (is_tuple (t, "\\setcounter", 2)) {
    if (v2e (t[2]) == "0")
      return compound ("reset-counter", v2e (t[1]));
    return tree (ASSIGN, v2e (t[1]) * "-nr", v2e (t[2]));
  }
  if (is_tuple (t, "\\addtocounter", 2)) {
    if (v2e (t[2]) == "1")
      return compound ("inc-counter", v2e (t[1]));
    return tree (ASSIGN, v2e (t[1]) * "-nr",
        tree (PLUS, v2e (t[1]) * "-nr", v2e (t[2])));
  }
  if (is_tuple (t, "\\setlength", 2)) {
    if (!textm_class_flag) return "";
    else {
      tree len= l2e (t[1]);
      tree val= l2e (t[2]);
      return tree (ASSIGN, len, tree (MACRO, val));
    }
  }

  if (is_tuple (t, "\\@startsection")) {
    tree name, indent, spa, spb, style, r, indentafter;
    name = l2e(t[1]);
    indent = l2e(t[3]);
    spb = l2e(t[4]);
    spa = l2e(t[5]);
    style = concat();
    r = concat();
    bool inserted = false, center = false;

    for (int i = 0 ; i < N(t[6]) ; i++){
      if (is_tuple(t[6][i], "\\centering", 0)) center = true;
      else if (is_tuple(t[6][i], "\\normalfont", 0)) ;
      else style << t[6][i];
    }
    style = l2e(style);

    if (is_negative_length(spb)) indentafter = tree();
    else indentafter = tree(HSPACE, "par-first");
    spb = compound("vspace*", abs_length(spb));

    if (is_negative_length(spa))
      spa = tree(HSPACE, abs_length(spa));
    else
      spa = tree(VSPACE, spa);

    for (int i = 0 ; i < N(style) ; i++) {
      if (is_func(style[i], RESET) && !inserted) {
        if (center)
          r << spb << compound("center", tree(ARG, "name")) 
            << spa << indentafter;
        else
          r << spb << tree(HSPACE, indent) << tree(ARG, "name") 
            << spa << indentafter;
        inserted = true;
      }
      else
        r << style[i];
    }
    if (is_func(spa, VSPACE)) r = compound("sectional-normal", r);
    return tree (ASSIGN, concat(name->label, "-title"),
        tree (MACRO, "name", r));
  }
  if (is_tuple (t, "\\@setfontsize", 3)) {
    tree fontsize = l2e(t[2]);
    tree baselineskip = l2e(t[3]);
    return tree(WITH, "font-size-base", fontsize, "par-sep", 
      (tree(MINUS, concat(baselineskip, "pt"), concat(fontsize, "pt"))));
  }
  if (is_tuple (t, "\\addtolength")) return "";
  if (is_tuple (t, "\\enlargethispage")) return "";
  if (is_tuple (t, "\\mathop", 1)) return l2e (t[1]);
  if (is_tuple (t, "\\mathrel", 1)) return l2e (t[1]);
  if (is_tuple (t, "\\overbrace", 1))
    return tree (WIDE, l2e (t[1]), "<wide-overbrace>");
  if (is_tuple (t, "\\underbrace", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<wide-underbrace>");

  if (is_tuple (t, "\\text", 1) || is_tuple (t, "\\textnormal", 1) ||
      is_tuple (t, "\\mbox", 1) || is_tuple (t, "\\hbox", 1))
    return var_m2e (t, MODE, "text");
  if (is_tuple (t, "\\mathchoice", 4))
    return compound ("math-choice", 
        l2e (t[1]), l2e (t[2]), l2e (t[3]), l2e (t[4]));
  if (is_tuple (t, "\\ensuremath", 1))
    return var_m2e (t, MODE, "math");
  if (is_tuple (t, "\\Mvariable", 1))
    return compound ("Mvariable", var_m2e (t, MODE, "text"));
  if (is_tuple (t, "\\Mfunction", 1))
    return compound ("Mfunction", var_m2e (t, MODE, "text"));
  if (is_tuple (t, "\\Muserfunction", 1))
    return compound ("Muserfunction", var_m2e (t, MODE, "text"));

  if (is_tuple (t, "\\<sup>", 1)) {
    if (is_tuple (t[1], "\\prime", 0))
      return tree (RPRIME, "'");
    else return tree (RSUP, l2e (t[1]));
  }
  if (is_tuple (t, "\\stackrel", 2))
    return tree (ABOVE, l2e (t[2]), l2e (t[1]));
  if (is_tuple (t, "\\overset", 2))
    return tree (ABOVE, l2e (t[2]), l2e (t[1]));
  if (is_tuple (t, "\\underset", 2))
    return tree (BELOW, l2e (t[2]), l2e (t[1]));
  if (is_tuple (t, "\\parbox", 2))
    return compound ("mini-paragraph", v2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\parbox*", 3))
    return compound ("mini-paragraph", v2e (t[2]), l2e (t[3]));

  int dtype= 0;
  if (is_large_delimiter (t, dtype)) {
    string s= t[1]->label;
    if ((N(s)>1) && (s[0]=='\\')) s=s(1,N(s));
    if (s == "vert" || s == "arrowvert") s= "|";
    if (s == "Vert" || s == "Arrowvert") s= "||";
    if (s == "lbrace") s= "{";
    if (s == "rbrace") s= "}";
    if (dtype == -1) return tree (LEFT, s);
    else if (dtype == 1) return tree (RIGHT, s);
    else return tree (MID, s);
  }
  if (is_tuple (t, "\\cite", 1) || is_tuple (t, "\\nocite", 1)) {
    string cite_type= t[0]->label (1, N(t[0]->label));
    string s= v2e (t[1]);
    return latex_cite_to_tree (cite_type, s);
  }
  if (is_tuple (t, "\\cite*", 2)) {
    tree   ot= l2e (t[1]);
    string s = v2e (t[2]);
    tree   ct= latex_cite_to_tree ("cite", s);
    if (N(ct) == 2) return compound ("cite-detail", ct[1], ot);
    return tree (CONCAT, ct, " (", ot, ")");
  }
  if (is_tuple (t, "\\citedetail", 2))
    return compound ("cite-detail", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\citet", 1) || is_tuple (t, "\\citep", 1) ||
      is_tuple (t, "\\citet*", 1) || is_tuple (t, "\\citep*", 1) ||
      is_tuple (t, "\\citealt", 1) || is_tuple (t, "\\citealp", 1) ||
      is_tuple (t, "\\citealt*", 1) || is_tuple (t, "\\citealp*", 1))
    {
      textm_natbib= true;
      string star= "";
      string cite_type= t[0]->label (1, N(t[0]->label));
      if (ends (cite_type, "*")) {
	star= "*"; cite_type= cite_type (0, N (cite_type) - 1); }
      if (cite_type == "citet") cite_type= "cite-textual" * star;
      if (cite_type == "citep") cite_type= "cite-parenthesized" * star;
      if (cite_type == "citealt") cite_type= "cite-raw" * star;
      if (cite_type == "citealp") cite_type= "cite-raw" * star;
      string s= v2e (t[1]);
      return latex_cite_to_tree (cite_type, s);
    }
  if (is_tuple (t, "\\citetext", 1))
    return compound ("render-cite", l2e (t[1]));
  if (is_tuple (t, "\\onlinecite", 1))
    return compound ("cite-arg", l2e (t[1]));
  if (is_tuple (t, "\\citeauthor", 1)) {
    textm_natbib= true; return compound ("cite-author-link", t2e (t[1])); }
  if (is_tuple (t, "\\citeauthor*", 1)) {
    textm_natbib= true; return compound ("cite-author*-link", t2e (t[1])); }
  if (is_tuple (t, "\\citeyear", 1)) {
    textm_natbib= true; return compound ("cite-year-link", t2e (t[1])); }
  if (is_tuple (t, "\\bibitem", 1))
    return compound ("bibitem", v2e (t[1]));
  if (is_tuple (t, "\\bibitem*", 2))
    return compound ("bibitem-with-key", v2e (t[1]), v2e (t[2]));
  if (is_tuple (t, "\\index", 1)) {
    string s= v2e (t[1]);
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
  if (is_tuple (t, "\\includegraphics", 1) ||
      is_tuple (t, "\\includegraphics*", 1)) {
    tree name= v2e (t[1]);
    if (name == "") return "";
    else {
      tree g (IMAGE, 7);
      g[0]= name;
      return g;
    }
  }
  if (is_tuple (t, "\\includegraphics*", 2) ||
      is_tuple (t, "\\includegraphics**", 2)) {
    tree name= v2e (t[2]);
    tree data= v2e (t[1]);
    if (data == "" || name == "") return "";
    else {
      tree g (IMAGE, 7);
      g[0]= name;
      tree width = latex_eps_get (data, "width");
      tree height= latex_eps_get (data, "height");
      g[1]= width;
      g[2]= height;
      return g;
    }
  }
  if (is_tuple (t, "\\epsfig", 1)) {
    tree data  = v2e (t[1]);
    tree name  = latex_eps_get (data, "file");
    tree width = latex_eps_get (data, "width");
    tree height= latex_eps_get (data, "height");
    if (name == "") return "";
    else {
      tree g (IMAGE, 5);
      g[0]= name;
      g[1]= width;
      g[2]= height;
      return g;

    }
  }
  if (is_tuple (t, "\\fbox", 1)) return compound ("frame", l2e (t[1]));
  if (is_tuple (t, "\\framebox", 1)) return compound ("frame", l2e (t[1]));
  if (is_tuple (t, "\\centerline", 1)) return compound ("center", l2e (t[1]));
  if (is_tuple (t, "\\noalign", 1))
    return ""; // FIXME: for larger space in maple matrices
  if (is_tuple (t, "\\etalchar", 1)) return t2e (t[1]);
  if (is_tuple (t, "\\natexlab", 1)) return t2e (t[1]);
  if (is_tuple (t, "\\penalty", 1)) return "";
  if (is_tuple (t, "\\url", 1))
    return tree (APPLY, "href", t2e (t[1]));
  if (is_tuple (t, "\\href", 2))
    return tree (APPLY, "hlink", l2e (t[2]), t2e (t[1]));

  if (is_tuple (t, "\\xminus", 1))
    return tree (LONG_ARROW, "<rubber-minus>", l2e (t[1]));
  if (is_tuple (t, "\\xleftarrow", 1))
    return tree (LONG_ARROW, "<rubber-leftarrow>", l2e (t[1]));
  if (is_tuple (t, "\\xrightarrow", 1))
    return tree (LONG_ARROW, "<rubber-rightarrow>", l2e (t[1]));
  if (is_tuple (t, "\\xleftrightarrow", 1))
    return tree (LONG_ARROW, "<rubber-leftrightarrow>", l2e (t[1]));
  if (is_tuple (t, "\\xmapsto", 1))
    return tree (LONG_ARROW, "<rubber-mapsto>", l2e (t[1]));
  if (is_tuple (t, "\\xmapsfrom", 1))
    return tree (LONG_ARROW, "<rubber-mapsfrom>", l2e (t[1]));
  if (is_tuple (t, "\\xequal", 1))
    return tree (LONG_ARROW, "<rubber-equal>", l2e (t[1]));
  if (is_tuple (t, "\\xLeftarrow", 1))
    return tree (LONG_ARROW, "<rubber-Leftarrow>", l2e (t[1]));
  if (is_tuple (t, "\\xRightarrow", 1))
    return tree (LONG_ARROW, "<rubber-Rightarrow>", l2e (t[1]));
  if (is_tuple (t, "\\xLeftrightarrow", 1))
    return tree (LONG_ARROW, "<rubber-Leftrightarrow>", l2e (t[1]));
  if (is_tuple (t, "\\xminus*", 2))
    return tree (LONG_ARROW, "<rubber-minus>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xleftarrow*", 2))
    return tree (LONG_ARROW, "<rubber-leftarrow>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xrightarrow*", 2))
    return tree (LONG_ARROW, "<rubber-rightarrow>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xleftrightarrow*", 2))
    return tree (LONG_ARROW, "<rubber-leftrightarrow>",
                 l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xmapsto*", 2))
    return tree (LONG_ARROW, "<rubber-mapsto>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xmapsfrom*", 2))
    return tree (LONG_ARROW, "<rubber-mapsfrom>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xequal*", 2))
    return tree (LONG_ARROW, "<rubber-equal>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xLeftarrow*", 2))
    return tree (LONG_ARROW, "<rubber-Leftarrow>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xRightarrow*", 2))
    return tree (LONG_ARROW, "<rubber-Rightarrow>", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\xLeftrightarrow*", 2))
    return tree (LONG_ARROW, "<rubber-Leftrightarrow>",
                 l2e (t[1]), l2e (t[2]));

  // Start TeXmacs specific markup
  if (is_tuple (t, "\\tmmathbf", 1))
    return tree (CONCAT,
		 tree (SET, MATH_FONT_SERIES, "bold"),
		 l2e (t[1]),
		 tree (RESET, MATH_FONT_SERIES));
  if (is_tuple (t, "\\tmop", 1)) return t2e (t[1]);
  if (is_tuple (t, "\\tmstrong", 1)) return tree (APPLY, "strong", l2e (t[1]));
  if (is_tuple (t, "\\tmem", 1)) return tree (APPLY, "em", l2e (t[1]));
  if (is_tuple (t, "\\tmtt", 1)) return tree (APPLY, "tt", l2e (t[1]));
  if (is_tuple (t, "\\tmdate", 1)) return tree (APPLY, "date", l2e (t[1]));
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
  if (is_func (t, APPLY, 1) && is_atomic (t[0]))
    return tree (APPLY, latex_symbol_to_tree (t[0]->label));
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
static void
parse_pmatrix (tree& r, tree t, int& i, string lb, string rb, string fm) {
  tree tformat (TFORMAT);
  if (N(t[i]) > 1) tformat= parse_matrix_params (t[i][1]);
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
    else if (is_func (v, BEGIN) && (v[0] == "array" || v[0] == "tabular")) {
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
    else if (v == tree (END, "cases")) break;
    else if (v == tree (END, "stack")) break;
    else if (v == tree (END, "matrix")) break;
    else if (v == tree (END, "pmatrix")) break;
    else if (v == tree (END, "bmatrix")) break;
    else if (v == tree (END, "vmatrix")) break;
    else if (v == tree (END, "smallmatrix")) break;
    else if (v == tree (APPLY, "hline")) {
      int    row  = N(V)+ (N(L)==0? 0: 1);
      string row_s= row==0? as_string (row+1): as_string (row);
      string vbor = row==0? copy (CELL_TBORDER): copy (CELL_BBORDER);
      string how  = "1ln";
      tformat << tree (CWITH, row_s, row_s, "1", "-1", vbor, how);
      while (i+1<N(t) && t[i+1] == " ") i++;
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
  tformat << M;
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
	else if (u[i][0] == "tabular")
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
  if (latex_type ("\\begin-" * s) == "list") return true;
  if (latex_type ("\\begin-" * s) == "environment") return true;
  if (latex_type ("\\begin-" * s) == "math-environment") return true;
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

      if (is_func (v, BEGIN) && v[0] == "tmindent") {
	r << tree (BEGIN, "indent");
	continue;
      }

      if (is_func (v, END) && v[0] == "tmindent") {
	r << tree (END, "indent");
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

      if (is_func (v, BEGIN) && ((v[0] == "multicols"))) {
	r << tree (SET, "par-columns", v[1]);
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "multicols")) {
	r << tree (RESET, "par-columns");
	continue;
      }

      if (is_func (v, BEGIN) && ((v[0] == "figure") || (v[0] == "figure*"))) {
	r << tree (NEW_LINE) << tree (BEGIN, "bigfigure");
	continue;
      }

      if (is_func (v, END, 1) && (v[0] == "figure")) {
	r << tree (END, "bigfigure") << tree (NEW_LINE);
	continue;
      }

      if (is_func (v, BEGIN) && ((v[0] == "table") || (v[0] == "table*"))) {
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
	if (((i+1) == N(t)) || (t[i+1] != tree (FORMAT, "new line")))
	  insert_return (r);
	spc_flag = true;
	item_flag= false;
	continue;
      }
      
      // Needed for matching beginning/ending for unknown or user envs.
      // It will be restored in finalize_misc.
      if (is_func (v, BEGIN)) {
	string var= as_string (v[0]);
  if (var[N(var)-1] == '*') var= var(0,N(var)-1);
  tree w (BEGIN, var);
  for (int j=1; j<N(v); j++) w << v[j];
	r << w;
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
	(is_var_compound (u[0], "part", 1) ||
	 is_var_compound (u[0], "part*", 1) ||
	 is_var_compound (u[0], "chapter", 1) ||
	 is_var_compound (u[0], "chapter*", 1) ||
	 is_var_compound (u[0], "section", 1) ||
	 is_var_compound (u[0], "section*", 1) ||
	 is_var_compound (u[0], "subsection", 1) ||
	 is_var_compound (u[0], "subsection*", 1) ||
	 is_var_compound (u[0], "subsubsection", 1) ||
	 is_var_compound (u[0], "subsubsection*", 1)))
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
  r << arg;
  return r;
}

tree
complete_algorithm_args (tree t, bool &in) {
  if (is_atomic (t)) return t;
  tree r = tree (L(t));
  for (int i=0; i<N(t); i++) {
    if (textm_algorithm_begin_algo (t[i])) in = true;
    else if (in && textm_algorithm_end_algo (t[i])) {
      in = false;
      r << t[i];
      continue;
    }

    if (in && textm_algorithm_need_arg (t[i])) {
      r << textm_algorithm_parse_arg (t, i) << "\n";
    }
    else if (in && textm_algorithm_space_after (t[i]))
      r << t[i] << " ";
    else if (in && textm_algorithm_break_after (t[i]))
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
finalize_document (tree t) {
  if (is_atomic (t)) t= tree (CONCAT, t);
  t= finalize_algorithms (t); 
  t= finalize_returns  (t); 
  t= finalize_pmatrix  (t); 
  t= finalize_layout   (t); 
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
  return finalize_sections (r);
}

bool
is_preamble_command (tree t, tree& doc, string& style) {
  (void) doc;
  if (is_func (t, APPLY, 2)) {
    if (t[0] == "usepackage") return true;
    if ((t[0] == "documentstyle") ||
	(t[0] == "documentclass")) {
      style= string_arg (t[1]);
      return true;
    }
  }
  if (is_func (t, APPLY, 3)) {
    if (t[0] == "usepackage*") return true;
    if ((t[0] == "documentstyle*") ||
	(t[0] == "documentclass*")) {
      style= string_arg (t[2]);
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
  return starts (s, "http://") || starts (s, "ftp://");
}

tree
finalize_misc (tree t) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "verbatim", 1) &&
           is_atomic (t[0]) && is_hyper_link (t[0]->label)) {
    return compound ("href", finalize_misc (t[0]));
  }
  else if (is_func (t, WITH, 3) && t[0] == FONT_FAMILY && t[1] == "tt" &&
           is_atomic (t[2]) && is_hyper_link (t[2]->label)) {
    return compound ("href", finalize_misc (t[2]));
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
  else {
    int i, n= N(t);
    tree r (t, n);
    string l= as_string (L(t));
    // Restore name of users envs, munged in finalize_layout.
    if (latex_type ("\\begin-"*l) == "user" && latex_arity ("\\begin-"*l) < 0
        && N(t) == abs (latex_arity ("\\begin-"*l))+1) {
      r= compound (l*"*");
      for (int i=0; i<n; i++)
        r << finalize_misc (t[i]);
      return r;
    }
    if (n > 1 && is_var_compound (t, "algo-if-else-if")
        && (is_var_compound (t[N(t)-1], "document", 0))) n--;
    for (i=0; i<n; i++)
      r[i]= finalize_misc (t[i]);
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
  string style, lan= "";
  bool is_document= is_compound (t1, "!file", 1);
  if (is_document) t1= t1[0];
  if (is_compound (t1, "!language", 2)) {
    lan= t1[1]->label;
    t1 = t1[0];
  }
  textm_appendices= false;
  textm_unicode   = false;
  textm_natbib    = false;
  command_type ("!em") = "false";
  //cout << "\n\nt1= " << t1 << "\n\n";
  tree t2= is_document? filter_preamble (t1): t1;
  //cout << "\n\nt2= " << t2 << "\n\n";
  tree t3= parsed_latex_to_tree (t2);
  //cout << "\n\nt3= " << t3 << "\n\n";
  tree t4= finalize_document (t3);
  // cout << "\n\nt4= " << t4 << "\n\n";
  tree t5= is_document? finalize_preamble (t4, style): t4;
  // cout << "\n\nt5= " << t5 << "\n\n";
  tree t6= handle_improper_matches (t5);
  //cout << "\n\nt6= " << t6 << "\n\n";
  if ((!is_document) && is_func (t6, DOCUMENT, 1)) t6= t6[0];
  tree t7= upgrade_tex (t6);
  //cout << "\n\nt7= " << t7 << "\n\n";
  tree t8= finalize_floats (t7);
  // cout << "\n\nt8= " << t8 << "\n\n";
  tree t9= finalize_misc (t8);
  // cout << "\n\nt9= " << t9 << "\n\n";
  tree t10= finalize_textm (t9);
  // cout << "\n\nt10= " << t10 << "\n\n";
  tree t11= drd_correct (std_drd, t10);
  // cout << "\n\nt11= " << t11 << "\n\n";

  if (!exists (url ("$TEXMACS_STYLE_PATH", style * ".ts")))
    style= "generic";
  tree initial (COLLECTION), mods (WITH);

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
    tree the_body   = compound ("body", t13);
    tree the_style  = compound ("style", style);
    tree the_initial= compound ("initial", initial);
    if (textm_natbib)
      the_style= compound ("style", tuple (style, "cite-author-year"));
    if (N (initial) == 0) return tree (DOCUMENT, the_style, the_body);
    else return tree (DOCUMENT, the_style, the_body, the_initial);
  }
  else return t13;
}

tree
latex_document_to_tree (string s) {
  command_type ->extend ();
  command_arity->extend ();
  command_def  ->extend ();
  tree t= parse_latex_document (s, true);
  tree r= latex_to_tree (t);
  command_type ->shorten ();
  command_arity->shorten ();
  command_def  ->shorten ();
  return r;
}
