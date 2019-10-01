
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
#include "font.hpp"

tree upgrade_tex (tree t);
bool textm_class_flag= false;
//bool textm_class_flag= true;
bool textm_appendices= false;
bool textm_unicode   = false;
bool textm_natbib    = false;

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

bool
is_var_compound (tree t, string s) {
  return
    is_compound (t, s) ||
    (is_func (t, APPLY) && t[0] == s) ||
    (is_func (t, EXPAND) && t[0] == s);
}

bool
is_var_compound (tree t, string s, int n) {
  return
    is_compound (t, s, n) ||
    (is_func (t, APPLY, n+1) && t[0] == s) ||
    (is_func (t, EXPAND, n+1) && t[0] == s);
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
  return N(t) > 0 &&
         ((is_func (t, TUPLE) && t[0] == "\\\\")              ||
          (is_func (t, TUPLE) && t[0] == "\\author")          ||
          (is_func (t, TUPLE) && t[0] == "\\begin-document")  ||
          (is_func (t, TUPLE) && t[0] == "\\begingroup")      ||
          (is_func (t, TUPLE) && t[0] == "\\date")            ||
          (is_func (t, TUPLE) && t[0] == "\\declaretheorem")  ||
          (is_func (t, TUPLE) && t[0] == "\\declaretheorem*") ||
          (is_func (t, TUPLE) && t[0] == "\\def")             ||
          (is_func (t, TUPLE) && t[0] == "\\def*")            ||
          (is_func (t, TUPLE) && t[0] == "\\def**")           ||
          (is_func (t, TUPLE) && t[0] == "\\endgroup")        ||
          (is_func (t, TUPLE) && t[0] == "\\hspace")          ||
          (is_func (t, TUPLE) && t[0] == "\\hyphenation")     ||
          (is_func (t, TUPLE) && t[0] == "\\index")           ||
          (is_func (t, TUPLE) && t[0] == "\\label")           ||
          (is_func (t, TUPLE) && t[0] == "\\lnl")             ||
          (is_func (t, TUPLE) && t[0] == "\\newdef")          ||
          (is_func (t, TUPLE) && t[0] == "\\newenvironment")  ||
          (is_func (t, TUPLE) && t[0] == "\\newenvironment*") ||
          (is_func (t, TUPLE) && t[0] == "\\newenvironment**")||
          (is_func (t, TUPLE) && t[0] == "\\newtheorem")      ||
          (is_func (t, TUPLE) && t[0] == "\\newtheorem*")     ||
          (is_func (t, TUPLE) && t[0] == "\\nllabel")         ||
          (is_func (t, TUPLE) && t[0] == "\\noindent*")       ||
          (is_func (t, TUPLE) && t[0] == "\\pagenumbering")   ||
          (is_func (t, TUPLE) && t[0] == "\\setcounter")      ||
          (is_func (t, TUPLE) && t[0] == "\\setlength")       ||
          (is_func (t, TUPLE) && t[0] == "\\maketitle")       ||
          (is_func (t, TUPLE) && t[0] == "\\sloppy")          ||
          (is_func (t, TUPLE) && t[0] == "\\SetKw")           ||
          (is_func (t, TUPLE) && t[0] == "\\SetKwData")       ||
          (is_func (t, TUPLE) && t[0] == "\\SetKwInOut")      ||
          (is_func (t, TUPLE) && t[0] == "\\SetKwInput")      ||
          (is_func (t, TUPLE) && t[0] == "\\SetKwFunction")   ||
          (is_func (t, TUPLE) && t[0] == "\\blx")             ||
          (is_func (t, TUPLE) && t[0] == "\\elx")             ||
           is_vertical_space (t));
}

static bool
is_begin (tree t) {
  return is_tuple (t) && N(t) > 0 && starts (as_string (t[0]), "\\begin-");
}

static bool
is_end (tree t) {
  return is_tuple (t) && N(t) > 0 && starts (as_string (t[0]), "\\end-");
}

static bool
is_enunciation (string s) {
  if (ends (s, "*")) s= s (0, N(s)-1);
  return latex_type (s) == "enunciation";
}

static bool
is_block_algorithm (tree t) {
  if (!is_tuple (t) || N(t) == 0) return false;
  string s= as_string (t[0]);
  s= s (1, N(s));
  if (ends (s, "*")) s= s (0, N(s)-1);
  static tree a (CONCAT);
  if (N(a) == 0) {
    a << "BEGIN" << "BODY" << "Begin" << "Call" << "ELSE" << "ELSIF"
      << "ENDBODY" << "ENDFOR" << "ENDIF" << "ENDINPUTS" << "ENDLOOP"
      << "ENDOUTPUTS" << "ENDWHILE" << "ElsIf" << "Else" << "ElseIf"
      << "EndFor" <<"EndFunction" << "EndIf" << "EndLoop" << "EndProcedure"
      << "EndWhile" << "FOR" << "FORALL" << "For" << "ForAll" << "ForEach"
      << "Function" << "IF" << "INPUTS" << "If" << "KwData" << "KwIn"
      << "KwOut" << "KwResult" << "KwRet" << "LOOP" << "Loop" << "OUTPUTS"
      << "REPEAT" << "RETURN" << "Repeat" << "UNTIL" << "Until" << "WHILE"
      << "While" << "eIf" << "lElse" << "lElseIf" << "lFor" << "lForAll"
      << "lForEach" << "lIf" << "uElse" << "uElseIf" << "uIf";
  }
  return contains (tree (s), A(a));
}

static bool
is_block_environnement (tree t) {
  if (is_block_algorithm (t)) return true;
  if (!is_begin (t) && ! is_end (t))
    return is_block_algorithm (t);
  string s= as_string (t[0]);
  s= replace (s, "*", "");
  return
    is_enunciation (s)           ||
    ends (s, "abstract")         ||
    ends (s, "center")           ||
    ends (s, "description")      ||
    ends (s, "document")         ||
    ends (s, "enumerate")        ||
    ends (s, "figure")           ||
    ends (s, "flushleft")        ||
    ends (s, "flushright")       ||
    ends (s, "itemize")          ||
    ends (s, "list")             ||
    ends (s, "quotation")        ||
    ends (s, "quote")            ||
    ends (s, "sloppypar")        ||
    ends (s, "table")            ||
    ends (s, "tabularx")         ||
    ends (s, "teaserfigure")     ||
    ends (s, "thebibliography")  ||
    ends (s, "theindex")         ||
    ends (s, "titlepage")        ||
    ends (s, "trivlist")         ||
    ends (s, "verbatim")         ||
    ends (s, "verse")            ||
    ends (s, "multicols")        ||
    ends (s, "part")             ||
    ends (s, "chapter")          ||
    ends (s, "section")          ||
    ends (s, "indent")           ||
    ends (s, "tmparsep")         ||
    ends (s, "tmparmod");
}

bool
is_sectionnal (tree t) {
  return (is_func (t, TUPLE, 2) && t[0] == "\\section")            ||
         (is_func (t, TUPLE, 2) && t[0] == "\\section*")           ||
         (is_func (t, TUPLE, 3) && t[0] == "\\section*")           ||
         (is_func (t, TUPLE, 2) && t[0] == "\\subsection")         ||
         (is_func (t, TUPLE, 2) && t[0] == "\\subsection*")        ||
         (is_func (t, TUPLE, 3) && t[0] == "\\subsection*")        ||
         (is_func (t, TUPLE, 2) && t[0] == "\\subsubsection")      ||
         (is_func (t, TUPLE, 2) && t[0] == "\\subsubsection*")     ||
         (is_func (t, TUPLE, 3) && t[0] == "\\subsubsection*")     ||
         (is_func (t, TUPLE, 2) && t[0] == "\\part")               ||
         (is_func (t, TUPLE, 2) && t[0] == "\\part*")              ||
         (is_func (t, TUPLE, 3) && t[0] == "\\part*")              ||
         (is_func (t, TUPLE, 2) && t[0] == "\\chapter")            ||
         (is_func (t, TUPLE, 2) && t[0] == "\\chapter*")           ||
         (is_func (t, TUPLE, 3) && t[0] == "\\chapter*")           ||
         (is_func (t, TUPLE)    && t[0] == "\\bibliography");
}

tree
kill_space_invaders (tree t, char &status) {
  // cout << "kill_space_invaders (" << status << "): " << t << LF;
  if (is_atomic (t)) return t;
  //if (is_tuple (t, "\\blx") || is_tuple (t, "\\elx")) return t;
  if (!is_concat (t)) {
    tree r(L(t), N(t));
    int i= is_tuple (t)? 1:0;
    if (i == 1) r[0]= copy(t[0]);
    for (; i<N(t); i++)
      r[i]= kill_space_invaders (t[i], status);
    return r;
  }
  tree r = concat ();
  for (int i=0; i<N(t); i++) {
    tree u= t[i];
    if (is_concat (u)) r << kill_space_invaders (u, status);
    else if (is_sectionnal (u) || is_block_environnement (u)) {
        if (is_sectionnal (u) || is_begin (u)) {
          if (status != 'N') {
            r << "\n";
            status = 'N';
          }
        }
        r << kill_space_invaders (u, status);
        i++;
        while (i<N(t) && (might_not_be_typesetted (t[i]) ||
             t[i] == " "  || t[i] == "\t" || t[i] == "\n")) {
          if (might_not_be_typesetted (t[i]))
            r << kill_space_invaders (t[i], status);
          i++;
        }
        if (is_sectionnal (u) || is_end (u)) {
          if (status != 'N') {
            r << "\n";
            status = 'N';
          }
        }
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

static tree
simplify_unary_concat (tree t) {
  if (is_atomic (t)) return t;
  if (is_concat (t) && N(t) == 1) return t[0];
  int i, n= N(t);
  tree r (L(t));
  for (i=0; i<n; i++)
    r << simplify_unary_concat (t[i]);
  return r;
}

tree
kill_space_invaders (tree t) {
  char status = 'N';
  t= simplify_unary_concat (t);
  return kill_space_invaders (t, status);
}

/******************************************************************************
* Set extra fonts
******************************************************************************/

tree set_special_fonts (tree t, string lan);

static bool
is_cjk_code (int code) {
  return (code >= 0x4E00  && code <= 0x9FFF ) ||
         (code >= 0x3400  && code <= 0x4DBF ) ||
         (code >= 0x20000 && code <= 0x2A6DF) ||
         (code >= 0x2A700 && code <= 0x2B73F) ||
         (code >= 0x2B740 && code <= 0x2B81F) ||
         (code >= 0xF900  && code <= 0xFAFF ) ||
         (code >= 0x2F800 && code <= 0x2FA1F) ||
         (code >= 0x2F00  && code <= 0x2FDF ) ||
         (code >= 0x2E80  && code <= 0x2EFF ) ||
         (code >= 0x31C0  && code <= 0x31EF );
}

static bool
is_katakana_code (int code) {
  return (code >= 0x30A0  && code <= 0x30FF) ||
         (code >= 0x31F0  && code <= 0x31FF) ||
         (code >= 0x3200  && code <= 0x32FF) ||
         (code >= 0xFF00  && code <= 0xFFEF) ||
         (code >= 0x1B000 && code <= 0x1B0FF);
}

static bool
is_hiragana_code (int code) {
  return (code >= 0x3040  && code <= 0x309F ) ||
         (code >= 0x1B000 && code <= 0x1B0FF);
}

static bool
is_hangul_code (int code) {
  return (code >= 0xAC00 && code <= 0xD7AF) ||
         (code >= 0x1100 && code <= 0x11FF) ||
         (code >= 0x3130 && code <= 0x318F) ||
         (code >= 0x3200 && code <= 0x32FF) ||
         (code >= 0xA960 && code <= 0xA97F) ||
         (code >= 0xD7B0 && code <= 0xD7FF) ||
         (code >= 0xFF00 && code <= 0xFFEF);
}

static bool
is_cyrillic_code (int code) {
  return (code >= 0x400  && code <= 0x4FF ) ||
         (code >= 0x2DE0 && code <= 0x2DFF) ||
         (code >= 0xA640 && code <= 0xA69F);
}

static string
lang_of_code (int code) {
  if (is_cyrillic_code (code)) return "cyrillic";
  if (is_cjk_code      (code)) return "cjk";
  if (is_hiragana_code (code)) return "japanese";
  if (is_katakana_code (code)) return "japanese";
  if (is_hangul_code   (code)) return "korean";
  return "";
}

static int
codepoint_of_tree (tree t) {
  if (!is_atomic (t)) return -1;
  string s= as_string (t);
  if (!starts (s, "<#") || !ends (s, ">"))
    return -1;
  s= s (2, N(s)-1);
  return from_hexadecimal (s);
}

static bool
is_special_codepoint_entity (tree t, string lan) {
  int code= codepoint_of_tree (t);
  return code != -1 && lang_of_code (code) != lan;
}

static string
font_of_tree (tree t) {
  if (!is_atomic (t)) return "roman";
  int code= codepoint_of_tree (t);
  string lang= lang_of_code (code);
  if (lang == "cyrillic")
    return "cyrillic";
  if (lang == "cjk")
    return default_chinese_font_name ();
  if (lang == "japanese")
    return default_japanese_font_name ();
  if (lang == "korean")
    return default_korean_font_name ();
  return "roman";
}

static tree
set_special_fonts (tree t, string lan, string &current_lang) {
  if (is_atomic (t) && is_special_codepoint_entity (t, current_lang)
      && is_special_codepoint_entity (t, lan))
    return concat (tree (SET, "font", font_of_tree (t)), t,
                   tree (RESET, "font"));
  if (is_atomic (t))
    return t;
  int i, n= N(t);
  tree r (L(t));
  if (is_concat (t)) {
    for (i=0; i<n; i++) {
      if (t[i] == " ")
        r << t[i];
      else {
        string charlang= lang_of_code (codepoint_of_tree (t[i]));
        if (current_lang != charlang && charlang != lan) {
          if (current_lang != lan)
            r << tree (RESET, "font");
          current_lang= charlang;
          r << tree (SET, "font", font_of_tree (t[i]));
        }
        else if (current_lang != charlang && charlang == lan) {
          current_lang= lan;
          r << tree (RESET, "font");
        }
        r << set_special_fonts (t[i], lan, current_lang);
      }
    }
  }
  else
    for (i=0; i<n; i++)
      r << set_special_fonts (t[i], lan);
  return r;
}

tree
set_special_fonts (tree t, string lan) {
  // Unfortunatelly there is now way to distinguish if it is Chinese, Japanese,
  // Korean or Taiwanese.
  if (lan == "chinese"  || lan == "taiwanese" || lan == "japanese")
    lan= "cjk";
  string current_lang= lan;
  return set_special_fonts (t, lan, current_lang);
}

/******************************************************************************
* Preprocess preamble
******************************************************************************/

tree latex_symbol_to_tree (string s);
tree parsed_latex_to_tree (tree t);

static bool
is_declaration (tree u) {
  return (is_tuple (u, "\\def") ||
          is_tuple (u, "\\def*") ||
          is_tuple (u, "\\def**") ||
          is_tuple (u, "\\newenvironment") ||
          is_tuple (u, "\\newenvironment*") ||
          is_tuple (u, "\\newenvironment**") ||
          is_tuple (u, "\\newtheorem") ||
          is_tuple (u, "\\newtheorem*"));
}

static bool
is_custom_maketitle (tree t) {
  if (!is_concat (t)) return false;
  bool flag1= false, flag2= false;
  for (int i=0; i<N(t); i++) {
    flag1= flag1 || is_tuple (t[i], "\\newpage");
    flag2= flag2 || is_tuple (t[i], "\\maketitle");
  }
  return flag1 && flag2;
}

static bool
is_doc_data (tree t) {
  if (is_func (t, APPLY) && N(t) > 0 && t[0] == "\\doc-data") return true;
  if (!is_concat (t)) return false;
  for (int i=0; i<N(t); i++) {
    if (is_doc_data (t[i])) return true;
    return false;
  }
}

tree
filter_preamble (tree t) {
  int i, n=N(t);
  bool in_preamble= true;
  tree r (CONCAT);
  tree doc (CONCAT);
  tree preamble (CONCAT);
  tree metadata (CONCAT);
  tree latex_class;

  for (i=0; i<n; i++) {
    tree u= t[i];
    if (in_preamble) {
      if (u == tuple ("\\begin-document")) {
        r << u;
        if (N(preamble) > 0)
          r << tuple ("\\begin-hide-preamble") << A(preamble)
            << tuple ("\\end-hide-preamble") << "\n";
        in_preamble= false;
      }
      else if (is_tuple (u, "\\documentclass")  ||
               is_tuple (u, "\\documentclass*") ||
               is_tuple (u, "\\documentstyle")  ||
               is_tuple (u, "\\documentstyle*")) {
        doc << u;
        latex_class = u;
        if (is_tuple (u, "\\documentstyle*", 2) ||
            is_tuple (u, "\\documentclass*", 2)) {
          tree opt= parsed_latex_to_tree (u[1]);
          if (is_atomic (opt) && occurs ("11pt", opt->label))
            preamble << tuple ("\\env-init", "font-base-size", "11");
          if (is_atomic (opt) && occurs ("12pt", opt->label))
            preamble << tuple ("\\env-init", "font-base-size", "12");
        }
      }
      else if (is_tuple (u, "\\usepackage", 1)) {
        tree opt= parsed_latex_to_tree (u[1]);
        if (is_atomic (opt) && occurs ("palatino", opt->label))
          preamble << tuple ("\\env-init", "font", "pagella");
      }
      else if (is_tuple (u, "\\geometry", 1))
        preamble << u << "\n" << "\n";
      else if (is_declaration (u))
        preamble << u << "\n" << "\n";
      else if (is_tuple (u, "\\newdef", 2))
        preamble << tuple("\\newtheorem", u[1], u[2]) << "\n" << "\n";
      else if (is_tuple (u, "\\declaretheorem", 1) ||
               is_tuple (u, "\\declaretheorem*", 2))
        preamble << tuple("\\newtheorem", u[N(u)-1], u[N(u)-1]) << "\n" << "\n";
      else if (is_tuple (u, "\\SetKw", 2)          ||
               is_tuple (u, "\\SetKwData", 2)      ||
               is_tuple (u, "\\SetKwInOut", 2)     ||
               is_tuple (u, "\\SetKwInput", 2)     ||
               is_tuple (u, "\\SetKwFunction", 2))
        preamble << u << "\n" << "\n";
      else if (is_tuple (u, "\\conferenceinfo")    ||
               is_tuple (u, "\\CopyrightYear")     ||
               is_tuple (u, "\\crdata")) {
        preamble << u << "\n" << "\n";
      }
      else if (i+6 < N(t) &&
               is_tuple (t[i  ], "\\begingroup") &&
               is_tuple (t[i+1], "\\blx") &&
               is_tuple (t[i+2], "\\endgroup") &&
               is_declaration (t[i+3]) &&
               is_tuple (t[i+4], "\\begingroup") &&
               is_tuple (t[i+5], "\\elx") &&
               is_tuple (t[i+6], "\\endgroup")) {
        preamble << A(t(i,i+7)) << "\n" << "\n"; i += 6; continue; }
      else if (is_tuple (u, "\\itm"))
        preamble << u << "\n" << "\n";
      else if (is_tuple (u, "\\abovedisplayshortskip") ||
               is_tuple (u, "\\abovedisplayskip") ||
               is_tuple (u, "\\belowdisplayshortskip") ||
               is_tuple (u, "\\belowdisplayskip") ||
               is_tuple (u, "\\columnsep") ||
               is_tuple (u, "\\evensidemargin") ||
               is_tuple (u, "\\footnotesep") ||
               is_tuple (u, "\\footskip") ||
               is_tuple (u, "\\headheight") ||
               is_tuple (u, "\\headsep") ||
               is_tuple (u, "\\jot") ||
               is_tuple (u, "\\marginparwidth") ||
               is_tuple (u, "\\mathindent") ||
               is_tuple (u, "\\oddsidemargin") ||
               is_tuple (u, "\\parindent") ||
               is_tuple (u, "\\textheight") ||
               is_tuple (u, "\\textwidth") ||
               is_tuple (u, "\\columnwidth") ||
               is_tuple (u, "\\linewidth") ||
               is_tuple (u, "\\topmargin") ||
               is_tuple (u, "\\topskip")) {
        i++;
        tree var= latex_symbol_to_tree (u[0]->label);
        string val;
        if (i<n && t[i] == "=") i++;
        while (i<n && is_atomic (t[i]) && N(t[i]->label) == 1 &&
               (is_alpha (t[i]->label[0]) ||
                is_numeric (t[i]->label[0]) ||
                t[i]->label[0] == '-' ||
                t[i]->label[0] == '.'))
          val << t[i++]->label;
        if (ends (val, "mm") || ends (val, "cm") || ends (val, "in") ||
            ends (val, "dd") || ends (val, "dc") || ends (val, "pc") ||
            ends (val, "pt") || ends (val, "em")) {
          if (N(val)>6 && val(N(val)-6, N(val)-2) == "true")
            val= val (0, N(val)-6) * val (N(val)-2, N(val));
          if (N(val)>2 && !is_alpha (val[N(val)-3]))
            preamble << tuple ("\\env-init", var, val) << "\n" << "\n";
        }
      }
    }
    else if (is_custom_maketitle (u))
      doc << tuple ("\\custom-maketitle");
    else if (is_metadata_env (u)) {
      string s= as_string (u[0]);
      s= "\\end-" * s(7,N(s));
      while (i<n && !is_tuple (t[i], s)) i++;
    }
    else if (!is_metadata (u))
      doc << u;
  }
  if (in_preamble) return t;
  metadata = collect_metadata (t, latex_class);
  // cout << "Parsed metadatas: " << metadata << "\n\n";
  r << A(metadata);
  r << A(doc);
  for (i=0; i<N(r); i++)
    if (is_tuple (r[i], "\\custom-maketitle")) {
      for (int j=0; j<i; j++)
        if (is_doc_data (r[j])) {
          r= r (0, j) * r (j+1, i) * r (j, j+1) * r (i+1, N(r));
          break;
        }
      break;
    }
  return r;
}

/******************************************************************************
* Import macro as pictures
******************************************************************************/

bool
find_latex_previews (tree t) {
  if (is_atomic (t)) return false;
  else if (is_tuple (t, "\\latex_preview", 2))
    return true;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (find_latex_previews (t[i]))
        return true;
  }
  return false;
}

tree
substitute_latex_previews (tree t, array<tree> a, int &i) {
  if (N(a) <= i);
  else if (is_atomic (t));
  else if (is_tuple (t, "\\latex_preview", 2)) {
    t[0]= "\\picture-mixed";
    t[1]= a[i++];
  }
  else if (is_tuple (t, "\\def") || is_tuple (t, "\\def*")
      || is_tuple (t, "\\def**") || is_tuple (t, "\\newenvironment**") ||
      is_tuple (t, "\\newenvironment") || is_tuple (t, "\\newenvironment*"));
  else {
    int j, n= N(t);
    for (j=0; j<n; j++)
      t[j]= substitute_latex_previews (t[j], a, i);
  }
  return t;
}

static int
count_unbalanced_preview (tree t) {
  if (!is_concat (t)) return 0;
  int i, n= N(t), count= 0;
  for (i=0; i<n; i++) {
    tree v= t[i];
    if (is_tuple (v, "\\latex_preview", 2)
        && starts (as_string (v[1]), "begin-")) count++;
    if (is_tuple (v, "\\latex_preview", 2)
        && starts (as_string (v[1]), "end-")) count--;
    if (is_concat (v))
      count += count_unbalanced_preview (v);
  }
  return count;
}

static tree
merge_environment_previews (tree t) {
  if (is_atomic (t)) return t;
  else if (is_tuple (t, "\\def") || is_tuple (t, "\\def*")
      || is_tuple (t, "\\def**") || is_tuple (t, "\\newenvironment**") ||
      is_tuple (t, "\\newenvironment") || is_tuple (t, "\\newenvironment*"))
    return t;
  int i, n= N(t);
  tree r (L(t));
  string name= "";
  tree code;
  bool in_env= false;
  for (i=0; i<n; i++) {
    tree v= t[i];
    if (!in_env && is_concat (t) && is_tuple (v, "\\latex_preview", 2)
        && starts (as_string (v[1]), "begin-")) {
      in_env= true;
      name= as_string (v[1]);
      code= v[2];
    }
    if (in_env && is_concat (t) && is_tuple (v, "\\latex_preview", 2)
        && starts (as_string (v[1]), "end-")) {
      in_env= false;
      code= concat (code, v[2]);
      r << tuple ("\\latex_preview", name, code);
      name= "";
    }
    else if (is_concat (t) && count_unbalanced_preview (v) != 0) {
      tree tmp (CONCAT);
      int j, m= N(v);
      for (j=0;   j<m; j++) tmp << v[j];
      for (j=i+1; j<n; j++) tmp << t[j];
      t= tmp;
      n= N(t);
      i= -1;
    }
    else if (!in_env)
      r << merge_environment_previews (v);
  }
  return r;
}

tree
latex_fallback_on_pictures (string s, tree t) {
  if (!find_latex_previews (t)) return t;
  int i= 0;
  t= merge_environment_previews (t);
  array<tree> a= latex_preview (s, t);
  return substitute_latex_previews (t, a, i);
}

/******************************************************************************
* Transform parsed tex/latex trees into texmacs trees
******************************************************************************/

#define l2e parsed_latex_to_tree
#define t2e parsed_text_to_tree
#define m2e latex_modifier_to_tree
#define var_m2e var_latex_modifier_to_tree
#define mod_m2e mod_latex_modifier_to_tree
#define v2e latex_verbarg_to_string
tree l2e (tree);
tree latex_command_to_tree (tree t);
string latex_to_texmacs_languages (string s);

tree
latex_symbol_to_tree (string s) {
  if (s == "") return "";
  if (s[0] == '\\') {
    s= s(1,N(s));
    if ((s == "ldots" || s == "dots" || s == "dotso")
        && (command_type ("!mode") != "math")) return "...";
    if (s == "\n")     return tree (APPLY, "!emptyline");
    if (latex_type ('\\' * s) == "command") {
      if (s == " ")      return " ";
      if (s == "-")      return "";
      if (s == "/")      return "";
      if (s == "lq")     return "`";
      if (s == "rq")     return "'";
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
      if (s == "par")          return "\n";
      if (s == "P")            return tree (APPLY, "paragraphsign");;
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
      if (s == "!")          return tree (SPACE, "-0.17em");
      if (s == ",")          return tree (SPACE, "0.17em");
      if (s == "thinspace")  return tree (SPACE, "0.17em");
      if (s == ":")          return tree (SPACE, "0.22em");
      if (s == "enspace")    return tree (SPACE, "0.5em");
      if (s == ";")          return tree (SPACE, "0.27em");
      if (s == "quad")       return tree (SPACE, "1em");
      if (s == "qquad")      return tree (SPACE, "2em");
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
      if (s == "hrulefill")  return tree (APPLY, "hrule");
      if (s == "hdashline")  return "";
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
      if (s == "null")       return "";
      if (s == "unskip")     return "";
      if (s == "protect")    return "";
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
      if (s == "bysame") return "---";
      if (s == "MR") return "MR ";
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
      if (s == "tmprecdot")     return "<precdot>";
      
      if (starts (s, "up")) {
        if (s == "upalpha") return "<up-alpha>";
        if (s == "upbeta") return "<up-beta>";
        if (s == "upgamma") return "<up-gamma>";
        if (s == "updelta") return "<up-delta>";
        if (s == "upepsilon") return "<up-epsilon>";
        if (s == "upvarepsilon") return "<up-varepsilon>";
        if (s == "upzeta") return "<up-zeta>";
        if (s == "upeta") return "<up-eta>";
        if (s == "uptheta") return "<up-theta>";
        if (s == "upvartheta") return "<up-vartheta>";
        if (s == "upiota") return "<up-iota>";
        if (s == "upkappa") return "<up-kappa>";
        if (s == "uplambda") return "<up-lambda>";
        if (s == "upmu") return "<up-mu>";
        if (s == "upnu") return "<up-nu>";
        if (s == "upomicron") return "<up-omicron>";
        if (s == "uppi") return "<up-pi>";
        if (s == "upvarpi") return "<up-varpi>";
        if (s == "uprho") return "<up-rho>";
        if (s == "upvarrho") return "<up-varrho>";
        if (s == "upsigma") return "<up-sigma>";
        if (s == "upvarsigma") return "<up-varsigma>";
        if (s == "uptau") return "<up-tau>";
        if (s == "upupsilon") return "<up-upsilon>";
        if (s == "upphi") return "<up-phi>";
        if (s == "upvarphi") return "<up-varphi>";
        if (s == "upchi") return "<up-chi>";
        if (s == "uppsi") return "<up-psi>";
        if (s == "upomega") return "<up-omega>";
      }

      if (starts (s, "Up")) {
        if (s == "Upalpha") return "<Alpha>";
        if (s == "Upbeta") return "<Beta>";
        if (s == "Upgamma") return "<Gamma>";
        if (s == "Updelta") return "<Delta>";
        if (s == "Upepsilon") return "<Epsilon>";
        if (s == "Upzeta") return "<Zeta>";
        if (s == "Upeta") return "<Eta>";
        if (s == "Uptheta") return "<Theta>";
        if (s == "Upiota") return "<Iota>";
        if (s == "Upkappa") return "<Kappa>";
        if (s == "Uplambda") return "<Lambda>";
        if (s == "Upmu") return "<Mu>";
        if (s == "Upnu") return "<Nu>";
        if (s == "Upomicron") return "<Omicron>";
        if (s == "Uppi") return "<Pi>";
        if (s == "Uprho") return "<Rho>";
        if (s == "Upsigma") return "<Sigma>";
        if (s == "Uptau") return "<Tau>";
        if (s == "Upupsilon") return "<Upsilon>";
        if (s == "Upphi") return "<Phi>";
        if (s == "Upchi") return "<Chi>";
        if (s == "Uppsi") return "<Psi>";
        if (s == "Upomega") return "<Omega>";
      }

      return "<" * s * ">";
    }

    if (latex_type (s) == "texmacs") {
      if (s == "tmdummy")         return "";
      if (s == "tmbsl")           return "\\";
      if (s == "withTeXmacstext") return tree (COMPOUND, "with-TeXmacs-text");
    }

    if ((latex_type (s) == "modifier") && (latex_arity (s) == 0)) {
      if (s == "centering")
        return tree (SET, PAR_MODE, "center");
      if (s == "raggedright" || s == "flushleft")
        return tree (SET, PAR_MODE, "left");
      if (s == "raggedleft" || s == "flushright")
        return tree (SET, PAR_MODE, "right");

      if (s == "normalfont") return concat (tree (SET, FONT_FAMILY, "rm"),
          tree (SET, FONT_SHAPE , "right"), tree (SET, FONT_SERIES, "medium"));

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

      failed_error << "The symbol was \\" << s << "\n";
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
      if (s == "columnwidth")           return "tex-column-width";
      if (s == "linewidth")             return "tex-line-width";
      if (s == "topmargin")             return "tex-top-margin";
      if (s == "topskip")               return "tex-top-skip";
      if (s == "smallskipamount")       return "0.5fn";
      if (s == "medskipamount")         return "1fn";
      if (s == "bigskipamount")         return "2fn";
    }

    // FIXME: avoid redefinition of command_type in parsetex.cpp
    if (latex_type (s) == "name" || latex_type (s) == "user") {
      if (s == "abstractname")    return "abstract-text";
      if (s == "appendixname")    return "appendix-text";
      if (s == "contentsname")    return "table-of-contents-text";
      if (s == "figurename")      return "figure-text";
      if (s == "indexname")       return "index-text";
      if (s == "listfigurename")  return "list-of-figures-text";
      if (s == "listtablename")   return "list-of-tables-text";
      if (s == "partname")        return "part-text";
      if (s == "refname")         return "bibliography-text";
      if (s == "tablename")       return "table-text";
    }

    if (latex_type (s) == "ignore") return "";

    if (latex_type (s) == "operator" || latex_type (s) == "control") return s;
    if (s == "bignone") return tree (BIG, ".");
    if (s == "Return") return tree (APPLY, "algo-return");
    if (s == "tmhrule") return tree (APPLY, "hrule");
    if (s == "og") return "\x13 "; // open guillemets (French)
    if (s == "fg") return "\x14"; // close guillemets (French)
    if (latex_type (s) == "big-symbol") {
      if (s(0,3)=="big") return tree (BIG, s(3,N(s)));
      else return tree (BIG, s);
    }

    if ((N(s) > 6) && (s(0,6) == "begin-")) {
      if (s == "begin-center") return tree (BEGIN, "padded-center");
      if (s == "begin-flushleft") return tree (BEGIN, "padded-left-aligned");
      if (s == "begin-flushright") return tree (BEGIN, "padded-right-aligned");
    }
    if ((N(s) > 4) && (s(0,4) == "end-")) {
      if (s == "end-center") return tree (END, "padded-center");
      if (s == "end-flushleft") return tree (END, "padded-left-aligned");
      if (s == "end-flushright") return tree (END, "padded-right-aligned");
    }

    if ((N(s) > 6) && (s(0,6) == "begin-")) {
      if (s == "begin-th") return tree (BEGIN, "theorem");
      if (s == "begin-thm") return tree (BEGIN, "theorem");
      if (s == "begin-prop") return tree (BEGIN, "proposition");
      if (s == "begin-lem") return tree (BEGIN, "lemma");
      if (s == "begin-cor") return tree (BEGIN, "corollary");
      if (s == "begin-corr") return tree (BEGIN, "corollary");
      if (s == "begin-pf") return tree (BEGIN, "proof");
      if (s == "begin-dem") return tree (BEGIN, "proof");
      if (s == "begin-preuve") return tree (BEGIN, "proof");
      if (s == "begin-IEEEproof") return tree (BEGIN, "proof");
      if (s == "begin-ax") return tree (BEGIN, "axiom");
      if (s == "begin-def") return tree (BEGIN, "definition");
      if (s == "begin-dfn") return tree (BEGIN, "definition");
      if (s == "begin-defn") return tree (BEGIN, "definition");
      if (s == "begin-not") return tree (BEGIN, "notation");
      if (s == "begin-ex") return tree (BEGIN, "example");
      if (s == "begin-exa") return tree (BEGIN, "example");
      if (s == "begin-rem") return tree (BEGIN, "remark");
      if (s == "begin-war") return tree (BEGIN, "warning");
      if (s == "begin-conv") return tree (BEGIN, "convention");
      if (s == "begin-exe") return tree (BEGIN, "exercise");
      if (s == "begin-exc") return tree (BEGIN, "exercise");
      if (s == "begin-exo") return tree (BEGIN, "exercise");
      if (s == "begin-prob") return tree (BEGIN, "problem");
      if (s == "begin-sol") return tree (BEGIN, "solution");
      if (s == "begin-ans") return tree (BEGIN, "answer");
      if (s == "begin-acks") return tree (BEGIN, "acknowledgments");
      return tree (BEGIN, s(6,N(s)));
    }
    if ((N(s) > 4) && (s(0,4) == "end-")) {
      if (s == "end-th") return tree (END, "theorem");
      if (s == "end-thm") return tree (END, "theorem");
      if (s == "end-prop") return tree (END, "proposition");
      if (s == "end-lem") return tree (END, "lemma");
      if (s == "end-cor") return tree (END, "corollary");
      if (s == "end-corr") return tree (END, "corollary");
      if (s == "end-pf") return tree (END, "proof");
      if (s == "end-dem") return tree (END, "proof");
      if (s == "end-preuve") return tree (END, "proof");
      if (s == "end-IEEEproof") return tree (END, "proof");
      if (s == "end-ax") return tree (END, "axiom");
      if (s == "end-def") return tree (END, "definition");
      if (s == "end-dfn") return tree (END, "definition");
      if (s == "end-defn") return tree (END, "definition");
      if (s == "end-not") return tree (END, "notation");
      if (s == "end-ex") return tree (END, "example");
      if (s == "end-exa") return tree (END, "example");
      if (s == "end-rem") return tree (END, "remark");
      if (s == "end-war") return tree (END, "warning");
      if (s == "end-conv") return tree (END, "convention");
      if (s == "end-exe") return tree (END, "exercise");
      if (s == "end-exc") return tree (END, "exercise");
      if (s == "end-exo") return tree (END, "exercise");
      if (s == "end-prob") return tree (END, "problem");
      if (s == "end-sol") return tree (END, "solution");
      if (s == "end-ans") return tree (END, "answer");
      if (s == "end-acks") return tree (END, "acknowledgments");
      return tree (END, s(4,N(s)));
    }
    
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
string_arg (tree t, bool url) {
  if (is_atomic (t)) return t->label;
  else if (is_concat (t)) {
    string r;
    int i, n= N(t);
    for (i=0; i<n; i++)
      r << string_arg (t[i], url);
    return r;
  }
  else if (is_func (t, RSUB, 1))
    return "_" * string_arg (t[0], url);
  else if (is_func (t, RSUP, 1))
    return "^" * string_arg (t[0], url);
  else if (is_func (t, APPLY, 1) && t[0] == "nbsp" && !url)
    return " ";
  else if (is_func (t, APPLY, 1) && t[0] == "nbsp" && url)
    return "~";
  else if (is_func (t, APPLY, 1) && t[0] == "emdash")
    return "---";
  else {
    //cout << "t= " << t << "\n";
    return "";
  }
}

tree
latex_key_arg (tree t) {
  string s= string_arg (l2e(t));
  if (s == "") {
    t= l2e(t);
    if (is_concat (t) && N(t) == 3 && is_apply (t[1], "op", 1)) {
      if (t[1][1] == "<leftarrow>")
        return "left";
      else if (t[1][1] == "<rightarrow>")
        return "right";
      else if (t[1][1] == "<uparrow>")
        return "up";
      else if (t[1][1] == "<downarrow>")
        return "down";
      else
        return t;
    }
    else
      return t;
  }
  string r= "";
  int i, n= N(s);
  for (i=0; i<n; i++) {
    if (test (s, i, "Shift+"))
      r << "S-", i+=5;
    else if (test (s, i, "Ctrl+"))
      r << "C-", i+=4;
    else if (test (s, i, "Alt+"))
      r << "A-", i+=3;
    else if (test (s, i, "Meta+"))
      r << "M-", i+=4;
    else if (test (s, i, "Hyper+"))
      r << "H-", i+=5;
    else if (is_alpha (s[i]))
      r << locase (s[i]);
    else r << s[i];
  }
  return r;
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
      if ((N(r)>0) && is_atomic (r[N(r)-1])) {
        if (s != " " || r[N(r)-1]->label != " ")
          r[N(r)-1]->label << s;
      }
      else
        r << s;
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
mod_m2e (tree t, string var, string val) {
  if (command_type["!mode"] != "math")
    return m2e (t, var, val);
  else
    return tree (CONCAT,
                 tree (SET, "mode", "text"),
                 tree (SET, copy (var), copy (val)),
                 t2e (t[1], false),
                 tree (RESET, copy (var)),
                 tree (RESET, "mode"));
}

string
url_arg_to_string (tree t) {
  return string_arg (t2e (t, false), true);
}

string
v2e (tree t) {
  return string_arg (t2e (t, false));
}

static bool
is_left_type (string s) {
  return
    (s == "(") || (s == "[") || (s == "\\{") ||
    (s == "\\lvert") || (s == "lVert") ||
    (s == "\\lfloor") || (s == "\\lceil") || (s == "\\langle");
}

static bool
is_right_type (string s) {
  return
    (s == ")") || (s == "]") || (s == "\\}") ||
    (s == "\\rvert") || (s == "\\rVert") ||
    (s == "\\rfloor") || (s == "\\rceil") || (s == "\\rangle");
}

static bool
is_mid_type (string s) {
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
  if (!is_tuple (t) || N(t) != 2 || is_compound (t[0])) return false;
  string br;
  if (is_atomic (t[1]))
    br= t[1]->label;
  else if (is_tuple (t[1]) && N(t[1]) == 1 && is_atomic (t[1][0]))
    br= t[1][0]->label;
  else return false;
  string s= t[0]->label;
  if (starts (s, "\\Big")) s= "\\big" * s(4,N(s));
  if (starts (s, "\\bigg")) s= "\\big" * s(5,N(s));
  if ((s == "\\left") || (s == "\\bigl") ||
      ((s == "\\big") && is_left_type (br))) {
    type= -1;
    return true;
  }
  if ((s == "\\right") || (s == "\\bigr") ||
      ((s == "\\big") && is_right_type (br))) {
    type= 1;
    return true;
  }
  if ((s == "\\middle") || (s == "\\bigm") ||
      ((s == "\\big") && is_mid_type (br))) {
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

static array<string> cref_table;

tree
latex_cref_to_tree (string s) {
  if (N(cref_table) == 0)
    cref_table << string ("fig") << string ("Figure")
               << string ("tab") << string ("Table")
               << string ("alg") << string ("Algorithm")
               << string ("subsec") << string ("Subsection")
               << string ("ssec") << string ("Subsection")
               << string ("sec") << string ("Section")
               << string ("thm") << string ("Theorem")
               << string ("prop") << string ("Proposition")
               << string ("lem") << string ("Lemma")
               << string ("cor") << string ("Corollary")
               << string ("note") << string ("Note")
               << string ("rem") << string ("Remark")
               << string ("def") << string ("Definition")
               << string ("dfn") << string ("Definition")
               << string ("conv") << string ("Convention")
               << string ("war") << string ("Warning")
               << string ("exa") << string ("Example")
               << string ("not") << string ("Notation")
               << string ("prob") << string ("Problem")
               << string ("prb") << string ("Problem")
               << string ("exe") << string ("Exercise")
               << string ("exc") << string ("Exercise")
               << string ("sol") << string ("Solution")
               << string ("eqn") << string ("Equation")
               << string ("ch") << string ("Chapter")
               << string ("th") << string ("Theorem")
               << string ("lm") << string ("Lemma")
               << string ("ax") << string ("Axiom")
               << string ("ex") << string ("Example")
               << string ("eq") << string ("Equation");
  tree t (CONCAT);
  string type= "";
  for (int i=0; i<N(cref_table); i+=2)
    if (type == "" && starts (s, cref_table[i]))
      type= cref_table[i+1];
  for (int i=0; i<N(cref_table); i+=2)
    if (type == "" && ends (s, cref_table[i]))
      type= cref_table[i+1];
  for (int i=0; i<N(cref_table); i+=2)
    if (type == "" && occurs (cref_table[i], s))
      type= cref_table[i+1];
  array<string> a= tokenize (s, ",");
  if (type != "") {
    if (N(a) == 1) t << compound ("localize", type);
    else t << compound ("localize", type * "s");
  }
  for (int i=0; i<N(a); i++) {
    string ss= trim_spaces (a[i]);
    if (i == 0) t << compound ("nbsp");
    else if (i == 1 && N(a) == 2) t << " and" << compound ("nbsp");
    else if (i == N(a) - 1) t << ", and" << compound ("nbsp");
    else t << ", ";
    if (type == "Equation") t << "(";
    t << tree (REFERENCE, ss);
    if (type == "Equation") t << ")";
  }
  return t;
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

string
textm_normalize_length (string len) {
  len= replace (len, " ", "");
  len= replace (len, "tex-text-width", "par");
  len= replace (len, "tex-line-width", "par");
  len= replace (len, "tex-column-width", "par");
  len= replace (len, "tex-text-height", "pag");
  if (len == "par") len= "1par";
  if (len == "pag") len= "1pag";
  return len;
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
        return textm_normalize_length (val);
      }
      start= i+1;
    }
  return "";
}

static array< array<tree> >
tokenize_keys_vals (tree t) {
  array<tree> l1= tokenize_concat (t, A(concat (",")));
  array< array<tree> > l2;
  for (int i= 0; i<N(l1); i++)
    l2 << tokenize_concat (l1[i], A(concat ("=")));
  return l2;
}

static array<tree>
decode_keys_vals (tree t) {
  array< array<tree> > l= tokenize_keys_vals (t);
  int i, n= N(l);
  array<tree> r;
  for (i=0; i<n; i++) {
    if (N(l[i]) != 2) continue;
    r << l2e (l[i][0]) << l2e (l[i][1]);
  }
  return r;
}

static tree
translate_keys (array<tree> a, hashmap<tree,tree> dic) {
  int i, n= N(a);
  tree r (CONCAT);
  for (i=0; i<n-1; i+=2) {
    tree tmp= dic[a[i]];
    if (tmp == "")
      r << tree (SET, a[i], a[i+1]);
    else if (is_tuple (tmp) && N(tmp) == 2)
      r << tree (SET, tmp[0], tmp[1]);
    else
      r << tree (SET, tmp, a[i+1]);
  }
  return r;
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
    string var= v2e(t[1]);
    string val= v2e(t[2]);
    return compound ("new-theorem", var, val);
  }

  if (is_tuple (t, "\\newenvironment", 3)) {
    string var= v2e(t[1]);
    return tree (ASSIGN, var, tree (ENV, l2e (t[2]), l2e (t[3])));
  }
  if (is_tuple (t, "\\newenvironment*", 4)) {
    string var= v2e(t[1]);
    int i, arity= as_int (l2e(t[2])->label);
    tree e (ENV);
    for (i=1; i<=arity; i++) e << as_string (i);
    e << l2e (t[3]);
    e << l2e (t[4]);
    return tree (ASSIGN, var, e);
  }
  if (is_tuple (t, "\\newenvironment**", 5)) {
    string var= v2e(t[1]);
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

  if (is_tuple (t, "\\latex_preview", 2))
    return tree (APPLY, "latex_preview", l2e (t[1]), t[2]);

  if (is_tuple (t, "\\picture-mixed", 2)) {
    return tree (APPLY, "picture-mixed", l2e (t[1]), t[2]);
  }

  if (is_tuple (t, "\\TMDoCoqRecNotationSep", 2)) {
    return tree (WITH, "coq-rec-notation-sep", l2e (t[1]), l2e (t[2]));
  }

  if (is_tuple (t, "\\part*", 2)          ||
      is_tuple (t, "\\chapter*", 2)       ||
      is_tuple (t, "\\section*", 2)       ||
      is_tuple (t, "\\subsection*", 2)    ||
      is_tuple (t, "\\subsubsection*", 2) ||
      is_tuple (t, "\\paragraph*", 2)     ||
      is_tuple (t, "\\subparagraph*", 2)  ||
      is_tuple (t, "\\subsubparagraph*", 2)) {
    string s= as_string (t[0]);
    s= s(1, N(s)-1);
    return tree (APPLY, s, l2e (t[2]));
  }

  if (is_tuple (t, "\\begin-part")            ||
      is_tuple (t, "\\begin-part*")           ||
      is_tuple (t, "\\begin-part**")          ||
      is_tuple (t, "\\begin-chapter")         ||
      is_tuple (t, "\\begin-chapter*")        ||
      is_tuple (t, "\\begin-chapter**")       ||
      is_tuple (t, "\\begin-section")         ||
      is_tuple (t, "\\begin-section*")        ||
      is_tuple (t, "\\begin-section**")       ||
      is_tuple (t, "\\begin-subsection")      ||
      is_tuple (t, "\\begin-subsection*")     ||
      is_tuple (t, "\\begin-subsection**")    ||
      is_tuple (t, "\\begin-subsubsection")   ||
      is_tuple (t, "\\begin-subsubsection*")  ||
      is_tuple (t, "\\begin-subsubsection**") ||
      is_tuple (t, "\\begin-paragraph")       ||
      is_tuple (t, "\\begin-paragraph*")      ||
      is_tuple (t, "\\begin-paragraph**")     ||
      is_tuple (t, "\\begin-subparagraph")    ||
      is_tuple (t, "\\begin-subparagraph*")   ||
      is_tuple (t, "\\begin-subparagraph**")  ||
      is_tuple (t, "\\begin-subsubparagraph") ||
      is_tuple (t, "\\begin-subsubparagraph*")||
      is_tuple (t, "\\begin-subsubparagraph**")) {
    string s= as_string (t[0]);
    s= s(7, N(s));
    t[0]= s;
    return l2e(t);
  }

  if (is_tuple (t, "\\end-part")           ||
      is_tuple (t, "\\end-part*")          ||
      is_tuple (t, "\\end-chapter")        ||
      is_tuple (t, "\\end-chapter*")       ||
      is_tuple (t, "\\end-section")        ||
      is_tuple (t, "\\end-section*")       ||
      is_tuple (t, "\\end-subsection")     ||
      is_tuple (t, "\\end-subsection*")    ||
      is_tuple (t, "\\end-subsubsection")  ||
      is_tuple (t, "\\end-subsubsection*") ||
      is_tuple (t, "\\end-paragraph")      ||
      is_tuple (t, "\\end-paragraph*")     ||
      is_tuple (t, "\\end-subparagraph")   ||
      is_tuple (t, "\\end-subparagraph*")  ||
      is_tuple (t, "\\end-subsubparagraph")||
      is_tuple (t, "\\end-subsubparagraph*")) {
    return "";
  }  

  if (is_tuple (t, "\\begin-tmpadded*", 1)     ||
      is_tuple (t, "\\begin-tmunderlined*", 1) ||
      is_tuple (t, "\\begin-tmoverlined*", 1)  ||
      is_tuple (t, "\\begin-tmbothlined*", 1)  ||
      is_tuple (t, "\\begin-tmornamented*", 1) ||
      is_tuple (t, "\\begin-tmframed*", 1)) {
        string env= as_string (t[0]);
        env= env (7, N(env));
        hashmap<tree,tree> dic ("");
        dic("skipabove")=            "padding-above";
        dic("skipbelow")=            "padding-below";
        if (env == "tmpadded*") {
          dic("innertopmargin")=     "framed-vsep";
          dic("innerbottommargin")=  "framed-vsep";
        }
        else if (env == "tmunderlined*" || env == "tmbothlined*" ||
                 env == "tmoverlined*") {
          dic("innerbottommargin")=  "underlined-sep";
          dic("innertopmargin")=     "overlined-sep";
        }
        else if (env == "tmframed*") {
          dic("innertopmargin")=     "framed-vsep";
          dic("innerbottommargin")=  "framed-vsep";
          dic("innerleftmargin")=    "framed-hsep";
          dic("innerrightmargin")=   "framed-hsep";
        }
        else if (env == "tmornamented*") {
          dic("innertopmargin")=     "ornament-vpadding";
          dic("innerbottommargin")=  "ornament-vpadding";
          dic("innerleftmargin")=    "ornament-hpadding";
          dic("innerrightmargin")=   "ornament-hpadding";
          dic("backgroundcolor")=    "ornament-color";
          dic("roundcorner")=        tuple ("ornament-shape", "rounded");
        }
        tree keys= translate_keys (decode_keys_vals (t[1]), dic);
        return tree (BEGIN, env, keys);
  }

  if (is_tuple (t, "\\env-init", 2)) return "";

  if (is_tuple (t, "\\geometry", 1)) {
    array< array<tree> > l= tokenize_keys_vals (t[1]);
    int i, n= N(l);
    tree r (COLLECTION);
    for (i=0; i<n; i++) {
      if (N(l[i]) < 1 || N(l[i]) > 2) continue;
      string  key= trim_spaces (as_string (l2e (l[i][0])));
      string type= paper_type (key);
      string opts= paper_opts (key);
      if (N(l[i]) == 1) {
        if (key == "landscape" || key == "portrait")
          r << tree (ASSOCIATE, "page-orientation", key);
        else if (type != "undefined")
          r << tree (ASSOCIATE, "paper-type", type);
      }
      else if (N(l[i]) == 2) {
        tree val= trim_spaces (l2e (l[i][1]));
        string sval= trim_spaces (as_string (val));
        if ((key == "landscape" || key == "portrait") && sval == "true")
          r << tree (ASSOCIATE, "page-orientation", key);
        else if (type != "undefined" && sval == "true")
          r << tree (ASSOCIATE, "paper-type", type);
        else if (opts != "undefined") {
          r << tree (ASSOCIATE, opts, val);
          if (opts == "page-height" || opts == "page-width")
            r << tree (ASSOCIATE, "page-type", "user");
        }
      }
    }
    return tree (APPLY, "geometry", r);
  }

  if (is_tuple (t, "\\marginpar", 1))
    return tree (APPLY, "marginal-note", "normal", "", l2e (t[1]));

  if (is_tuple (t, "\\marginpar*", 2)) {
    tree l= l2e (t[1]), r= l2e (t[2]);
    if (l == "" && r != "")
      return tree (APPLY, "marginal-right-note", "", r);
    if (r == "" && l != "")
      return tree (APPLY, "marginal-left-note",  "", l);
    return (concat (
          tree (APPLY, "marginal-right-note", "", r),
          tree (APPLY, "marginal-left-note",  "", l)));
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

  if (is_tuple (t, "\\colorbox", 2))
    return compound ("colored-frame", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\color", 1)) return tree (SET, COLOR, string_arg (t[1]));
  if (is_tuple (t, "\\color*", 2)) {
    string cm= string_arg (t[1]), val= string_arg (t[2]);
    if (cm == "HTML") val= "#"*val;
    else
      val= cm * ":" * val;
    return tree (SET, COLOR, val);
  }
  if (is_tuple (t, "\\textcolor*", 3)) {
    string cm= string_arg (t[1]), val= string_arg (t[2]);
    if (cm == "HTML") val= "#"*val;
    else
      val= cm * ":" * val;
    return concat (tree (SET, COLOR, val), l2e (t[3]), tree (RESET, COLOR));
  }

  if (is_tuple (t, "\\textcolor", 2) || is_tuple (t, "\\tmcolor", 2))
    return concat (tree (SET, COLOR, l2e (t[1])), l2e (t[2]),
        tree (RESET, COLOR));

  if (is_tuple (t, "\\key", 1)) return compound ("key", latex_key_arg (t[1]));
  if (is_tuple (t, "\\textnormalfont", 1)) return m2e (m2e (m2e (t,
          FONT_FAMILY, "rm"), FONT_SERIES, "medium"), FONT_SHAPE, "right");
  if (is_tuple (t, "\\textrm", 1)) return mod_m2e (t, FONT_FAMILY, "rm");
  if (is_tuple (t, "\\texttt", 1)) return mod_m2e (t, FONT_FAMILY, "tt");
  if (is_tuple (t, "\\textsf", 1)) return mod_m2e (t, FONT_FAMILY, "ss");
  if (is_tuple (t, "\\textmd", 1)) return mod_m2e (t, FONT_SERIES, "medium");
  if (is_tuple (t, "\\textbf", 1)) return mod_m2e (t, FONT_SERIES, "bold");
  if (is_tuple (t, "\\textup", 1)) return mod_m2e (t, FONT_SHAPE, "right");
  if (is_tuple (t, "\\textit", 1)) return mod_m2e (t, FONT_SHAPE, "italic");
  if (is_tuple (t, "\\textsl", 1)) return mod_m2e (t, FONT_SHAPE, "slanted");
  if (is_tuple (t, "\\textsc", 1)) return mod_m2e (t, FONT_SHAPE,"small-caps");
  if (is_tuple (t, "\\bsc", 1)) return m2e (t, FONT_SHAPE, "small-caps");
  if (is_tuple (t, "\\tmrsub", 1)) return tree (RSUB, l2e (t[1]));
  if (is_tuple (t, "\\tmrsup", 1)) return tree (RSUP, l2e (t[1]));
  if (is_tuple (t, "\\textsubscript", 1)) return tree (RSUB, l2e (t[1]));
  if (is_tuple (t, "\\textsuperscript", 1)) return tree (RSUP, l2e (t[1]));
  if (is_tuple (t, "\\lowercase", 1) || is_tuple (t, "\\MakeLowercase", 1))
    return tree (CHANGE_CASE, l2e (t[1]), "locase");
  if (is_tuple (t, "\\uppercase", 1) || is_tuple (t, "\\MakeUppercase", 1))
    return tree (CHANGE_CASE, l2e (t[1]), "UPCASE");
  if (is_tuple (t, "\\ExtractFirstChar", 1))
    return tree (CHANGE_CASE, l2e (t[1]), "first");
  if (is_tuple (t, "\\selectlanguage", 1)) {
    string lang= string_arg (t[1]);
    return tree (SET, "language", latex_to_texmacs_languages (lang));
  }
  if (is_tuple (t, "\\foreignlanguage", 2)) {
    string lang= string_arg (t[1]);
    return tree (CONCAT,
                  tree (SET, "language", latex_to_texmacs_languages (lang)),
                  l2e (t[2]),
                  tree (RESET, "language"));
  }
  if (is_tuple (t, "\\texorpdfstring", 2)) return l2e (t[1]);
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

  if (is_tuple (t, "\\centering", 1))
    return tree (APPLY, "center", l2e (t[1]));
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
  if (is_tuple (t, "\\bar", 1) || is_tuple (t, "\\Bar", 1))
    return tree (WIDE, l2e (t[1]), "<bar>");
  if (is_tuple (t, "\\overline", 1))
    return tree (WIDE, l2e (t[1]), "<bar>");
  if (is_tuple (t, "\\underline", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<bar>");
  if (is_tuple (t, "\\overrightarrow", 1))
    return tree (WIDE, l2e (t[1]), "<wide-varrightarrow>");
  if (is_tuple (t, "\\underrightarrow", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<wide-varrightarrow>");
  if (is_tuple (t, "\\overleftarrow", 1))
    return tree (WIDE, l2e (t[1]), "<wide-varleftarrow>");
  if (is_tuple (t, "\\underleftarrow", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<wide-varleftarrow>");
  if (is_tuple (t, "\\overleftrightarrow", 1))
    return tree (WIDE, l2e (t[1]), "<wide-varleftrightarrow>");
  if (is_tuple (t, "\\underleftrightarrow", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<wide-varleftrightarrow>");
  if (is_tuple (t, "\\Overrightarrow", 1))
    return tree (WIDE, l2e (t[1]), "<wide-Rightarrow>");
  if (is_tuple (t, "\\Underrightarrow", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<wide-Rightarrow>");
  if (is_tuple (t, "\\Overleftarrow", 1))
    return tree (WIDE, l2e (t[1]), "<wide-Leftarrow>");
  if (is_tuple (t, "\\Underleftarrow", 1))
    return tree (VAR_WIDE, l2e (t[1]), "<wide-Leftarrow>");
  if (is_tuple (t, "\\hat", 1) || is_tuple (t, "\\Hat", 1))
    return tree (WIDE, l2e (t[1]), "^");
  if (is_tuple (t, "\\tilde", 1) || is_tuple (t, "\\Tilde", 1))
    return tree (WIDE, l2e (t[1]), "~");
  if (is_tuple (t, "\\widehat", 1)) return tree (WIDE, l2e (t[1]), "^");
  if (is_tuple (t, "\\widetilde", 1)) return tree (WIDE, l2e (t[1]), "~");
  if (is_tuple (t, "\\dot", 1) || is_tuple (t, "\\Dot", 1))
    return tree (WIDE, l2e (t[1]), "<dot>");
  if (is_tuple (t, "\\ddot", 1) || is_tuple (t, "\\Ddot", 1))
    return tree (WIDE, l2e (t[1]), "<ddot>");
  if (is_tuple (t, "\\dddot", 1)) return tree (WIDE, l2e (t[1]), "<dddot>");
  if (is_tuple (t, "\\ddddot", 1)) return tree (WIDE, l2e (t[1]), "<ddddot>");
  if (is_tuple (t, "\\check", 1) || is_tuple (t, "\\Check", 1))
    return tree (WIDE, l2e (t[1]), "<check>");
  if (is_tuple (t, "\\grave", 1) || is_tuple (t, "\\Grave", 1))
    return tree (WIDE, l2e (t[1]), "<grave>");
  if (is_tuple (t, "\\acute", 1) || is_tuple (t, "\\Acute", 1))
    return tree (WIDE, l2e (t[1]), "<acute>");
  if (is_tuple (t, "\\vec", 1) || is_tuple (t, "\\Vec", 1))
    return tree (WIDE, l2e (t[1]), "<vect>");
  if (is_tuple (t, "\\breve", 1) || is_tuple (t, "\\Breve", 1))
    return tree (WIDE, l2e (t[1]), "<breve>");
  if (is_tuple (t, "\\textroundcap", 1))
    return tree (WIDE, l2e (t[1]), "<invbreve>");
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
  if (is_tuple (t, "\\raisebox", 2))
    return tree (MOVE, l2e (t[2]), "0pt", t2e (t[1]));
  if (is_tuple (t, "\\tmcodeinline", 1) || is_tuple (t, "\\tmverbatim", 1))
    return compound ("verbatim", v2e (t[1]));
  if (is_tuple (t, "\\tmcodeinline*", 2))
    return compound (string_arg (t[1]), v2e (t[2]));
  if (is_tuple (t, "\\label", 1)) return tree (LABEL, t2e (t[1]));
  if (is_tuple (t, "\\ref", 1)) return tree (REFERENCE, t2e (t[1]));
  if (is_tuple (t, "\\cref", 1) || is_tuple (t, "\\Cref", 1))
    return latex_cref_to_tree (v2e (t[1]));
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
  if (is_tuple (t, "\\custombinding", 1))
    return tree (SET_BINDING, v2e (t[1]));
  if (is_tuple (t, "\\tmlinenumber", 2))
    return compound ("render-line-number", l2e (t[1]), l2e (t[2]));
  if (is_tuple (t, "\\setlength", 2)) {
    if (!textm_class_flag) return "";
    else {
      tree len= l2e (t[1]);
      tree val= l2e (t[2]);
      return tree (ASSIGN, len, tree (MACRO, val));
    }
  }

  if (is_tuple (t, "\\@startsection") && N(t) >= 7) {
    tree name, indent, spa, spb, style, r, indentafter;
    name = l2e(t[1]);
    indent = l2e(t[3]);
    spb = l2e(t[4]);
    spa = l2e(t[5]);
    style = concat();
    r = concat();
    bool inserted = false, center = false;

    if (is_compound (t[6]))
      for (int i = 0 ; i < N(t[6]) ; i++) {
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
          r << spb << compound ("center", tree (ARG, "name"))
            << spa << indentafter;
        else
          r << spb << tree(HSPACE, indent) << tree (ARG, "name")
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
    string s;
    if (is_atomic (t[1])) s= t[1]->label;
    else s= t[1][0]->label;
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
      is_tuple (t, "\\citealt*", 1) || is_tuple (t, "\\citealp*", 1) ||
      is_tuple (t, "\\citeN", 1))
    {
      textm_natbib= true;
      string star= "";
      string cite_type= t[0]->label (1, N(t[0]->label));
      if (cite_type == "citeN") cite_type= "citealt";
      if (ends (cite_type, "*")) {
        star= "*"; cite_type= cite_type (0, N (cite_type) - 1); }
      if (cite_type == "citet") cite_type= "cite-textual" * star;
      if (cite_type == "citep") cite_type= "cite-parenthesized" * star;
      if (cite_type == "citealt") cite_type= "cite-raw" * star;
      if (cite_type == "citealp") cite_type= "cite-raw" * star;
      string s= v2e (t[1]);
      return latex_cite_to_tree (cite_type, s);
    }
  if (is_tuple (t, "\\citet*", 2) || is_tuple (t, "\\citep*", 2) ||
      is_tuple (t, "\\citet**", 2) || is_tuple (t, "\\citep**", 2) ||
      is_tuple (t, "\\citealp*", 2))
    {
      textm_natbib= true;
      string star= "";
      string cite_type= t[0]->label (1, N(t[0]->label)-1);
      if (ends (cite_type, "*")) {
        star= "*"; cite_type= cite_type (0, N (cite_type) - 1); }
      if (cite_type == "citet") cite_type= "cite-textual" * star;
      if (cite_type == "citep") cite_type= "cite-parenthesized" * star;
      if (cite_type == "citealt") cite_type= "cite-raw" * star;
      if (cite_type == "citealp") cite_type= "cite-raw" * star;
      string s= v2e (t[2]);
      return latex_cite_to_tree (cite_type, s);
    }
  if (is_tuple (t, "\\citetext", 1))
    return compound ("render-cite", l2e (t[1]));
  if (is_tuple (t, "\\onlinecite", 1))
    return compound ("cite-arg", l2e (t[1]));
  if (is_tuple (t, "\\citeauthor", 1)) {
    textm_natbib= true;
    return compound ("cite-author-link", t2e (t[1])); }
  if (is_tuple (t, "\\citeauthor*", 1)) {
    textm_natbib= true;
    return compound ("cite-author*-link", t2e (t[1])); }
  if (is_tuple (t, "\\citeyear", 1)) {
    textm_natbib= true;
    return compound ("cite-year-link", t2e (t[1])); }
  if (is_tuple (t, "\\citeauthoryear", 3)) {
    textm_natbib= true;
    return compound ("natbib-triple", l2e (t[1]), l2e (t[2]), t2e (t[3])); }
  if (is_tuple (t, "\\bibitem", 1))
    return compound ("bibitem", v2e (t[1]));
  if (is_tuple (t, "\\bibitem*", 2)) {
    tree key= t2e (t[1], false);
    if (is_func (key, CONCAT))
      for (int i=0; i<N(key); i++)
        if (is_compound (key[i], "natbib-triple")) {
          key= key[i]; break; }
    if (is_compound (key, "natbib-triple", 3)) {
      while (is_atomic (key[1]) && ends (key[1]->label, " "))
        key[1]= key[1]->label (0, N(key[1]->label) - 1);
      if (is_atomic (key[1]) && ends (key[1]->label, " et"))
        key[1]= key[1]->label * " al.";
    }
    else key= string_arg (key);
    return compound ("bibitem-with-key", key, v2e (t[2]));
  }
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
  if (is_tuple (t, "\\hline")) return tree (APPLY, "hline");
  if (is_tuple (t, "\\hdashline")) return "";
  if (is_tuple (t, "\\hdashline*")) return "";
  if (is_tuple (t, "\\noalign", 1))
    return ""; // FIXME: for larger space in maple matrices
  if (is_tuple (t, "\\etalchar", 1)) return t2e (t[1]);
  if (is_tuple (t, "\\MR", 1)) return tree (CONCAT, "MR ", l2e (t[1]));
  if (is_tuple (t, "\\MRhref", 2))
    return compound ("hlink", l2e (t[2]),
                     "http://www.ams.org/mathscinet-getitem?mr=" *
                     as_string (t2e (t[1])));
  if (is_tuple (t, "\\natexlab", 1)) return t2e (t[1]);
  if (is_tuple (t, "\\penalty", 1)) return "";
  if (is_tuple (t, "\\url", 1))
    return tree (APPLY, "slink", url_arg_to_string (t[1]));
  if (is_tuple (t, "\\path", 1))
    return tree (APPLY, "verbatim", url_arg_to_string (t[1]));
  if (is_tuple (t, "\\href", 2))
    return tree (APPLY, "hlink", l2e (t[2]), url_arg_to_string (t[1]));
  if (is_tuple (t, "\\og", 1) && t[1] == "")
    return "\x13"; // open guillemets (French)
  if (is_tuple (t, "\\fg", 1) && t[1] == "")
    return "\x14"; // close guillemets (French)

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
  if (is_func (t, SET) || is_func (t, RESET)) return t;
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
  if (is_tuple (t, "\\tmconverterinput", 4))
    return compound ("converter-input",
        l2e (t[1]), l2e (t[3]), l2e (t[4]));
  if (is_tuple (t, "\\tmconverteroutput", 4))
    return compound ("converter-output",
        l2e (t[1]), l2e (t[3]), l2e (t[4]));
  if (is_tuple (t, "\\tmscriptinput", 4))
    return compound ("script-input",
        l2e (t[1]), "default", l2e (t[3]), l2e (t[4]));
  if (is_tuple (t, "\\tmscriptoutput", 4))
    return compound ("script-output",
        l2e (t[1]), "default", l2e (t[3]), l2e (t[4]));
  if (is_tuple (t, "\\tmsession", 3))
    return compound ("session", l2e (t[1]), l2e (t[2]), l2e (t[3]));
  if (is_tuple (t, "\\tminputmath", 2)) {
    string old= command_type ["!mode"];
    command_type ("!mode") = "math";
    tree arg= l2e (t[2]);
    command_type ("!mode") = old;
    return document (compound ("input-math", l2e (t[1]), old));
  }
  if (is_tuple (t, "\\tminput", 2))
    return document (compound ("input", l2e (t[1]), l2e (t[2])));
  if (is_tuple (t, "\\tmtiming", 1))
    return document (compound ("timing", l2e (t[1])));
  if (is_tuple (t, "\\tmerrput", 1))
    return document (compound ("errput", l2e (t[1])));
  if (is_tuple (t, "\\tmoutput", 1))
    return document (compound ("output", l2e (t[1])));
  if (is_tuple (t) && N(t) == 3 &&
      (starts (as_string (t[0]), "\\tmfolded")    ||
       starts (as_string (t[0]), "\\tmunfolded")  ||
       starts (as_string (t[0]), "\\tmdetailed")  ||
       starts (as_string (t[0]), "\\tmsummarized"))) {
    string tag= as_string (t[0]);
    tag= tag (3, N(tag));
    if (starts (tag, "folded") && tag != "folded" && N(tag) > 6)
      tag= "folded-"* tag (6, N(tag));
    else if (starts (tag, "unfolded")   && tag != "unfolded" && N(tag) > 8)
      tag= "unfolded-"* tag (8, N(tag));
    else if (starts (tag, "detailed")   && tag != "detailed" && N(tag) > 8)
      tag= "detailed-"* tag (8, N(tag));
    else if (starts (tag, "summarized") && tag != "summarized" && N(tag) > 10)
      tag= "summarized-"* tag (10, N(tag));
    return compound (tag, l2e (t[1]), l2e (t[2]));
  }
  if (is_tuple (t) && N(t) == 4 &&
      (as_string (t[0]) == "\\tmfoldedio"       ||
       as_string (t[0]) == "\\tmfoldediomath"   ||
       as_string (t[0]) == "\\tmunfoldediomath" ||
       as_string (t[0]) == "\\tmunfoldedio")) {
    string tag= as_string (t[0]);
    tag= tag (3, N(tag));
    if (tag == "foldedio")       tag= "folded-io";
    if (tag == "unfoldedio")     tag= "unfolded-io";
    if (tag == "foldediomath")   tag= "folded-io-math";
    if (tag == "unfoldediomath") tag= "unfolded-io-math";
    string old= command_type ["!mode"];
    if (ends (tag, "math")) command_type ("!mode") = "math";
    tree arg= l2e (t[2]);
    command_type ("!mode") = old;
    return document (compound (tag, l2e (t[1]), arg, l2e (t[3])));
  }
  // End TeXmacs specific markup

  if (L(t) == IMAGE) return t;
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
