
/******************************************************************************
* MODULE     : latex_preview.cpp
* DESCRIPTION: generating pictures using LaTeX with preview package
* COPYRIGHT  : (C) 2013  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "LaTeX_Preview/latex_preview.hpp"
#include "Ghostscript/gs_utilities.hpp"
#include "Tex/convert_tex.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "sys_utils.hpp"

static inline void
dbg (string s) {
  if (DEBUG_AUTO) cout << "TeXmacs] " << s << LF;
}

static string latex_command= "pdflatex";

void
set_latex_command (string cmd) {
  latex_command= cmd;
}

bool
latex_present () {
  return exists_in_path (latex_command);
}

array<string>
search_latex_previews (tree t) {
  array<string> r;
  if (is_atomic (t));
  else if (is_tuple (t, "\\latex_preview", 2))
    r << as_string (t[1]);
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      r << search_latex_previews (t[i]);
  }
  return r;
}

void
latex_clean_tmp_directory (url u) {
  bool flag= false;
  array<string> content= read_directory (u, flag);
  for (int i=0; i<N(content); i++)
    if (content[i] != "." && content[i] != "..")
      remove (u * content[i]);
  remove (u * ".");
}

void
latex_install_preview (string s, tree t, url wdir, bool dvips) {
  array<string> macros= search_latex_previews (t);
  string preview= "%%%%%%%%%%%%%% ADDED BY TEXMACS %%%%%%%%%%%%%%%%%%\n";
  if (dvips)
    preview  << "\\usepackage[active,tightpage,delayed]{preview}\n";
  else
    preview  << "\\usepackage[active,tightpage,delayed,psfixbb,dvips]{preview}\n";

  for (int i=0; i<N(macros); i++) {
    int arity= latex_arity (macros[i]);
    bool option= (arity<0);
    string arity_code;
    if (option) {
      arity_code << "[]";
      arity= -arity;
    }
    while (arity-- > 0) arity_code << "{}";
    string name= "\\" * macros[i];
    string cmd= "\\PreviewMacro";
    if (test (name, 0, "\\begin-")) {
      name= name(7, N(name));
      cmd= "\\PreviewEnvironment";
    }
    preview << cmd << "[{" * arity_code * "}]{" * name * "}\n";
  }
  preview << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n";

  int i= 0;
  i= latex_search_forwards ("\n\\documentclass", i, s);
  i= latex_search_forwards ("{", i, s);
  i= latex_search_forwards ("}", i, s);
  i= latex_search_forwards ("\n", i, s);
  i++;
  s= s(0, i) * preview * s(i, N(s));
  save_string (wdir * "temp.tex", s);
}

tree
latex_load_image (url image) {
  string s;
  tree t (IMAGE, 5);
  load_string (image, s, false);
  if (s == "") {
    dbg ("Could not load " * as_string (image));
    return array<tree> ();
  }
  int width, height;
  gs_image_size (image, width, height);
  t[0]= tuple (tree (RAW_DATA, s), "eps");
  t[1]= as_string (width) * "pt";
  t[2]= as_string (height) * "pt";
  return (t);
}

array<tree>
latex_load_preview (url wdir, bool dvips= false) {
  string cmdln= "cd " * as_string (wdir) * "; ";
  if (dvips) {
    cmdln << "dvips temp.dvi && "
      << "gs -sDEVICE=epswrite -dSAFER -q -dNOPAUSE -dBATCH "
      << "-dLanguageLevel=3 -sOutputFile=temp%d.eps temp.ps";
  }
  else {
    cmdln << "gs -sDEVICE=epswrite -dSAFER -q -dNOPAUSE -dBATCH "
      << "-dLanguageLevel=3 -sOutputFile=temp%d.eps temp.pdf";
  }
  dbg ("GS command: " * cmdln);
  if (system (cmdln)) {
    dbg ("Could not extract pictures from LaTeX document");
    return array<tree> ();
  }
  unsigned int cnt= 1;
  bool stop= false;
  array<tree> r;
  while (!stop) {
    url u= wdir * ("temp" * as_string (cnt) * ".eps");
    if (exists (u)) {
      // gs can produce empty pictures, to be ignored
      tree tmp= latex_load_image (u);
      if (N(tmp) == 5 && (tmp[1] != "0pt" || tmp[2] != "0pt"))
        r << tmp;
      cnt++;
    }
    else
      stop= true;
  }
  return r;
}

array<tree>
latex_preview (string s, tree t) {
  if (!latex_present () && !exists_in_path ("latex")) {
    dbg ("LaTeX preview: " * latex_command * " not found");
    return array<tree>();
  }
  if (!exists_in_path ("gs")) {
    dbg ("LaTeX preview: ghostscript not found");
    return array<tree>();
  }
  // FIXME: ./Texmacs/Window/tm_frame.cpp:191 seems to crash here if we launch
  // system_wait ("LaTeX: compiling document, ", "please wait");
  url wdir= url_temp ("_latex_preview");
  mkdir (wdir);
  bool dvips= false;
  latex_install_preview (s, t, wdir, dvips);
  string document_root= as_string (head (get_file_focus ()));
  string cmdln= "cd " * document_root;
  cmdln << "; " << latex_command
        << " -interaction nonstopmode -halt-on-error -file-line-error "
        << " -output-directory " << as_string (wdir)
        << " " << as_string (wdir) << "/temp.tex";
  dbg ("LaTeX command: " * cmdln);
  if (system (cmdln)) {
    dbg ("Could not compile LaTeX document using " * latex_command);
    dbg ("Try to fallback on LaTeX");
    cmdln= "cd " * document_root;
    cmdln << "; latex"
          << " -interaction nonstopmode -halt-on-error -file-line-error "
          << " -output-directory " << as_string (wdir)
          << " " << as_string (wdir) << "/temp.tex";
    dbg ("LaTeX command: " * cmdln);
    dvips= true;
    latex_install_preview (s, t, wdir, dvips);
    if (system (cmdln)) {
      dbg ("Could not compile LaTeX document");
      latex_clean_tmp_directory (wdir);
      return array<tree> ();
    }
  }
  array<tree> r= latex_load_preview (wdir, dvips);
  if (N(r) != N(search_latex_previews (t))) {
    dbg ("Warning: did not found the expected number of pictures.");
    dbg ("         LaTeX compilation or picture importation might have failed");
  }
  latex_clean_tmp_directory (wdir);
  return r;
} 
