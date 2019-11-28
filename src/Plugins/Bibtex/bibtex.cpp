
/******************************************************************************
* MODULE     : bibtex.cpp
* DESCRIPTION: generating bibliographies using BiBTeX
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Bibtex/bibtex.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "wencoding.hpp"


static string bibtex_command= "bibtex";

void
set_bibtex_command (string cmd) {
  bibtex_command= cmd;
}

bool
bibtex_present () {
  return exists_in_path (bibtex_command);
}

tree
remove_start_space (tree t) {
  if (is_atomic (t)) {
    string s= t->label;
    if (starts (s, " ")) return s (1, N(s));
    else return s;
  }
  else return t;
}

array<tree>
search_defs (tree t) {
  array<tree> r;
  if (is_atomic (t));
  else if (is_compound (t, "assign", 2))
    r << t[0] << t[1];
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      r << search_defs (t[i]);
  }
  return r;
}

tree
search_bib (tree t) {
  if (is_atomic (t)) return "";
  else if (is_compound (t, "thebibliography", 2) && is_document (t[1]))
    return t;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++) {
      tree r= search_bib (t[i]);
      if (r != "") return r;
    }
    return "";
  }
}

string
bibtex_update_encoding (string s) {
  string r;
  array<string> a= tokenize (s, "\\bibitem");
  for (int i=0; i<N(a); i++) {
    array<string> b= tokenize (a[i], "\n");
    for (int j=0; j<N(b); j++)
      b[j]= cork_to_sourcecode (western_to_cork (b[j]));
    if (i != 0) r << "\\bibitem";
    r << recompose (b, "\n");
  }
  return r;
}

tree
bibtex_load_bbl (string bib, url bbl_file) {
  string result;
  if (load_string (bbl_file, result, false))
    return "Error: bibtex failed to create bibliography";

  result= bibtex_update_encoding (result);
  int count=1;
  tree t= generic_to_tree (result, "latex-snippet");
  tree with= tree (WITH);
  with << search_defs (t);
  t= search_bib (t);
  if (t == "") return "";
  tree largest= t[0];
  t= t[1];

  tree u (DOCUMENT);
  for (int i=0; i<arity(t); i++) {
    if (is_concat (t[i]) &&
	(is_compound (t[i][0], "bibitem") ||
	 is_compound (t[i][0], "bibitem*")||
	 is_compound (t[i][0], "bibitem-with-key")))
      {
	tree item= t[i][0];
	if (is_compound (item, "bibitem"))
	  item= compound ("bibitem*", as_string (count++), item[0]);
	t[i][0]= item;
	tree v (CONCAT, compound ("bibitem*", item[0]));
	if (is_atomic (item[1]))
	  v << tree (LABEL, bib * "-" * item[1]->label);
	if (N(t[i])>1) {
	  v << remove_start_space (t[i][1]);
	  v << A (t[i] (2, N(t[i])));
	}
	u << v;
      }
  }

  if (N(u) == 0) u= tree (DOCUMENT, "");
  if (N(with) > 0) {
    with << compound ("bib-list", largest, u);
    return with;
  }
  return compound ("bib-list", largest, u);
}

static bool
contain_space (string s) {
  for (int i=0; i<N(s); i++)
    if (s[i] == ' ')
      return true;
  return false;
}

void
complete_bib_file (url& bib_file, tree bib_t) {
  if (as_string (tail (bib_file)) == "auto.bib") return;
  if (!is_func (bib_t, DOCUMENT)) return;
  for (int i=0; i<N(bib_t); i++)
    if (is_atomic (bib_t[i]) && starts (bib_t[i]->label, "TeXmacs:")) {
      if (suffix (bib_file) != "bib") bib_file= glue (bib_file, ".bib");
      url rad= unglue (bib_file, 4);
      url xbib_file= "$TEXMACS_PATH/misc/bib/texmacs.bib";
      url mbib_file= head (bib_file) * glue (rad, "-extended.bib");
      copy (bib_file, mbib_file);
      append_to (xbib_file, mbib_file);
      bib_file= mbib_file;
      return;
    }
}

tree
bibtex_run (string bib, string style, url bib_file, tree bib_t) {
  complete_bib_file (bib_file, bib_t);
  if (contain_space (style))
    return "Error: bibtex disallows spaces in style name";
  string bib_name= as_string (tail (bib_file));
  if (contain_space (bib_name))
    return "Error: bibtex disallows spaces in bibliography name";
  int i;
  string bib_s= "\\bibstyle{" * style * "}\n";
  for (i=0; i<arity(bib_t); i++)
    bib_s << "\\citation{" << as_string (bib_t[i]) << "}\n";

  string dir= concretize (head (bib_file));
  if ((N(bib_name) >= 4) && (bib_name (N(bib_name)-4, N(bib_name)) == ".bib"))
    bib_name= bib_name (0, N(bib_name)- 4);
  bib_s << "\\bibdata{" << bib_name << "}\n";
  save_string ("$TEXMACS_HOME_PATH/system/bib/temp.aux", bib_s);

#ifdef OS_WIN32_LATER
  c_string directory (dir);
  RunBibtex (directory, "$TEXMACS_HOME_PATH/system/bib", "temp");
#else
  string cmdln= "cd $TEXMACS_HOME_PATH/system/bib; ";
  cmdln << "BIBINPUTS=\"" << dir << "\":$BIBINPUTS "
	<< "BSTINPUTS=\"" << dir << "\":$BSTINPUTS "
	<< bibtex_command
        << " temp > $TEXMACS_HOME_PATH/system/bib/temp.log";
  if (DEBUG_AUTO) {
    if (!(DEBUG_STD))
      debug_shell << cmdln << "\n";
  }
  string log;
  if (system (cmdln, log))
    bibtex_error << log << "\n";
  else {
    int pos=0;
    while (true) {
      pos= search_forwards ("Warning--", pos, log);
      if (pos < 0) break;
      pos += 9;
      int end= pos;
      while (end < N(log) && log[end] != '\n') end++;
      bibtex_warning << log (pos, end) << "\n";
    }
  }
#endif

  return bibtex_load_bbl (bib, "$TEXMACS_HOME_PATH/system/bib/temp.bbl");
  /*
  string result;
  if (load_string ("$TEXMACS_HOME_PATH/system/bib/temp.bbl", result, false))
    return "Error: bibtex failed to create bibliography";

  int count=1;
  tree t= generic_to_tree (result, "latex-snippet");
  t= search_bib (t);
  if (t == "") return "";
  tree largest= t[0];
  t= t[1];
  tree u (DOCUMENT);
  for (i=0; i<arity(t); i++) {
    if (is_concat (t[i]) &&
	(is_compound (t[i][0], "bibitem") ||
	 is_compound (t[i][0], "bibitem*")))
      {
	tree item= t[i][0];
	if (is_compound (item, "bibitem"))
	  item= compound ("bibitem*", as_string (count++), item[0]);
	t[i][0]= item;
	tree v (CONCAT, compound ("bibitem*", item[0]));
	if (is_atomic (item[1]))
	  v << tree (LABEL, bib * "-" * item[1]->label);
	if (N(t[i])>1) {
	  v << remove_start_space (t[i][1]);
	  v << A (t[i] (2, N(t[i])));
	}
	u << v;
      }
  }
  if (N(u) == 0) u= tree (DOCUMENT, "");
  return compound ("bib-list", largest, u);
  */
}

tree
bibtex_run (string bib, string style, url bib_file, array<string> names) {
  tree t (DOCUMENT);
  int i, n= N(names);
  for (i=0; i<n; i++) t << names[i];
  return bibtex_run (bib, style, bib_file, t);
}
