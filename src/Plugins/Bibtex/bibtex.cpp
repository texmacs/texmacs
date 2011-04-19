
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

#ifdef OS_WIN32
#include <sys/misc.h>
#endif

static string bibtex_command= "bibtex";

void
set_bibtex_command (string cmd) {
  bibtex_command= cmd;
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

tree
bibtex_run (string bib, string style, url bib_file, tree bib_t) {
  int i;
  string bib_s= "\\bibstyle{" * style * "}\n";
  for (i=0; i<arity(bib_t); i++)
    bib_s << "\\citation{" << as_string (bib_t[i]) << "}\n";

  string dir= concretize (head (bib_file));
  string bib_name= as_string (tail (bib_file));
  if ((N(bib_name) >= 4) && (bib_name (N(bib_name)-4, N(bib_name)) == ".bib"))
    bib_name= bib_name (0, N(bib_name)- 4);
  bib_s << "\\bibdata{" << bib_name << "}\n";
  save_string ("$TEXMACS_HOME_PATH/system/bib/temp.aux", bib_s);

#ifdef OS_WIN32
  char *directory = as_charp (dir);
  RunBibtex(directory, "$TEXMACS_HOME_PATH/system/bib", "temp");
  tm_delete_array (directory);
#else
  string cmdln= "cd $TEXMACS_HOME_PATH/system/bib; ";
  cmdln << "BIBINPUTS=" << dir << ":$BIBINPUTS "
	<< "BSTINPUTS=" << dir << ":$BSTINPUTS "
	<< bibtex_command << " temp";
  if (DEBUG_AUTO) cout << "TeXmacs] BibTeX command: " << cmdln << "\n";
  system (cmdln);
#endif

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
}
