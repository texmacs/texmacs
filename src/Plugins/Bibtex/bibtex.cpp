
/******************************************************************************
* MODULE     : bibtex.cpp
* DESCRIPTION: generating bibliographies using BiBTeX
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
bibtex_run (string style, string dir, string fname, tree bib_t) {
  int i;
  string bib_s= "\\bibstyle{" * style * "}\n";
  for (i=0; i<arity(bib_t); i++)
    bib_s << "\\citation{" << as_string (bib_t[i]) << "}\n";

  string bib_name= fname;
  if ((N(bib_name) >= 4) && (bib_name (N(bib_name)-4, N(bib_name)) == ".bib"))
    bib_name= bib_name (0, N(bib_name)- 4);
  bib_s << "\\bibdata{" << bib_name << "}\n";
  save_string ("$TEXMACS_HOME_PATH/system/bib/temp.aux", bib_s);

#ifdef OS_WIN32
  char *directory = as_charp(dir);
  RunBibtex(directory, "$TEXMACS_HOME_PATH/system/bib", "temp");
  delete [] directory;
#else
  string cmdln= "cd $TEXMACS_HOME_PATH/system/bib; ";
  cmdln << "BIBINPUTS=" << dir << ":$BIBINPUTS "
	<< "BSTINPUTS=" << dir << ":$BSTINPUTS "
	<< bibtex_command << " temp";
  if (DEBUG_AUTO) cout << "TeXmacs] BibTeX command: " << cmdln << "\n";
  system (cmdln);
#endif

  string result;
  if (load_string ("$TEXMACS_HOME_PATH/system/bib/temp.bbl", result))
    return "Error: bibtex failed to create bibliography";

  int count=1;
  tree t= generic_to_tree (result, "latex-snippet");
  if (is_document (t) && is_extension (t[0])) t= t[0];
  if (is_document (t) && (N(t)>1) && is_extension (t[1])) t= t[1];
  if (arity(t) == 0) return "";
  if ((!is_compound (t, "thebibliography", 2)) ||
      (!is_document (t[N(t)-1])))
    return "";
  t= t[N(t)-1];
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
	if (is_atomic (item[1])) v << tree (LABEL, "bib-" * item[1]->label);
	if (N(t[i])>1) {
	  v << remove_start_space (t[i][1]);
	  v << A (t[i] (2, N(t[i])));
	}
	u << v;
      }
  }
  return u;
}
