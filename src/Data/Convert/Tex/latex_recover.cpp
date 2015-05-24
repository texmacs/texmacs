
/******************************************************************************
* MODULE     : latex_recover.cpp
* DESCRIPTION: Error recovery for TeXmacs -> LaTeX exportation
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "analyze.hpp"
#include "iterator.hpp"
#include "file.hpp"
#include "scheme.hpp"

tree latex_expand (tree doc, url name);
bool tracked_tree_to_latex_document (tree, object, string&, string&);

/******************************************************************************
* Getting information out of log files
******************************************************************************/

int
number_latex_errors (url log) {
  string s;
  if (load_string (log, s, false)) return -1;
  //cout << "Log file" << LF << HRULE << s << HRULE;
  return count_occurrences ("\12! ", s) + count_occurrences ("\15! ", s);
}

tree
get_latex_errors (url log) {
  string s;
  tree t (TUPLE);
  if (load_string (log, s, false)) return t;
  int i=0, n=N(s);
  while (i<n) {
    while (i<n) {
      if (i+1 < n && s[i] == '!' && s[i+1] == ' ')
        if (i == 0 || s[i-1] == '\12' || s[i-1] == '\15')
          break;
      i++;
    }
    if (i<n) {
      int start=i;
      while (i<n) {
        while (i<n) {
          if (i>0 && s[i-1] == '\n' && s[i] == '\n') break;
          if (i>0 && s[i-1] == '\n' && s[i] == '\15' && s[i] == '\n') break;
          i++;
        }
        string ss= s (start, i);
        if (occurs ("\12l.", ss) || occurs ("\15l.", ss)) break;
        i++;
      }
      t << s (start, i);
    }
  }
  return t;
}

int
number_latex_pages (url log) {
  string s;
  if (load_string (log, s, false)) return -1;
  int pos= search_backwards ("Output written on ", s);
  if (pos < 0) return -1;
  pos= search_forwards (" pages, ", pos, s);
  if (pos < 0) return -1;
  int end= pos;
  while (pos > 0 && is_numeric (s[pos-1])) pos--;
  return as_int (s (pos, end));
}

/******************************************************************************
* Export, run LaTeX, and analyze
******************************************************************************/

void
try_latex_export (tree doc, object opts, url src, url dest) {
  string s, ms;
  cout << "TeXmacs] Performing conversion\n";
  doc= latex_expand (doc, src);
  if (tracked_tree_to_latex_document (doc, opts, s, ms))
    cout << "TeXmacs] could not track LaTeX export\n";
  else if (save_string (dest, s, false))
    cout << "TeXmacs] could not save LaTeX export\n";
  else {
    cout << "TeXmacs] Running pdflatex\n";
    system ("pdflatex -interaction=batchmode", dest);
    url log= glue (unglue (dest, N (suffix (dest))), "log");
    int nr= number_latex_errors (log);
    if (nr == 0) cout << "TeXmacs] succesfull LaTeX export\n";
    else cout << "TeXmacs] LaTeX export contains " << nr << " error(s)\n";
    tree errs= get_latex_errors (log);
    for (int i=0; i<N(errs); i++) {
      cout << "------------ Error " << i+1 << LF;
      cout << errs[i] << LF;
    }
  }
}
