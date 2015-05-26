
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
string latex_unmark (string, hashset<path>, hashmap<int,array<path> >&);

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
      string ss= s (start, i);
      ss= replace (ss, "\15\12", "\n");
      ss= replace (ss, "\12\15", "\n");
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
* Parsing the error message
******************************************************************************/

static string
first_line (string s) {
  int pos= search_forwards ("\n", s);
  if (pos < 0) return s;
  else return s (0, pos);
}

static string
other_lines (string s) {
  int pos= search_forwards ("\n", s);
  if (pos < 0) return "";
  else return s (pos+1, N(s));
}

string
latex_error_head (string s) {
  int pos= search_forwards ("\nl.", s);
  if (pos < 0) return s;
  string ss= s (0, pos);
  while (ends (ss, "\n")) ss= ss (0, N(ss) - 1);
  return ss;
}

string
latex_error_tail (string s) {
  int pos= search_forwards ("\nl.", s);
  if (pos < 0) return "";
  return s (pos+1, N(s));
}

string
latex_error_message (string s) {
  return first_line (s);
}

void
strip (string& s, string what) {
  if (ends (s, what)) s= s (0, N(s) - N(what));
  while (ends (s, "\n") || ends (s, " ")) s= s (0, N(s) - 1);
}

string
latex_error_explain (string s) {
  string ss= other_lines (latex_error_head (s));
  while (starts (ss, "\n")) ss= ss (1, N(ss));
  while (ends (ss, "\n") || ends (ss, " ")) ss= ss (0, N(ss) - 1);
  strip (ss, "...");
  strip (ss, "Type  H <return>  for immediate help.");
  return ss;
}

string
latex_error_position (string s) {
  string t= latex_error_tail (s);
  string l1= first_line (t);
  string l2= first_line (other_lines (t));
  return l1 * "\n" * l2;
}

string
latex_error_line (string s) {
  string t= latex_error_tail (s);
  if (!starts (t, "l.")) return "?";
  int pos= search_forwards (" ", t);
  if (pos < 0) return "?";
  return t (2, pos);
}

string
latex_error_extra (string s) {
  string t= latex_error_tail (s);
  string x= other_lines (other_lines (t));
  while (starts (x, "\n")) x= x (1, N(x));
  while (ends (x, "\n") || ends (x, " ")) x= x (0, N(x) - 1);
  strip (x, "or  <return>  to continue without it.");
  strip (x, "Type  I <command> <return>  to replace it with another command,");
  strip (x, "If that doesn't work, type  X <return>  to quit.");
  strip (x, "Try typing  <return>  to proceed.");
  return x;
}

/******************************************************************************
* Finding the error in the LaTeX file
******************************************************************************/

static int
find_line (string s, int l) {
  int pos= 0, n= N(s);
  l--;
  while (pos<n && l>0) {
    if (s[pos] == '\n') l--;
    pos++;
  }
  return pos;
}

int
latex_error_find (string s, string src) {
  string ls= latex_error_position (s);
  string l1= first_line (ls);
  string l2= other_lines (ls);
  if (!starts (l1, "l.")) return -1;
  if (N(l2) < N(l1)) return -1;
  for (int i=0; i<N(l1); i++)
    if (l2[i] != ' ') return -1;
  s= l1 * l2 (N(l1), N(l2));
  int offset= search_forwards (" ", s);
  if (offset < 0) return -1;
  string lnr= s (2, offset);
  while (offset < N(s) && s[offset] == ' ') offset++;
  s= s (offset, N(s));
  if (starts (s, "...")) { offset += 3; s= s (3, N(s)); }
  if (ends (s, "...")) s= s (0, N(s) - 3);
  if (!is_int (lnr)) return -1;
  int pos= find_line (src, as_int (lnr));
  int bef= search_backwards (s, pos, src);
  int aft= search_forwards (s, pos, src);
  int ind= N(l1) - offset;
  if (bef >= 0) bef += ind;
  if (aft >= 0) aft += ind;
  if (bef < 0 && aft < 0) return -1;
  if (bef < 0) return aft;
  if (aft < 0) return bef;
  if (pos - bef < aft - pos) return bef;
  else return aft;
}

int
get_line_number (string s, int pos) {
  int l=1;
  pos= min (pos, N(s));
  for (int i=0; i<pos; i++)
    if (s[i] == '\n') l++;
  return l;
}

int
get_column_number (string s, int pos) {
  int c=0;
  pos= min (pos, N(s));
  for (int i=0; i<pos; i++)
    if (s[i] == '\n') c=0;
    else c++;
  return c;
}

/******************************************************************************
* Find the error in the TeXmacs source file
******************************************************************************/

path
texmacs_error_find (string s, string src, hashmap<int,array<path> > corr) {
  int pos= latex_error_find (s, src);
  if (pos < 0) return path ();
  while (pos > 0) {
    if (corr->contains (pos)) {
      array<path> a= corr[pos];
      for (int i=0; i<N(a); i++)
        if (!is_nil (a[i]) && last_item (a[i]) == 0)
          return path_up (a[i]);
    }
    pos--;
  }
  return path ();
}

/******************************************************************************
* Export, run LaTeX, and analyze
******************************************************************************/

tree
try_latex_export (tree doc, object opts, url src, url dest) {
  string s, ms;
  doc= latex_expand (doc, src);
  //cout << "Exporting to LaTeX\n";
  if (tracked_tree_to_latex_document (doc, opts, s, ms)) {
    //cout << "Failed to export\n";
    //return "Error: could not track LaTeX export";
    cout << "TeXmacs] Failed to track LaTeX export\n";
    ms= s;
  }
  hashmap<int,array<path> > corr;
  string us= latex_unmark (ms, hashset<path> (), corr);
  if (save_string (dest, us, false)) {
    //cout << "Could not save\n";
    return "Error: could not save LaTeX export";
  }
  //cout << "Running pdflatex\n";
  string cmd=
    "cd " * sys_concretize (head (dest)) *
    "; pdflatex -interaction=batchmode " * as_system_string (tail (dest));
  //cout << cmd << LF;
  system (cmd);
  url log= glue (unglue (dest, N (suffix (dest))), "log");
  tree errs= get_latex_errors (log);
  tree r (TUPLE);
  r << us;
  tree b= extract (doc, "body");
  for (int i=0; i<N(errs); i++) {
    string err= errs[i]->label;
    //cout << "------------ Error " << i+1 << LF;
    //cout << err << LF;
    /*
    int pos= latex_error_find (err, us);
    if (pos < 0) cout << "Position could not be found\n";
    else {
      cout << us (max (pos-50, 0), pos)
           << "[*]"
           << us (pos, min (pos+50, N(us))) << LF;
    }
    */
    /*
    path p= texmacs_error_find (err, us, corr);
    if (is_nil (p) || !has_subtree (b, p))
      cout << "Position could not be found\n";
    else cout << subtree (b, p) << LF;
    */
    int pos= latex_error_find (err, us);
    path p= texmacs_error_find (err, us, corr);
    tree t (TUPLE);
    t << tree (err)
      << tree ("l." * latex_error_line (err) * " " *
               latex_error_message (err))
      << tree (latex_error_message (err))
      << tree (latex_error_explain (err))
      << tree (latex_error_position (err))
      << tree (latex_error_extra (err));
    if (pos >= 0) t << as_tree (pos);
    if (!is_nil (p) && has_subtree (b, p)) t << ((tree) p);
    r << t;
  }
  return r;
}
