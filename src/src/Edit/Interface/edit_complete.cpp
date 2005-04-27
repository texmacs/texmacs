
/******************************************************************************
* MODULE     : edit_complete.cpp
* DESCRIPTION: Tab completion
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_interface.hpp"
#include "merge_sort.hpp"
#include "hashset.hpp"
#include "analyze.hpp"

/******************************************************************************
* Closing completions
******************************************************************************/

static array<string>
as_completions (hashset<string> h) {
  tree t= (tree) h;
  int i, n= N(t);
  array<string> a (n);
  for (i=0; i<n; i++) a[i]= t[i]->label;
  merge_sort (a);
  return a;
}

static void
close_completions (hashset<string>& h) {
  array<string> a= as_completions (h);
  int i, j, n= N(a);
  for (i=1; i<n; i++) {
    for (j=0; j < min (N(a[i-1]), N(a[i])); j++)
      if (a[i-1][j] != a[i][j]) break;
    if (j < min (N(a[i-1]), N(a[i])))
      h->insert (a[i](0,j));
  }
}

static array<string>
close_completions (array<string> a) {
  int i;
  hashset<string> h;
  for (i=0; i<N(a); i++) h->insert (a[i]);
  close_completions (h);
  return as_completions (h);
}

/******************************************************************************
* Finding completions in text
******************************************************************************/

static void
find_completions (
  drd_info drd, tree t, hashset<string>& h, string prefix= "")
{
  if (is_atomic (t)) {
    string s= t->label;
    int i= 0, n= N(s);
    while (i<n) {
      if (is_iso_alpha (s[i])) {
	int start= i;
	while ((i<n) && (is_iso_alpha (s[i]))) i++;
	string r= s (start, i);
	if (starts (r, prefix) && (r != prefix))
	  h->insert (r (N(prefix), N(r)));
      }
      else skip_symbol (s, i);
    }
  }
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (drd->is_accessible_child (t, i))
	find_completions (drd, t[i], h, prefix);
  }
}

static array<string>
find_completions (drd_info drd, tree t, string prefix= "") {
  hashset<string> h;
  find_completions (drd, t, h, prefix);
  return as_completions (h);
}

/******************************************************************************
* Completion mode
******************************************************************************/

bool
edit_interface_rep::complete_try () {
  tree st= subtree (et, path_up (tp));
  if (is_compound (st)) return false;
  string s= st->label, ss;
  int end= last_item (tp);
  array<string> a;
  if (inside (LABEL) || inside (REFERENCE)) {
    if (end != N(s)) return false;
    ss= copy (s);
    tree t= get_labels ();
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (is_atomic (t[i]) && starts (t[i]->label, s))
	a << string (t[i]->label (N(s), N(t[i]->label)));
  }
  else {
    if ((end==0) || (!is_iso_alpha (s[end-1])) ||
	((end!=N(s)) && is_iso_alpha (s[end]))) return false;
    int start= end-1;
    while ((start>0) && is_iso_alpha (s[start-1])) start--;
    ss= s (start, end);
    a= find_completions (drd, et, ss);
  }
  if (N(a) == 0) return false;
  complete_start (ss, a);
  return true;
}

void
edit_interface_rep::complete_message () {
  int i, n= N(completions);
  string s= "Other completions: ";
  for (i=1; i<min(n,11); i++) {
    int j= (completion_pos + i) % n;
    if (i != 1) s << ", ";
    s << completion_prefix << completions[j];
  }
  set_message (s, "tab");
}

void
edit_interface_rep::complete_start (string prefix, array<string> compls) {
  // check consistency
  tree st= subtree (et, path_up (tp));
  if (is_compound (st)) return;
  string s= st->label;
  int end= last_item (tp);
  if ((end<N(prefix)) || (s (end-N(prefix), end) != prefix)) return;

  // perform first completion and switch to completion mode if necessary
  if (N (compls) == 1) {
    string s= compls[0];
    if (ends (s, "()")) // temporary fix for Pari
      insert_tree (s, path (N(s)-1));
    else insert_tree (s);
    completions= array<string> ();
  }
  else {
    completion_prefix= prefix;
    completions      = close_completions (compls);
    completion_pos   = 0;
    insert_tree (completions[0]);
    complete_message ();
    beep ();
    set_input_mode (INPUT_COMPLETE);
  }
}

bool
edit_interface_rep::complete_keypress (string key) {
  if ((key != "tab") && (key != "S-tab")) return false;
  tree st= subtree (et, path_up (tp));
  if (is_compound (st)) return false;
  string s= st->label;
  int end= last_item (tp);
  string old_s= completions [completion_pos];
  string test= completion_prefix * old_s;
  if ((end<N(test)) || (s (end-N(test), end) != test)) return false;

  if (key == "tab") completion_pos++;
  else completion_pos--;
  if (completion_pos < 0) completion_pos= N(completions)-1;
  if (completion_pos >= N(completions)) completion_pos= 0;
  string new_s= completions [completion_pos];
  remove (path_up (tp) * (end-N(old_s)), N(old_s));
  insert (path_up (tp) * (end-N(old_s)), new_s);
  complete_message ();
  return true;
}
