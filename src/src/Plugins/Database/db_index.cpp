
/******************************************************************************
* MODULE     : db_index.cpp
* DESCRIPTION: indexation for TeXmacs databases
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Database/database.hpp"
#include "hashset.hpp"
#include "convert.hpp"
#include "universal.hpp"
#include "analyze.hpp"

#define MAX_PREFIX_LENGTH 6

/******************************************************************************
* Computing list of keywords in a string
******************************************************************************/

inline bool
is_keyword_char (char c) {
  return is_digit (c) || is_locase (c) || c == '_';
}

void
compute_keywords (array<string>& r, string s) {
  s= uni_translit (s);
  s= locase_all (s);
  array<string> arr= tokenize(s, " ");
  int i=0, n=N(arr);
  while (i<n) {
    r << arr[i];
    i++;
  }
}

void
compute_keywords (array<string>& r, tree t) {
  if (is_atomic (t)) compute_keywords (r, t->label);
  else {
    int i, n=N(t);
    for (i=0; i<n; i++)
      compute_keywords (r, t[i]);
  }
}

array<string>
compute_keywords (string s) {
  array<string> r;
  tree t= texmacs_to_tree (s);
  compute_keywords (r, t);
  return r;
}

/******************************************************************************
* Key management
******************************************************************************/

db_key
database_rep::as_key (string s) {
  if (!key_encode->contains (s)) {
    db_key code= (db_key) N (key_decode);
    key_encode (s)= code;
    key_decode << s;
    key_occurrences << db_atoms ();
  }
  return key_encode[s];
}

string
database_rep::from_key (db_key a) {
  ASSERT (a < N(key_decode), "Invalid key");
  return key_decode[a];
}

/******************************************************************************
* Indexation
******************************************************************************/

void
database_rep::add_completed_as (db_key k) {
  string kw= from_key (k);
  int pos= 0, n= N(kw);
  for (int i=0; i<MAX_PREFIX_LENGTH && pos<n; i++) {
    tm_char_forwards (kw, pos);
    string ss= kw (0, pos);
    if (!key_completions->contains (ss))
      key_completions (ss)= db_keys ();
    key_completions (ss) << k;
    //cout << "Completions " << ss << " -> " << key_completions[ss] << LF;
  }
}

void
database_rep::indexate (db_atom val) {
  if (atom_indexed[val]) return;
  array<string> kws= compute_keywords (from_atom (val));
  //cout << "Indexate " << from_atom (val) << " -> " << kws << LF;
  for (int i=0; i<N(kws); i++) {
    bool new_key= !key_encode->contains (kws[i]);
    db_key k= as_key (kws[i]);
    key_occurrences[k] << val;
    if (new_key) add_completed_as (k);
  }
  atom_indexed[val]= true;
}

void
database_rep::indexate_name (db_atom val) {
  if (name_indexed[val]) return;
  string s= atom_decode[val];
  int pos= 0, n= N(s);
  for (int i=0; i<MAX_PREFIX_LENGTH && pos<n; i++) {
    tm_char_forwards (s, pos);
    string ss= s (0, pos);
    if (!name_completions->contains (ss))
      name_completions (ss)= db_keys ();
    name_completions (ss) << val;
    //cout << "Name completions " << ss << " -> " << name_completions[ss] << LF;
  }
  name_indexed[val]= true;
}

/******************************************************************************
* Using the index
******************************************************************************/

db_constraint
database_rep::encode_keywords_constraint (tree q) {
  //cout << "Encoding " << q << LF;
  hashset<db_atom> done;
  db_constraint r;
  r << -1;
  for (int i=1; i<N(q); i++)
    if (is_atomic (q[i])) {
      string kw= scm_unquote (q[i]->label);
      //cout << "  Keyword " << kw << LF;
      if (key_encode->contains (kw)) {
        db_key k= key_encode[kw];
        db_atoms vals= key_occurrences[k];
        if (N(r) + N(vals) > 1000) {
          r= db_constraint ();
          r << -2;
          return r;
        }
        for (int i=0; i<N(vals); i++) {
          //cout << "    Treating " << vals[i] << ", " << from_atom (vals[i]) << LF;
          if (!done->contains (vals[i])) {
            r << vals[i];
            done->insert (vals[i]);
          }
        }
      }
    }
  //cout << "Encoded as " << r << LF;
  return r;
}

strings
database_rep::compute_completions (string s) {
  int pos=0, n=N(s);
  for (int i=0; i<MAX_PREFIX_LENGTH && pos<n; i++)
    tm_char_forwards (s, pos);
  db_keys ks= key_completions (s (0, pos));
  strings r;
  for (int i=0; i<N(ks); i++)
    if (pos == n || starts (from_key (ks[i]), s))
      r << from_key (ks[i]);
  return r;
}

strings
database_rep::compute_name_completions (string s) {
  int pos=0, n=N(s);
  for (int i=0; i<MAX_PREFIX_LENGTH && pos<n; i++)
    tm_char_forwards (s, pos);
  db_atoms vals= name_completions (s (0, pos));
  strings r;
  for (int i=0; i<N(vals); i++)
    if (pos == n || starts (from_atom (vals[i]), s))
      r << from_atom (vals[i]);
  return r;
}

tree
database_rep::normalize_query (tree q) {
  if (!is_tuple (q)) return q;
  tree r (TUPLE);
  for (int i=0; i<N(q); i++)
    if (is_tuple (q[i], "contains", 1) ||
        is_tuple (q[i], "completes", 1)) {
      if (is_atomic (q[i][1]) && is_quoted (q[i][1]->label)) {
        bool flag= is_tuple (q[i], "completes", 1);
        string s= scm_unquote (q[i][1]->label);
        strings kws= compute_keywords (s);
        int n= N(kws);
        if (n == 0) continue;
        if (flag) n--;
        for (int j=0; j<n; j++)
          r << tree (TUPLE, "keywords", scm_quote (kws[j]));
        if (flag) {
          tree t (TUPLE, "keywords");
          strings cs= compute_completions (kws[n]);
          for (int k=0; k<N(cs); k++)
            t << scm_quote (cs[k]);
          r << t;
        }
      }
    }
    else r << q[i];
  return r;
}
