
/******************************************************************************
* MODULE     : edit_process.cpp
* DESCRIPTION: incorporate automatically generated data into text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Process/edit_process.hpp"
#include "analyze.hpp"
#include "tm_buffer.hpp"
#include "merge_sort.hpp"
#include "Bibtex/bibtex.hpp"
#include "Bibtex/bibtex_functions.hpp"
#include "Sqlite3/sqlite3.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "scheme.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_process_rep::edit_process_rep () {}
edit_process_rep::~edit_process_rep () {}

/******************************************************************************
* Removing labels
******************************************************************************/

// Labels in TOC, index or glossaries (list-of-anything) leads to redefinition
// and impede typesetting. This generates errors reported via intrusive popups.
// So we remove them.

static tree
remove_labels (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, LABEL)) return "";
  int i, n= N(t);
  tree r (L(t));
  for (i=0; i<n; i++) {
    tree u= remove_labels (t[i]);
    if (!is_concat (t) || u != "") r << u;
  }
  if (is_func (r, CONCAT, 0)) return "";
  if (is_func (r, CONCAT, 1)) return r[0];
  return r;
}

/******************************************************************************
* Automatically generate a bibliography
******************************************************************************/

void
copy_bst_file (url base, string style) {
  string bst= style * ".bst";
  url u1= url ("$TEXMACS_HOME_PATH/system/bib", bst);
  url u2= relative (base, bst);
  if (!exists (u1) && exists (u2)) copy (u2, u1);
}

url
find_bib_file (url base, string fname,
               string suffix= ".bib", bool rooted= false) {
  if (!ends (fname, suffix)) fname= fname * suffix;
  url bibf (fname);
  if (exists (bibf) && (!rooted || is_rooted (bibf)))
    return bibf;
  if (exists (relative (base, bibf)))
    return relative (base, bibf);
  if (exists (expand (relative (base, url_ancestor () * bibf))))
    return resolve (expand (relative (base, url_ancestor () * bibf)));
  return url_none ();
}

bool
supports_db () {
  return get_preference ("database tool") == "on";
}

bool
uses_natbib (tree t) {
  if (is_atomic (t)) return false;
  if (is_compound (t, "natbib-triple")) return true;
  for (int i=0; i<N(t); i++)
    if (uses_natbib (t[i])) return true;
  return false;
}

void
edit_process_rep::generate_bibliography (
  string bib, string style, string fname)
{
  system_wait ("Generating bibliography, ", "please wait");
  if (DEBUG_AUTO)
    debug_automatic << "Generating bibliography"
                    << " [" << bib << ", " << style << ", " << fname << "]\n";
  tree bib_t= buf->data->aux[bib];
  if (buf->prj != NULL) bib_t= buf->prj->data->aux[bib];
  tree t;
  copy_bst_file (buf->buf->name, style);
  url bib_file= find_bib_file (buf->buf->name, fname);
  //cout << fname << " -> " << concretize (bib_file) << "\n";
  if (is_none (bib_file)) {
    url bbl_file= find_bib_file (buf->buf->name, fname, ".bbl");
    if (is_none (bbl_file)) {
      if (supports_db ()) {
        t= as_tree (call (string ("bib-compile"), bib, style, bib_t));
        call (string ("bib-attach"), bib, bib_t);
      }
      else {
	std_error << "Could not load BibTeX file " << fname;
        set_message ("Could not find bibliography file",
                     "compile bibliography");
        return;
      }
    }
    else t= bibtex_load_bbl (bib, bbl_file);
  }
  else {
    if (!bibtex_present () && !starts (style, "tm-")) {
      if (style == "abbrv") style= "tm-abbrv";
      else if (style == "acm") style= "tm-acm";
      else if (style == "alpha") style= "tm-alpha";
      else if (style == "elsart-num") style= "tm-elsart-num";
      else if (style == "ieeetr") style= "tm-ieeetr";
      else if (style == "siam") style= "tm-siam";
      else if (style == "unsrt") style= "tm-unsrt";
      else style= "tm-plain";
    }
    if (supports_db () && !is_rooted (bib_file))
      bib_file= find_bib_file (buf->buf->name, fname, ".bib", true);
    if (supports_db ()) {
      //(void) call (string ("bib-import-bibtex"), bib_file);
      t= as_tree (call (string ("bib-compile"), bib, style, bib_t, bib_file));
    }
    else if (starts (style, "tm-")) {
      string sbib;
      if (load_string (bib_file, sbib, false))
	std_error << "Could not load BibTeX file " << fname;
      tree te= bib_entries (parse_bib (sbib), bib_t);
      object ot= tree_to_stree (te);
      eval ("(use-modules (bibtex " * style (3, N(style)) * "))");
      t= stree_to_tree (call (string ("bib-process"),
                              bib, style (3, N(style)), ot));
    }
    else t= bibtex_run (bib, style, bib_file, bib_t);
    if (supports_db ())
      (void) call (string ("bib-attach"), bib, bib_t, bib_file);
    if (uses_natbib (t) && !defined_at_init ("cite-author-year-package")) {
      tree st= get_style ();
      if (is_atomic (st)) st= tuple (st);
      bool missing= true;
      for (int i=0; i<N(st); i++)
        if (st[i] == "cite-author-year") missing= false;
      if (missing) {
        st << "cite-author-year";
        change_style (st);
      }
    }
  }
  if (is_atomic (t) && starts (t->label, "Error:"))
    set_message (t->label, "compile bibliography");
  else if (is_compound (t) && N(t) > 0) insert_tree (t);
}

/******************************************************************************
* Automatically generate table of contents
******************************************************************************/

void
edit_process_rep::generate_table_of_contents (string toc) {
  if (DEBUG_AUTO)
    debug_automatic << "Generating table of contents [" << toc << "]\n";
  tree toc_t= buf->data->aux[toc];
  if (buf->prj != NULL) toc_t= copy (buf->prj->data->aux[toc]);
  if (N(toc_t)>0) insert_tree (remove_labels (toc_t));
}

/******************************************************************************
* Automatically generate an index
******************************************************************************/

static hashmap<string,tree> followup (TUPLE);

static string
index_name_sub (tree t, bool all) {
  if (is_atomic (t)) {
    string s= t->label, r;
    int i, n= N(s);
    for (i=0; i<n; i++)
      if (is_iso_alpha (s[i]) || is_digit (s[i]) || (s[i] == ' ') ||
	  (all && (s[i] >= ' '))) r << s[i];
    return r;
  }
  else if (is_concat (t)) {
    string r;
    int i, n= N(t);
    for (i=0; i<n; i++)
      r << index_name_sub (t[i], all);
    return r;
  }
  else if (is_tuple (t)) {
    string r;
    int i, j, n= N(t);
    for (i=0; i<n; i++) {
      if (i!=0) r << "\t";
      string s= index_name_sub (t[i], false);
      if (s == "") s= index_name_sub (t[i], true);
      tree u= copy (followup [s]);
      for (j=0; j<N(u); j++)
	if (u[j] == t[i]) break;
      if (j == N(u)) { u << t[i]; followup (s)= u; }
      r << s;
      if (j != 0) r << "\n" << as_string (j);
    }
    return r;
  }
  else if (all && is_func (t, WITH))
    return index_name_sub (t[N(t)-1], all);
  else return "";
}

static string
index_name (tree t) {
  if (is_func (t, TUPLE, 2)) t= t[0];
  else if (is_func (t, TUPLE, 3)) t= t[0];
  else if (is_func (t, TUPLE, 5)) {
    if (t[0] == "") t= t[3];
    else t= t[0];
  }
  if (!is_tuple (t)) t= tuple (t);
  return locase_all (index_name_sub (t, false));
}

static tree
index_value (tree t) {
  if (is_func (t, TUPLE, 2)) return t;
  else if (is_func (t, TUPLE, 3)) return tuple (t[2]);
  else if (is_func (t, TUPLE, 5)) {
    tree l= t[3], r= t[4];
    if (!is_tuple (l)) l= tuple (l);
    if (t[1] == "strong") r= compound ("strong", r);
    if (t[2] != "") r= tuple ("range", t[2], r);
    return tuple (l, r);
  }
  return "";
}

static void
insert_recursively (array<string>& a, string s, hashmap<string,tree>& h) {
  // cout << "Insert recursively \t" << s << "\n";
  int i= search_backwards ("\t", s);
  if (i != -1) {
    string r= s (0, i);
    if (!h->contains (r)) {
      tree u= h[s][0][0];
      h (r)= tuple (tuple (copy (u (0, N(u)-1)), ""));
      insert_recursively (a, s (0, i), h);
    }
  }
  a << s;
}

static void
make_entry (tree& D, tree t, hashmap<string,tree> refs) {
  // cout << "Make entry " << t << "\n";
  int i, j, n= N(t);
  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 1)) {
      bool flag= true;
      for (j=0; j<n; j++)
	if (is_func (t[j], TUPLE, 2) && (t[i][0] == t[j][0]))
	  flag= false;
      if (flag) D << t[i][0];
    }

  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 2) && is_tuple (t[i][1], "range", 2)) {
      bool flag= true;
      for (j=i+1; j<n; j++)
	if (is_func (t[j], TUPLE, 2) && is_tuple (t[j][1], "range", 2))
	  if ((t[i][0] == t[j][0]) && (t[i][1][1] == t[j][1][1])) {
	    t[i][1]= tree (CONCAT, t[i][1][2], "\25", t[j][1][2]);
	    t[j]= "";
	    flag= false;
	    break;
	  }
      if (flag) t[i][1]= tree (CONCAT, t[i][1][2], "\25?");
    }

  hashmap<tree,tree> h ("");
  hashmap<tree,string> last ("");
  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 2)) {
      tree l= t[i][0], r= t[i][1];
      string prev= "", next= "";
      if (is_func (r, PAGEREF, 1) &&
          is_atomic (r[0]) &&
          refs->contains (r[0]->label) &&
          is_func (refs[r[0]->label], TUPLE, 2) &&
          is_atomic (refs[r[0]->label][1])) {
        if (last->contains (l)) prev= last[l];
        next= refs[r[0]->label][1]->label;
        last (l)= next;
      }
      if (!h->contains (l)) h (l)= r;
      else {
	tree rr= h[l];
	if (rr == "") rr= r;
        else if (prev != "" && next == prev);
        else if (is_int (prev) && is_int (next) &&
                 as_int (next) == as_int (prev) + 1) {
	  if (!is_concat (rr))
            rr= tree (CONCAT, rr, "\25", r);
          else if (is_concat (rr) && N(rr) >= 2 && rr[N(rr)-2] == "\25")
            rr[N(rr)-1]= r;
          else
            rr << "\25" << r;
        }
	else if (r != "") {
	  if (!is_concat (rr)) rr= tree (CONCAT, rr);
	  rr << ", " << r;
	}
	h (l)= rr;
      }
    }

  for (i=0; i<n; i++)
    if (is_func (t[i], TUPLE, 2)) {
      tree l= t[i][0];
      if (h->contains (l)) {
	int k= N(l);
	tree e= compound ("index-" * as_string (k), copy (l[k-1]), h[l]);
	if (h[l] == "")
	  e= compound ("index-" * as_string (k) * "*", copy (l[k-1]));
	D << e;
	h->reset (l);
      }
    }
}

void
edit_process_rep::generate_index (string idx) {
  system_wait ("Generating index, ", "please wait");
  if (DEBUG_AUTO)
    debug_automatic << "Generating index [" << idx << "]\n";
  tree I= copy (buf->data->aux[idx]);
  hashmap<string,tree> R= buf->data->ref;
  if (buf->prj != NULL) {
    I= copy (buf->prj->data->aux[idx]);
    R= buf->prj->data->ref;
  }
  if (N(I)>0) {
    followup= hashmap<string,tree> (TUPLE);
    int i, n= N(I);
    array<string> entry (n);
    for (i=0; i<n; i++)
      entry[i]= index_name (I[i]);
    merge_sort (entry);

    hashmap<string,tree> h (TUPLE);
    for (i=0; i<n; i++) {
      string name = index_name  (I[i]);
      tree   value= index_value (I[i]);
      if (!h->contains (name)) h (name)= tuple (value);
      else h (name) << value;
    }

    array<string> new_entry;
    for (i=0; i<n; i++) {
      if ((i>0) && (entry[i] == entry[i-1])) continue;
      insert_recursively (new_entry, entry[i], h);
    }
    entry= new_entry;
    n= N(entry);

    tree D (DOCUMENT);
    for (i=0; i<n; i++)
      make_entry (D, h (entry[i]), R);
    insert_tree (remove_labels (D));
  }
}

/******************************************************************************
* Automatically generate a glossary
******************************************************************************/

void
edit_process_rep::generate_glossary (string gly) {
  system_wait ("Generating glossary, ", "please wait");
  if (DEBUG_AUTO)
    debug_automatic << "Generating glossary [" << gly << "]\n";
  tree G= copy (buf->data->aux[gly]);
  if (buf->prj != NULL) G= copy (buf->prj->data->aux[gly]);
  if (N(G)>0) {
    int i, n= N(G);
    tree D (DOCUMENT);
    for (i=0; i<n; i++)
      if (is_func (G[i], TUPLE, 1)) D << G[i][0];
      else if (is_func (G[i], TUPLE, 3) && (G[i][0] == "normal")) {
        tree content= G[i][1];
        if (is_document (content) && N(content) == 1) content= content[0];;
	tree L= compound ("glossary-1", content, G[i][2]);
	D << L;
      }
      else if (is_func (G[i], TUPLE, 4) && (G[i][0] == "normal")) {
        tree content= G[i][1];
        if (is_document (content) && N(content) == 1) content= content[0];;
	tree L= compound ("glossary-2", content, G[i][2], G[i][3]);
	D << L;
      }
      else if (is_func (G[i], TUPLE, 3) && (G[i][0] == "dup")) {
	int j;
	for (j=0; j<N(D); j++)
	  if ((is_compound (D[j], "glossary-1") ||
	       is_compound (D[j], "glossary-2")) &&
	      (D[j][0] == G[i][1]))
	    {
	      tree C= D[j][N(D[j])-1];
	      if (!is_concat (C)) C= tree (CONCAT, C);
	      C << ", ";
	      C << G[i][2];
	      D[j][N(D[j])-1]= C;
	    }
      }
    insert_tree (remove_labels (D));
  }
}

/******************************************************************************
* Automatically generate auxiliairy data and replace in text
******************************************************************************/

static bool
is_aux (tree t) {
  return
    is_compound (t, "bibliography", 4) ||
    is_compound (t, "bibliography*", 5) ||
    is_compound (t, "table-of-contents", 2) ||
    is_compound (t, "table-of-contents*", 3) ||
    is_compound (t, "the-index", 2) ||
    is_compound (t, "the-index*", 3) ||
    is_compound (t, "the-glossary", 2) ||
    is_compound (t, "the-glossary*", 3) ||
    is_compound (t, "list-of-figures", 2) ||
    is_compound (t, "list-of-tables", 2);
}

void
edit_process_rep::generate_aux_recursively (string which, tree st, path p) {
  int i, n= N(st);
  for (i=0; i<n; i++)
    if (!is_aux (st[i])) {
      if (is_compound (st[i]))
	generate_aux_recursively (which, st[i], p * i);
    }
    else {
      tree t= st[i];
      path doc_p= p * path (i, N(t)-1);
      assign (doc_p, tree (DOCUMENT, ""));
      go_to (doc_p * path (0, 0));

      /*
	cout << "et= " << et << "\n";
	cout << "tp= " << tp << "\n";
	cout << "------------------------------------------------------\n";
      */
      if (arity (t) >= 1) {
	if ((arity(t) >= 3) &&
	    (is_compound (t, "bibliography") ||
	     is_compound (t, "bibliography*")) &&
	    ((which == "") || (which == "bibliography")))
	  generate_bibliography (as_string (t[0]), as_string (t[1]),
				 as_string (t[2]));
	if ((is_compound (t, "table-of-contents") ||
	     is_compound (t, "table-of-contents*")) &&
	    ((which == "") || (which == "table-of-contents")))
	  generate_table_of_contents (as_string (t[0]));
	if ((is_compound (t, "the-index") || is_compound (t, "the-index*")) &&
	    ((which == "") || (which == "the-index")))
	  generate_index (as_string (t[0]));
	if ((is_compound (t, "the-glossary") ||
	     is_compound (t, "the-glossary*")) &&
	    ((which == "") || (which == "the-glossary")))
	  generate_glossary (as_string (t[0]));
	if (is_compound (t, "list-of-figures") &&
	    ((which == "") || (which == "list-of-figures")))
	  generate_glossary (as_string (t[0]));
	if (is_compound (t, "list-of-tables") &&
	    ((which == "") || (which == "list-of-tables")))
	  generate_glossary (as_string (t[0]));
      }
      /*
	cout << "et= " << et << "\n";
	cout << "tp= " << tp << "\n";
	cout << "------------------------------------------------------\n\n\n";
      */
    }
}

void
edit_process_rep::generate_aux (string which) {
  // path saved_path= tp;
  generate_aux_recursively (which, subtree (et, rp), rp);
  init_update ();
  // if (which == "") go_to (saved_path);
  // ... may be problematic if cursor was inside regenerated content
}

bool
edit_process_rep::get_save_aux () {
  return as_bool (get_init_string (SAVE_AUX));
}
