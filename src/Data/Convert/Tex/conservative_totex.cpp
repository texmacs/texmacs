
/******************************************************************************
* MODULE     : conservative_totex.cpp
* DESCRIPTION: Conservative conversion of TeXmacs to LaTeX
* COPYRIGHT  : (C) 2014 Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"
#include "analyze.hpp"
#include "hashset.hpp"
#include "scheme.hpp"
#include "iterator.hpp"

/******************************************************************************
* Extract tables for source/target correspondences
******************************************************************************/

tree
texmacs_correspondence (tree t, hashmap<tree,tree>& h) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "mlx", 2)) {
    tree r= texmacs_correspondence (t[1], h);
    if (!h->contains (r)) h(r)= tree (TUPLE);
    h(r) << t[0];
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_correspondence (t[i], h);
    if (is_concat (r)) return simplify_concat (r);
    if (is_document (r)) return simplify_document (r);
    return r;
  }
}

void
texmacs_neighbours (tree t,
                    hashmap<tree,tree>& prec,
                    hashmap<tree,tree>& succ) {
  if (is_atomic (t)) return;
  else {
    int i, n= N(t);
    for (i=1; i<n; i++)
      if (is_compound (t[i-1], "mlx", 2) &&
          is_compound (t[i], "mlx", 2)) {
        prec (t[i  ][0])= t[i-1][0];
        succ (t[i-1][0])= t[i  ][0];
      }
  }
}

/******************************************************************************
* Construct invarianted TeXmacs document
******************************************************************************/

static bool
get_range (tree id, int& b, int& e, string src) {
  array<string> a= tokenize (as_string (id), ":");
  if (N(a) != 2) return false;
  b= as_int (a[0]);
  e= as_int (a[1]);
  return b >= 0 && b <= e && e <= N(src);
}

int
common_len (tree id, tree p, int c, int delta,
            hashmap<tree,tree> corr,
            hashmap<tree,tree> next) {
  int len= 0;
  while (true) {
    c += delta;
    if (c < 0 || c >= N(p)) break;
    if (!corr->contains (p[c])) break;
    tree next_ids= corr[p[c]];
    if (!next->contains (id)) break;
    tree next_id= next[id];
    bool found= false;
    for (int k=0; k<N(next_ids); k++)
      found= found || (next_ids[k] == next_id);
    if (!found) break;
    len++;
  }
  return len;
}

tree
texmacs_best_match (tree ids, tree p, int c,
                    hashmap<tree,tree> corr,
                    hashmap<tree,tree> pred,
                    hashmap<tree,tree> succ) {
  if (N(ids) == 1) return ids[0];
  int best= -1, best_len= -1;
  for (int i=0; i<N(ids); i++) {
    int plen= common_len (ids[i], p, c, -1, corr, pred);
    int slen= common_len (ids[i], p, c,  1, corr, succ);
    int len = plen + slen;
    if (len > best_len) {
      best= i;
      best_len= len;
    }
    else if (len == best_len)
      best= -1;
  }
  //if (best >= 0)
  //cout << HRULE << "Multiple matches: " << ids
  //<< " -> " << ids[best] << LF << HRULE;
  return best >= 0? ids[best]: tree (UNINIT);
}

tree
texmacs_invarianted (tree t, tree p, int c, string src,
                     hashmap<tree,tree> corr,
                     hashmap<tree,tree> pred,
                     hashmap<tree,tree> succ) {
  if (corr->contains (t)) {
    tree oids= corr[t], ids (TUPLE);
    for (int i=0; i<N(oids); i++) {
      int b, e;
      if (get_range (oids[i], b, e, src)) ids << oids[i];
    }
    if (N(ids) >= 1) {
      tree id= texmacs_best_match (ids, p, c, corr, pred, succ);
      if (id != tree (UNINIT)) return compound ("ilx", id);
    }
  }
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_invarianted (t[i], t, i, src, corr, pred, succ);
    return r;
  }
}

static void
rewind_spaces (string s, int& i) {
  while (i>0 && (s[i-1] == ' ' || s[i-1] == '\t')) i--;
}

static void
rewind_line (string s, int& i) {
  while (i>0 && (s[i-1] != '\n')) i--;
}

static void
forward_line (string s, int& i) {
  while (i<N(s) && (s[i] != '\n')) i++;
}

static bool
is_comment_line (string s) {
  int i=0;
  skip_spaces (s, i);
  return i<N(s) && s[i] == '%';
}

tree
texmacs_invarianted_extend (tree id, string src) {
  int b, e, n= N(src);
  get_range (id, b, e, src);
  //cout << "Extending" << LF << HRULE << src (b, e) << LF << HRULE;
  rewind_spaces (src, b);
  while (b>0 && src[b-1] == '\n') {
    int prev= b-1;
    rewind_line (src, prev);
    if (is_comment_line (src (prev, b-1))) b= prev;
    else break;
  }
  skip_spaces (src, e);
  if (e<n && src[e] == '%') skip_line (src, e);
  while (e<n && src[e] == '\n') {
    int next= e+1;
    forward_line (src, next);
    if (is_comment_line (src (e+1, next))) e= next;
    else break;
  }
  //cout << "Extended to" << LF << HRULE << src (b, e) << LF << HRULE;
  return as_string (b) * ":" * as_string (e);
}

static void
get_subtree_paths (tree t, path p, hashmap<tree,path>& h) {
  if (h->contains (t)) h (t)= path (-1);
  else h (t)= p;
  if (is_compound (t))
    for (int i=0; i<N(t); i++)
      get_subtree_paths (t[i], p * i, h);
}

tree
texmacs_invarianted_merge (tree t, string src,
                           tree org, tree u, hashmap<tree,path> h) {
  if (is_atomic (t)) return t;
  else {
    if (true) {
      int i, n= N(t);
      tree r (t, n);
      for (i=0; i<n; i++)
        r[i]= texmacs_invarianted_merge (t[i], src, org, u, h);
      t= r;
    }
    if (is_concat (t) || is_document (t)) {
      int i, n= N(t);
      tree r (L(t));
      for (i=0; i<n; i++) {
        if (is_document (t) && is_compound (t[i], "ilx", 1))
          t[i]= compound ("ilx", texmacs_invarianted_extend (t[i][0], src));

        if (N(r) > 0 &&
            is_compound (r[N(r)-1], "ilx", 1) &&
            is_compound (t[i], "ilx", 1)) {
          int b1, e1, b2, e2;
          bool ok = get_range (r[N(r)-1][0], b1, e1, src);
          ok = get_range (t[i][0], b2, e2, src) || ok;
          if (ok && e1 <= b2) {
            skip_latex_spaces (src, e1);
            if (e1 >= b2) {
              string id= as_string (b1) * ":" * as_string (e2);
              r[N(r)-1][0]= id;
              continue;
            }
          }
        }

        int j= i;
        while (j<n && !is_compound (t[j], "ilx", 1)) j++;
        if (j < n && j > i && N(r) > 0 && is_compound (r[N(r)-1], "ilx", 1)) {
          // NOTE: this special treatment allows for the recognition of
          // pieces which may be invarianted even in case of missing markers
          int b1, e1, b2, e2;
          bool ok1= get_range (r[N(r)-1][0], b1, e1, src);
          bool ok2= get_range (t[j][0], b2, e2, src);
          if (ok1 && ok2 && e1 <= b2 && i-1 < N(org)) {
            path p= h [org[i-1]];
            if (p != path (-1)) {
              tree pt= subtree (u, path_up (p));
              int k, k2= last_item (p);
              for (k=i-1; k<=j && k2<N(pt); k++, k2++)
                if (org[k] != pt[k2]) {
                  //cout << "  <<< " << org[k] << LF
                  //     << "  >>> " << pt[k2] << LF;
                  break;
                }
              if (k > j) {
                string id= as_string (b1) * ":" * as_string (e2);
                r[N(r)-1][0]= id;
                i= j;
                continue;
              }
            }
          }
        }

        r << t[i];
      }
      if (is_concat (r) && N(r) == 1) r= r[0];
      return r;
    }
    return t;
  }
}

tree
texmacs_invarianted_replace (tree t, string src) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "ilx", 1)) {
    int b, e;
    get_range (t[0], b, e, src);
    return compound ("!ilx", src (b, e));
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= texmacs_invarianted_replace (t[i], src);
    return r;
  }
}

tree
texmacs_invarianted (tree t, tree oldt, string src) {
  tree orgbody= extract (t, "body");
  tree oldbody= extract (oldt, "body");
  hashmap<tree,tree> corr (UNINIT);
  hashmap<tree,tree> pred (UNINIT);
  hashmap<tree,tree> succ (UNINIT);
  tree uoldbody= texmacs_correspondence (oldbody, corr);
  texmacs_neighbours (oldbody, pred, succ);
  tree body= orgbody;
  body= texmacs_invarianted (body, UNINIT, -1, src, corr, pred, succ);
  hashmap<tree,path> h (path (-1));
  //cout << "body" << LF << HRULE << body << LF << HRULE;
  //cout << "orgbody" << LF << HRULE << orgbody << LF << HRULE;
  //cout << "uoldbody" << LF << HRULE << uoldbody << LF << HRULE;
  get_subtree_paths (uoldbody, path (), h);
  body= texmacs_invarianted_merge (body, src, orgbody, uoldbody, h);
  //cout << "merged" << LF << HRULE << body << LF << HRULE;
  body= texmacs_invarianted_replace (body, src);
  return change_doc_attr (t, "body", body);
}

/******************************************************************************
* Conserve as much of the metadata as possible
******************************************************************************/

static bool
latex_unchanged_metadata (tree oldt, tree newt) {
  tree oldb= extract (oldt, "body");
  tree newb= extract (newt, "body");
  int  oldi= search_doc_data (oldb);
  int  newi= search_doc_data (newb);
  return oldi >= 0 && newi >= 0 && oldb[oldi] == newb[newi];
}

string
latex_merge_metadata (string olds, string news) {
  array<path> oldps= latex_get_metadata_snippets (olds, false);
  array<path> newps= latex_get_metadata_snippets (news, false);
  if (N(oldps) > 0 && N(newps) == 1) {
    string accum;
    for (int i=0; i<N(oldps); i++)
      accum << olds (oldps[i][0], oldps[i][1]);
    return news (0, newps[0][0]) * accum * news (newps[0][1], N(news));
  }
  else return news;
}

/******************************************************************************
* Conserve as much of the abstract as possible
******************************************************************************/

static bool
latex_unchanged_abstract (tree oldt, tree newt) {
  tree oldb= extract (oldt, "body");
  tree newb= extract (newt, "body");
  int  oldi= search_abstract_data (oldb);
  int  newi= search_abstract_data (newb);
  return oldi >= 0 && newi >= 0 && oldb[oldi] == newb[newi];
}

string
latex_merge_abstract (string olds, string news) {
  array<path> oldps= latex_get_metadata_snippets (olds, true);
  array<path> newps= latex_get_metadata_snippets (news, true);
  if (N(oldps) > 0 && N(newps) == 1) {
    string accum;
    for (int i=0; i<N(oldps); i++)
      accum << olds (oldps[i][0], oldps[i][1]);
    return news (0, newps[0][0]) * accum * news (newps[0][1], N(news));
  }
  else return news;
}

/******************************************************************************
* Conserve style and preamble in case of unchanged preambles
******************************************************************************/

/* NOTE: this code has currently been superseded

static tree
simplify_initial (tree init) {
  if (!is_func (init, COLLECTION)) return init;
  tree r (COLLECTION);
  for (int i=0; i<N(init); i++)
    if (is_func (init[i], ASSOCIATE, 2) && is_atomic (init[i][0])) {
      string s= init[i][0]->label;
      if (s == "font-base-size" ||
          s == "language" ||
          s == "page-top" ||
          s == "page-bot" ||
          s == "page-odd" ||
          s == "page-even" ||
          s == "page-right" ||
          s == "page-height" ||
          s == "page-width" ||
          s == "page-orientation")
        r << init[i];
    }
  return r;
}

bool
texmacs_unchanged_preamble (tree oldt, tree newt) {
  tree olds= extract (oldt, "style");
  tree news= extract (newt, "style");
  tree oldi= extract (oldt, "initial");
  tree newi= extract (newt, "initial");
  tree oldb= extract (oldt, "body");
  tree newb= extract (newt, "body");
  if (news != olds) return false;
  if (simplify_initial (newi) != simplify_initial (oldi)) return false;
  if (!is_document (oldb) || !is_document (newb)) return false;
  if (is_compound (oldb[0], "hide-preamble", 1)) {
    if (!is_compound (newb[0], "hide-preamble", 1)) return false;
    if (newb[0] != oldb[0]) return false;
  }
  return true;
}

string
latex_recover_preamble (string news, string olds) {
  int oldp= search_forwards ("\\begin{document}", olds);
  int newp= search_forwards ("\\begin{document}", news);
  if (oldp < 0 || newp < 0) return news;
  return olds (0, oldp) * news (newp, N(news));
}

*/

/******************************************************************************
* Conserve as much of the style and preamble as possible, otherwise
******************************************************************************/

static object
get_used_packages (string src) {
  hashmap<string,path> h= latex_get_packages (src);
  object packs= null_object ();
  iterator<string> it= iterate (h);
  while (it->busy ())
    packs= cons (object (it->next ()), packs);
  return packs;
}

static string
merge_styles (string olds, string news) {
  int oldb, olde, newb, newe;
  string oldst= latex_get_style (olds, oldb, olde);
  string newst= latex_get_style (news, newb, newe);
  if (newst == oldst || oldst == "" || newst == "") return olds;
  return olds (0, oldb) * news (newb, newe) * olds (olde, N(olds));
}

static string
merge_packages (string olds, string news) {
  // Extract usepackage commands from news
  hashmap<string,path> oldp= latex_get_packages (olds);
  hashmap<string,path> newp= latex_get_packages (news);
  hashmap<int,string> packs;
  iterator<string> it= iterate (newp);
  while (it->busy ()) {
    string name= it->next ();
    if (!oldp->contains (name)) {
      path p= newp [name];
      packs (p[0])= news (p[0], p[1]);
    }
  }
  string accum;
  for (int i=0; i<N(news); i++)
    if (test (news, i, "\\begin{document}")) break;
    else if (packs->contains (i)) accum << packs[i] << "\n";

  // Insert result into olds, just after last \usepackage{...}
  int docpos= search_forwards ("\\begin{document}", olds);
  if (docpos < 0) return olds;
  int start= search_backwards ("\\usepackage", docpos, olds);
  if (start < 0) start= search_backwards ("\\documentclass", docpos, olds);
  if (start < 0) start= search_backwards ("\\documentstyle", docpos, olds);
  if (start < 0) return olds;
  skip_line (olds, start);
  if (start > docpos) return olds;
  return olds (0, start) * accum * olds (start, N(olds));
}

static string
replace (string s, hashmap<int,string> w, hashmap<int,string> b) {
  string r;
  for (int i=0; i<N(s); )
    if (w->contains (i)) {
      r << b[i];
      i += N(w[i]);
    }
    else r << s[i++];
  return r;
}

static string
merge_declarations (string olds, string news) {
  hashmap<string,path> oldd= latex_get_declarations (olds);
  hashmap<string,path> newd= latex_get_declarations (news);
  //cout << "oldd= " << oldd << "\n";
  //cout << "newd= " << newd << "\n";

  // Substitute redefinitions into old preamble
  hashmap<int,string> oldw, oldb;
  iterator<string> it= iterate (oldd);
  while (it->busy ()) {
    string cmd= it->next ();
    if (newd->contains (cmd)) {
      path oldp= oldd[cmd];
      path newp= newd[cmd];
      oldw (oldp[0])= olds (oldp[0], oldp[1]);
      oldb (oldp[0])= news (newp[0], newp[1]);
    }
  }
  olds= replace (olds, oldw, oldb);

  // Extract new definitions from new preamble
  hashmap<int,string> back;
  it= iterate (newd);
  while (it->busy ()) {
    string cmd= it->next ();
    if (!oldd->contains (cmd)) {
      path p= newd [cmd];
      back (p[0])= news (p[0], p[1]);
    }
  }
  string accum;
  for (int i=0; i<N(news); i++)
    if (test (news, i, "\\begin{document}")) break;
    else if (back->contains (i)) accum << back[i] << "\n";

  // Insert new definitions into old preamble
  int oldi= search_forwards ("\\begin{document}", olds);
  int newi= search_forwards ("\\begin{document}", news);
  if (oldi < 0) {
    if (ends (olds, "\n") || olds == "" || accum == "") return olds * accum;
    else return olds * "\n" * accum;
  }
  int i= oldi, cnt= 0;
  while (i>0 && (olds[i-1] == ' ' || olds[i-1] == '\t' || olds[i-1] == '\n')) {
    i--; if (olds[i] == '\n') cnt++; }
  if (cnt < 2 && N(accum) != 0) accum= "\n" * accum;
  if (N(accum) != 0) accum << "\n";
  return olds (0, oldi) * accum * news (newi, N(news));
}

string
latex_merge_preamble (string olds, string news) {
  string oldl= latex_remove_texmacs_preamble (olds);
  string newl= latex_remove_texmacs_preamble (news);
  string oldt= latex_get_texmacs_preamble (olds);
  string newt= latex_get_texmacs_preamble (news);
  oldl= merge_styles (oldl, newl);
  oldl= merge_packages (oldl, newl);
  oldl= merge_declarations (oldl, newl);
  oldt= merge_declarations (oldt, newt);
  if (oldt == "") return oldl;
  else return latex_set_texmacs_preamble (oldl, oldt);
}

/******************************************************************************
* Conservative TeXmacs -> LaTeX conversion
******************************************************************************/

string
conservative_texmacs_to_latex (tree doc, object opts) {
  if (get_preference ("texmacs->latex:conservative", "off") != "on")
    return tracked_texmacs_to_latex (doc, opts);
  tree atts= extract (doc, "attachments");
  hashmap<string,tree> atts_map (UNINIT, atts);
  if (!atts_map->contains ("latex-source"))
    return tracked_texmacs_to_latex (doc, opts);
  string lsource= as_string (atts_map["latex-source"]);
  tree ltarget= atts_map["latex-target"];
  tree target= texmacs_unmark (ltarget);
  if (doc == target) return lsource;
  tree idoc= texmacs_invarianted (doc, ltarget, lsource);
  call ("latex-set-virtual-packages", get_used_packages (lsource));
  string conv= tracked_texmacs_to_latex (idoc, opts);
  call ("latex-set-virtual-packages", null_object ());
  if (latex_unchanged_metadata (target, doc))
    conv= latex_merge_metadata (lsource, conv);
  if (latex_unchanged_abstract (target, doc))
    conv= latex_merge_abstract (lsource, conv);
  //cout << "Conversion" << LF << HRULE << conv << HRULE;
  //if (texmacs_unchanged_preamble (target, doc))
  //conv= latex_recover_preamble (conv, lsource);
  //else
  conv= latex_merge_preamble (lsource, conv);
  return conv;
}
