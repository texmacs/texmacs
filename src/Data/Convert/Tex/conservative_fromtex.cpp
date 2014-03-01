
/******************************************************************************
* MODULE     : conservative_fromtex.cpp
* DESCRIPTION: Conservative conversion of LaTeX to TeXmacs
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
#include "base64.hpp"
#include "iterator.hpp"

string encode_as_string (path p);
path decode_as_path (string s);
string latex_unmark (string s, hashset<path> l, hashmap<int,path>& corr);

/******************************************************************************
* Getting the TeXmacs attachments
******************************************************************************/

static bool
get_texmacs_attachments (string s, string& mod, tree& src, string& mtar) {
  string bs= "\n%%%%%%%%%% Begin TeXmacs source\n";
  string es= "\n%%%%%%%%%% End TeXmacs source\n";
  int bpos= search_forwards (bs, 0, s);
  if (bpos < 0) return false;
  int epos= search_forwards (es, bpos + N(bs), s);
  if (epos < 0) return false;
  int i= epos + N(es), n= N(s);
  while (i<n && (s[i] == ' ' || s[i] == '\t' || s[i] == '\n')) i++;
  if (i<n) return false;
  string comm_enc_atts= s (bpos + N(bs), epos);
  string enc_atts= replace (comm_enc_atts, "\n %", "\n");
  string atts= decode_base64 (enc_atts);
  // TODO: check integrity checksum
  string sep= "\n% Separate attachments\n";
  int spos= search_forwards (sep, 0, atts);
  if (spos < 0) return false;
  mod = s (0, bpos);
  src= scheme_to_tree (atts (0, spos));
  mtar= atts (spos + N(sep), N(atts));
  return true;
}

/******************************************************************************
* Computing the correspondence
******************************************************************************/

string
latex_correspondence (string mtar, hashmap<path,path>& corr) {
  hashset<path> l;
  hashmap<int,path> pcorr;
  string tar= latex_unmark (mtar, l, pcorr);
  hashmap<path,int> inv (-1);
  iterator<int> it= iterate (pcorr);
  while (it->busy ()) {
    int pos= it->next ();
    path p= pcorr [pos];
    path cp= path_up (p) * (1 - last_item (p));
    inv (p)= pos;
    int cpos= inv [cp];
    if (cpos >= 0) {
      int b= min (pos, cpos);
      int e= max (pos, cpos);
      corr (path_up (p))= path (b, e);
    }
  }
  return tar;
}

/******************************************************************************
* Construct invarianted LaTeX document
******************************************************************************/

void
latex_invarianted_search (string s, tree t, path p, string tar,
                          hashmap<path,path> corr,
                          array<bool>& done, hashmap<int,path>& subs) {
  if (corr->contains (p)) {
    path r= corr[p];
    int b= r->item, e= r->next->item;
    if (b >= 0 && e <= N(tar)) {
      string ss= tar (b, e);
      int pos= search_forwards (ss, 0, s);
      // TODO: more efficient searching
      // TODO: several matches
      if (pos >= 0) {
        bool ok= true;
        for (int i= pos; i<pos+N(ss); i++)
          ok= ok && !done[i];
        if (ok) {
          for (int i= pos; i<pos+N(ss); i++)
            done[i]= true;
          subs (pos)= path (N(ss), p);
        }
      }
    }
  }
  if (is_compound (t)) {
    for (int i=0; i<N(t); i++)
      latex_invarianted_search (s, t[i], p * i, tar, corr, done, subs);
  }
}

string
latex_invarianted_apply (string s, hashmap<int,path> subs) {
  string r;
  int i= 0, n= N(s);
  while (i<n) {
    if (subs->contains (i)) {
      path p= subs[i];
      int len= p->item;
      string id= encode_as_string (p->next);
      r << "{\\itm{" << id << "}}";
      i += len;
    }
    else r << s[i++];
  }
  return r;
}

string
latex_invarianted (string s, tree src, string tar, hashmap<path,path> corr) {
  array<bool> done (N(s));
  hashmap<int,path> subs;
  for (int i=0; i<N(s); i++) done[i]= false;
  latex_invarianted_search (s, src, path (), tar, corr, done, subs);
  cout << "subs= " << subs << "\n";
  string invs= latex_invarianted_apply (s, subs);
  return invs;
}

/******************************************************************************
* Replace invarianted identifiers by corresponding trees
******************************************************************************/

tree
latex_invarianted_replace (tree t, tree src) {
  if (is_atomic (t)) return t;
  else if (is_compound (t, "itm", 1)) {
    path p= decode_as_path (as_string (t[0]));
    if (has_subtree (src, p)) return subtree (src, p);
    else return t;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= latex_invarianted_replace (t[i], src);
    return r;
  }
}

/******************************************************************************
* Conservative TeXmacs -> LaTeX conversion
******************************************************************************/

tree
conservative_latex_to_texmacs (string s, bool as_pic) {
  string mod;
  tree src;
  string mtar;
  bool ok= get_texmacs_attachments (s, mod, src, mtar);
  if (!ok) return tracked_latex_to_texmacs (s, as_pic);
  hashmap<path,path> corr;
  string tar= latex_correspondence (mtar, corr);
  cout << "corr= " << corr << LF;
  string imod= latex_invarianted (mod, src, tar, corr);
  cout << "imod= " << imod << LF;
  tree iconv= tracked_latex_to_texmacs (imod, as_pic);
  cout << "iconv= " << iconv << LF;
  tree ibody= extract (iconv, "body");
  tree body= latex_invarianted_replace (ibody, src);
  tree conv= change_doc_attr (iconv, "body", body);
  cout << "conv= " << conv << LF;
  return conv;
}
