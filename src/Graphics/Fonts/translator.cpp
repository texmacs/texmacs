
/******************************************************************************
* MODULE     : translator.cpp
* DESCRIPTION: used for the translation of tokens, mainly to name symbols
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "translator.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "iterator.hpp"
#include "analyze.hpp"

RESOURCE_CODE(translator);

/******************************************************************************
* Routines for translators
******************************************************************************/

translator&
operator << (translator& trl, int i) {
  trl->cur_c= i;
  return trl;
}

translator&
operator << (translator& trl, string s) {
  if (N(s)>0) {
    if (N(s)>1) s= "<" * s * ">";
    trl->dict(s) = trl->cur_c;
    if (starts (s, "<large-")) {
      string sub= s (7, N(s)-1);
      trl->dict ("<left-"  * sub * ">") = trl->cur_c;
      trl->dict ("<mid-"   * sub * ">") = trl->cur_c;
      trl->dict ("<right-" * sub * ">") = trl->cur_c;
      if (ends (s, "-0>")) return trl << s (1, N(s)-3);
    }
  }
  trl->cur_c++;
  return trl;
}

translator&
operator << (translator& trl, translator trm) {
  if ((trl->cur_c & 255) != 0) return trl;
  iterator<string> it= iterate (trm->dict);
  while (it->busy()) {
    string key= it->next();
    trl->dict (key)= trl->cur_c+ trm->dict [key];
  }
  trl->cur_c += 256;
  return trl;
}

/******************************************************************************
* Loading virtual fonts as translators
******************************************************************************/

translator
load_virtual (string name) {
  if (translator::instances -> contains (name))
    return translator (name);
  translator trl= tm_new<translator_rep> (name);

  string s, r;
  name= name * ".vfn";
  if (DEBUG_STD) cout << "TeXmacs] Loading " << name << "\n";
  url u ("$TEXMACS_HOME_PATH/fonts/virtual:$TEXMACS_PATH/fonts/virtual", name);
  load_string (u, s, true);
  tree t= string_to_scheme_tree (s);
  ASSERT (is_tuple (t, "virtual-font"), "bad virtual font format");

  int i, n= N(t);
  trl->virt_def= array<tree> (n);
  for (i=1; i<n; i++)
    if (is_func (t[i], TUPLE, 2) && is_atomic (t[i][0])) {
      string s= as_string (t[i][0]);
      if (N(s)>1) s= "<" * s * ">";
      trl->dict (s)= i;
      trl->virt_def[i]= t[i][1];
      // cout << s << "\t" << i << "\t" << t[i][1] << "\n";
    }
  return trl;
}

/******************************************************************************
* Loading translators
******************************************************************************/

translator
load_translator (string name) {
  if (translator::instances -> contains (name))
    return translator (name);

  string s, r;
  string file_name= name * ".enc";
  if (DEBUG_STD) cout << "TeXmacs] Loading " << file_name << "\n";
  url u ("$TEXMACS_HOME_PATH/fonts/enc:$TEXMACS_PATH/fonts/enc", file_name);
  if (load_string (u, s, false)) return load_virtual (name);

  translator trl= tm_new<translator_rep> (name);
  int i, j, num=0;
  for (i=0; i<N(s); i++)
    switch (s[i]) {
    case '\"': // "
      r= "";
      for (i++; i<N(s); i++) {
	if ((s[i]=='\\') && (i<N(s)-1)) i++;
	else if (s[i]=='\"') break; // "
	r << s[i];
      }
      trl << r;
      num= 0;
      break;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      if (i==N(s)-1) break;
      num= 10*num+ ((int) s[i])- ((int) '0');
      trl << num;
      break;
    case '*':
      if (i==N(s)-1) break;
      num= 256*num;
      trl << num;
      break;
    case '[':
      i++; j=i;
      while ((i<N(s)) && (s[i]!=']')) i++;
      trl << load_translator (s (j, i));
      break;
    default:
      num= 0;
    }
  return trl;
}
