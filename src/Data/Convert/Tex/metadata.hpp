
/******************************************************************************
* MODULE     : metadata.hpp
* DESCRIPTION: import of metadata for various LaTeX styles
* COPYRIGHT  : (C) 2013  Joris van der Hoeven, François Poulain
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Tex/convert_tex.hpp"

#ifndef METADATA_H
#define METADATA_H

tree        collect_abstract_data (tree u);
array<tree> collect_metadata_latex (tree t, array<tree>(*get_author_datas)(tree));
array<tree> collect_metadata_latex (tree t);
tree        collect_metadata_acm_old (tree t);
tree        collect_metadata_acm (tree t);
tree        collect_metadata_ams (tree t);
tree        collect_metadata_elsevier (tree t);
tree        collect_metadata_revtex (tree t);
tree        collect_metadata_svmono (tree t);
tree        collect_metadata_springer (tree t, bool llncs= false);
tree        collect_metadata_ieee (tree t);
tree        collect_metadata (tree t, tree latex_class);
bool        is_metadata (tree u);
bool        is_metadata_env (tree u);
tree        filter_spaces (tree t, bool &spaced);
array<tree> filter_spaces (array<tree> a, bool &spaced);

/******************************************************************************
* Usefull APPLY tag
******************************************************************************/

inline bool is_apply (tree t) {
  return (L(t) == APPLY); }
inline bool is_apply (tree t, string s) {
  return (L(t) == APPLY) && (N(t) >= 1) && (t[0] == s); }
inline bool is_apply (tree t, const char* s) {
  return (L(t) == APPLY) && (N(t) >= 1) && (t[0] == s); }
inline bool is_apply (tree t, string s, int n) {
  return (L(t) == APPLY) && (N(t) == (n+1)) && (t[0] == s); }
inline bool is_apply (tree t, const char* s, int n) {
  return (L(t) == APPLY) && (N(t) == (n+1)) && (t[0] == s); }

#endif // defined METADATA_H
