
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

array<tree> collect_metadata_latex (tree t);
tree        collect_metadata_acm (tree t);
tree        collect_metadata_ams (tree t);
tree        collect_metadata_elsevier (tree t);
tree        collect_metadata_revtex (tree t);
tree        collect_metadata_svmono (tree t);
tree        collect_metadata_springer (tree t);
tree        collect_metadata (tree t, tree latex_classe);
bool        is_metadata (tree u);
bool        is_metadata_env (tree u);

#endif // defined METADATA_H
