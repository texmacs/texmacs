
/******************************************************************************
* MODULE     : tree_analyze.hpp
* DESCRIPTION: routines for analyzing trees
* COPYRIGHT  : (C) 2010  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_ANALYZE_H
#define TREE_ANALYZE_H
#include "drd_std.hpp"
#include "language.hpp"
#include "analyze.hpp"
#include "vars.hpp"

/******************************************************************************
* Concatenations and with-like structures
******************************************************************************/

array<tree> concat_tokenize (tree t);
array<tree> concat_decompose (tree t);
tree concat_recompose (array<tree> a);

bool is_with_like (tree t);
tree& with_body (tree w);
bool with_same_type (tree w1, tree w2);
bool with_similar_type (tree w1, tree w2);
array<tree> with_decompose (tree w, tree t);
tree with_recompose (tree w, array<tree> a);

/******************************************************************************
* Symbol types
******************************************************************************/

#define SYMBOL_DELETED           -1
#define SYMBOL_BASIC              0
#define SYMBOL_PREFIX             1
#define SYMBOL_POSTFIX            2
#define SYMBOL_INFIX              3
#define SYMBOL_PREFIX_INFIX       4
#define SYMBOL_SEPARATOR          5
#define SYMBOL_SKIP               6
#define SYMBOL_SCRIPT             7
#define SYMBOL_OPEN_BIG           8
#define SYMBOL_CLOSE_BIG          9
#define SYMBOL_OPEN              10
#define SYMBOL_MIDDLE            11
#define SYMBOL_CLOSE             12
#define SYMBOL_PROBABLE_OPEN     13
#define SYMBOL_PROBABLE_MIDDLE   14
#define SYMBOL_PROBABLE_CLOSE    15
#define SYMBOL_DUBIOUS_OPEN      16
#define SYMBOL_DUBIOUS_MIDDLE    17
#define SYMBOL_DUBIOUS_CLOSE     18

int symbol_type (tree t);
array<int> symbol_types (array<tree> a);

/******************************************************************************
* Symbol priorities
******************************************************************************/

#define PRIORITY_SEPARATOR          0
#define PRIORITY_ASSIGN             1
#define PRIORITY_FLUX               2
#define PRIORITY_MODELS             3
#define PRIORITY_IMPLY              4
#define PRIORITY_OR                 5
#define PRIORITY_AND                6
#define PRIORITY_RELATION           7
#define PRIORITY_ARROW              8
#define PRIORITY_UNION              9
#define PRIORITY_INTERSECTION      10
#define PRIORITY_PLUS              11
#define PRIORITY_TIMES             12
#define PRIORITY_POWER             13
#define PRIORITY_RADICAL           14

int symbol_priority (tree t);
array<int> symbol_priorities (array<tree> a);

/******************************************************************************
* DRD-based
******************************************************************************/

drd_info get_document_drd (tree doc);
bool is_correctable_child (tree t, int i, bool noaround= false);

#endif // defined TREE_ANALYZE_H
