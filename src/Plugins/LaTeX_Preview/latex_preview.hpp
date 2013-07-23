
/******************************************************************************
* MODULE     : latex_preview.hpp
* DESCRIPTION: generating pictures using LaTeX with preview package
* COPYRIGHT  : (C) 2013  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LATEX_PREVIEW_H
#define LATEX_PREVIEW_H

#include "array.hpp"

array<tree> latex_preview (string s, tree t);
void set_latex_command (string cmd);

#endif // LATEX_PREVIEW_H
