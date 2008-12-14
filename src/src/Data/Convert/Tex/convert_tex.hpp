
/******************************************************************************
* MODULE     : convert_tex.h
* DESCRIPTION: declaration shared among the tex conversion units
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CONVERT_TEX_H
#define CONVERT_TEX_H
#include "convert.hpp"
#include "rel_hashmap.hpp"
#include "hashfunc.hpp"

extern rel_hashmap<string,string> command_type;
extern rel_hashmap<string,int>    command_arity;
extern rel_hashmap<string,string> command_def;

string latex_type  (string cmd);
int    latex_arity (string cmd);

#endif // defined CONVERT_TEX_H
