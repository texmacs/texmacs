
/******************************************************************************
* MODULE     : convert_tex.h
* DESCRIPTION: declaration shared among the tex conversion units
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef CONVERT_TEX_H
#define CONVERT_TEX_H
#include "convert.hpp"
#include "rel_hashmap.hpp"
#include "hashfunc.hpp"

extern rel_hashmap<string,string> command_type;
extern rel_hashmap<string,int>    command_arity;
extern rel_hashmap<string,string> command_def;
extern hashfunc<string,int>       latex_arity;
extern hashfunc<string,string>    latex_type;

#endif // defined CONVERT_TEX_H
