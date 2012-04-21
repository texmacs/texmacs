
/******************************************************************************
* MODULE     : new_project.hpp
* DESCRIPTION: Project management
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_PROJECT_H
#define NEW_PROJECT_H
#include "tree.hpp"
#include "url.hpp"

void project_attach (string prj_name= "");
bool project_attached ();
url  project_get ();

#endif // defined NEW_PROJECT_H
