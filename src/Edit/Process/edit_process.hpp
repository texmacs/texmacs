
/******************************************************************************
* MODULE     : edit_process.hpp
* DESCRIPTION: Interface for automatically generated content
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_PROCESS_H
#define EDIT_PROCESS_H
#include "editor.hpp"

class edit_process_rep: virtual public editor_rep {
public:
  edit_process_rep ();
  ~edit_process_rep ();

  void generate_bibliography (string bib, string style, string fname);
  void generate_table_of_contents (string toc);
  void generate_index (string idx);
  void generate_glossary (string glo);
  void generate_aux (string which= "");
  bool get_save_aux ();

private:
  void generate_aux_recursively (string which, tree tt, path ip);
};

#endif // defined EDIT_PROCESS_H
