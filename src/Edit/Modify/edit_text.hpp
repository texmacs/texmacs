
/******************************************************************************
* MODULE     : edit_text.hpp
* DESCRIPTION: Main routines for the manipulation of "ordinary" text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_TEXT_H
#define EDIT_TEXT_H
#include "editor.hpp"

class edit_text_rep: virtual public editor_rep {
protected:
  void correct_concat (path p, int done=0);
  void correct (path p);
  bool pure_line (path p);
  bool accepts_return (path p);
  path prepare_for_insert ();
  void get_deletion_point (path& p, int& l, int& r, tree& t, tree& u, bool f);

public:
  edit_text_rep ();
  ~edit_text_rep ();

  /********************************* text ************************************/
  bool insert_return ();
  void remove_return (path p);
  void insert_tree (tree t, path p_in_t);
  void var_insert_tree (tree t, path p_in_t);
  void insert_tree (tree t);
  void remove_text (bool forward);
  void remove_structure (bool forward);
  void remove_structure_upwards ();

  /******************************** format ***********************************/
  void make_space (tree t);
  void make_space (string w);
  void make_space (string w, string y1, string y2);
  void make_hspace (string s);
  void make_hspace (string smin, string sdef, string smax);
  void make_vspace_before (string s);
  void make_vspace_before (string smin, string sdef, string smax);
  void make_vspace_after (string s);
  void make_vspace_after (string smin, string sdef, string smax);
  void make_htab (string spc);
  void make_image (string file_name, bool link,
		   string w, string h, string x, string y);
};

#endif // defined EDIT_TEXT_H
