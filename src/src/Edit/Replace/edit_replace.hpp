
/******************************************************************************
* MODULE     : edit_replace.hpp
* DESCRIPTION: the interface for TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EDIT_REPLACE_H
#define EDIT_REPLACE_H
#include "editor.hpp"

class edit_replace_rep: virtual public editor_rep {
protected:
  bool        forward;       // forward search or replace ?
  string      search_mode;   // only search matches in search_mode mode
  string      search_lan;    // only search matches in search_lan language
  path        search_at;     // current search position
  path        search_end;    // position of the end of a match
  tree        search_what;   // search tree
  list<path>  where_stack;   // last search positions
  tree        what_stack;    // last search trees
  tree        replace_by;    // replace tree
  int         nr_replaced;   // number of replaced occurrences

  path        spell_end_p;   // spell check until here
  string      spell_s;       // the word being checked
  tree        spell_t;       // response of ispell
  bool        spell_dicmod;  // insert new words in personal dictionary?

protected:
  void spell_write (string s);
  tree spell_read ();

public:
  edit_replace_rep ();
  ~edit_replace_rep ();

  /* structural searching */
  bool inside (string what);
  bool inside (tree_label l);
  bool inside_compound (string what);
  bool inside_with (string var, string val);
  string inside_which (tree t);
  path search_upwards (string what);
  path search_upwards (tree_label l);
  path search_parent_upwards (tree_label l, int& last);
  path search_parent_upwards (tree_label l);
  path search_upwards_compound (string what);
  path search_upwards_with (string var, string val);
  path search_upwards_in_set (tree t);
  path search_previous_compound (path init, string which);
  path search_next_compound (path init, string which);

  /* search and replace */
  path test_sub (path p, tree t);
  path test (path p, tree t);
  void step_ascend (bool forward);
  void step_descend (bool forward);
  void step_horizontal (bool forward);
  void next_match (bool forward);
  void search_start (bool forward= true);
  void search_next (bool forward);
  void search_next (tree what, bool forward, bool step);
  void search_stop ();
  void search_button_next ();
  bool search_keypress (string s);
  void replace_start (tree what, tree by, bool forward= true);
  void replace_next ();
  bool replace_keypress (string s);

  /* spell */
  path test_spellable (path p);
  void spell_next ();
  void spell_replace (string by);
  void spell_start ();
  void spell_end ();
  bool spell_keypress (string s);
};

#endif // defined EDIT_REPLACE_H
