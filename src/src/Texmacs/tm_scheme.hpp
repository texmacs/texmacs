
/******************************************************************************
* MODULE     : tm_scheme.hpp
* DESCRIPTION: TeXmacs-lisp motor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_SCHEME_H
#define TM_SCHEME_H
#include "server.hpp"

class tm_scheme_rep: virtual public server_rep {
protected:
  array<command> cmds;         // commands which are still to be executed
  window dialogue_win;         // dialogue window
  widget dialogue_wid;         // dialogue widget

public:
  tm_scheme_rep ();
  ~tm_scheme_rep ();

  bool exec_file (url u);
  void exec_delayed (string s);
  void exec_delayed (command cmd);
  void exec_pending_commands ();
  string preference (string var);

  void dialogue_start (string name, widget wid, scheme_tree prg);
  void dialogue_inquire (scheme_tree& arg);
  void dialogue_end ();
  void choose_file (string title, string type, scheme_tree prg);
};

#endif // defined TM_SCHEME_H
