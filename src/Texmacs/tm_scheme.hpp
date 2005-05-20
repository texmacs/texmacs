
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
  array<object> cmds;          // commands which are still to be executed
  window dialogue_win;         // dialogue window
  widget dialogue_wid;         // dialogue widget

public:
  tm_scheme_rep ();
  ~tm_scheme_rep ();

  bool exec_file (url u);
  void exec_delayed (object cmd);
  void exec_pending_commands ();
  string preference (string var);

  void dialogue_start (string name, widget wid);
  void dialogue_inquire (string& arg);
  void dialogue_end ();
  void choose_file (object fun, string title, string type);
  void interactive (object fun, scheme_tree p);
};

#endif // defined TM_SCHEME_H
