
/******************************************************************************
* MODULE     : bridge.hpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef BRIDGE_H
#define BRIDGE_H
#include "typesetter.hpp"
#include "Concat/concater.hpp"
#include "Stack/stacker.hpp"
#include "Format/page_item.hpp"
#include "Page/vpenalty.hpp"
#include "Page/skeleton.hpp"
#include "Page/pager.hpp"

#define VALID_MASK        1
#define CORRUPTED         0
#define PROCESSED         1

#define WANTED_MASK       2
#define WANTED_PARAGRAPH  0
#define WANTED_PARUNIT    2

#define MACRO_ASSIGN      0
#define MACRO_INSERT      1
#define MACRO_REMOVE      2

class bridge;
class bridge_rep: public abstract_struct {
public:
  typesetter           ttt;     // the underlying typesetter
  edit_env&            env;     // the environment
  tree                 st;      // the present subtree
  path                 ip;      // source location of the paragraph
  int                  status;  // status among above values
  hashmap<string,tree> changes; // changes in the environment

  array<page_item>     l;       // the typesetted lines of st
  stack_border         sb;      // border properties of l

public:
  bridge_rep (typesetter ttt, tree st, path ip);
  inline virtual ~bridge_rep () {}

  virtual void notify_assign (path p, tree u) = 0;
  virtual void notify_insert (path p, tree u);
  virtual void notify_remove (path p, int nr);
  virtual void notify_split  (path p);
  virtual void notify_join   (path p);
  virtual bool notify_macro  (int type, string var, int l, path p, tree u) = 0;
  virtual void notify_change () = 0;

  virtual void my_exec_until (path p);
  virtual bool my_typeset_will_be_complete ();
  virtual void my_typeset (int desired_status);
  void exec_until (path p);
  void typeset (int desired_status);
};

class bridge {
  ABSTRACT_NULL(bridge);
  bool operator == (bridge br2);
  bool operator != (bridge br2);
  friend bridge make_bridge (typesetter ttt, tree st, path ip);
  friend void   replace_bridge (bridge& br, tree st, path ip);
};
ABSTRACT_NULL_CODE(bridge);

ostream& operator << (ostream& out, bridge br);
extern bridge nil_bridge;
tree substitute (tree t, path p, tree u);

#include "impl_typesetter.hpp"

#endif // defined BRIDGE_H
