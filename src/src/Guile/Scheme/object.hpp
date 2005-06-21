
/******************************************************************************
* MODULE     : object.hpp
* DESCRIPTION: Scheme objects
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef OBJECT_H
#define OBJECT_H
#include "tree.hpp"
#include "path.hpp"
#include "command.hpp"
#include "url.hpp"
#include <libguile.h>

class object_rep: concrete_struct {
  SCM handle;
public:
  object_rep (SCM obj);
  ~object_rep ();
  SCM lookup ();
  friend struct object;
};

struct object {
  CONCRETE(object);
  inline object (SCM obj): rep (new object_rep (obj)) {}
  object ();
  object (bool b);
  object (int i);
  object (char* s);
  object (string s);
  object (tree t);
  object (path p);
  object (url u);
};
CONCRETE_CODE(object);

ostream& operator << (ostream& out, object obj);
bool operator == (object obj1, object obj2);
bool operator != (object obj1, object obj2);

object null_object ();
object cons (object obj1, object obj2);
object car (object obj);
object cdr (object obj);
object caar (object obj);
object cdar (object obj);
object cadr (object obj);
object cddr (object obj);
object caddr (object obj);
object cadddr (object obj);

bool is_null (object obj);
bool is_list (object obj);
bool is_bool (object obj);
bool is_int (object obj);
bool is_string (object obj);
bool is_tree (object obj);
bool is_path (object obj);
bool is_url (object obj);

bool as_bool (object obj);
int as_int (object obj);
string as_string (object obj);
tree as_tree (object obj);
scheme_tree as_scheme_tree (object obj);
path as_path (object obj);
url as_url (object obj);
command as_command (object obj);
#ifdef WIDGET_H // FIXME: dirty hack
#include "Widget/make_widget.hpp"
widget as_widget (object obj);
make_widget as_make_widget (object obj);
#endif

object tree_to_stree (tree t);
tree   stree_to_tree (object obj);
tree   content_to_tree (object obj);
object string_to_object (string s);
string object_to_string (object obj);
object scheme_cmd (char* s);
object scheme_cmd (string s);
object scheme_cmd (object cmd);

object eval (char* expr);
object eval (string expr);
object eval (object expr);
object eval_secure (string expr);
object eval_file (string name);
void   eval_delayed (string expr);

object call (char* fun);
object call (char* fun, object a1);
object call (char* fun, object a1, object a2);
object call (char* fun, object a1, object a2, object a3);
object call (char* fun, array<object> a);
object call (string fun);
object call (string fun, object a1);
object call (string fun, object a1, object a2);
object call (string fun, object a1, object a2, object a3);
object call (string fun, array<object> a);
object call (object fun);
object call (object fun, object a1);
object call (object fun, object a1, object a2);
object call (object fun, object a1, object a2, object a3);
object call (object fun, array<object> a);

#endif // defined OBJECT_H
