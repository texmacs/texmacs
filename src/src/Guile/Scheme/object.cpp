
/******************************************************************************
* MODULE     : object.cpp
* DESCRIPTION: Scheme objects
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "guile.hpp"
#include "Glue/glue.hpp"
#include "Scheme/evaluate.hpp"
#include "Scheme/object.hpp"
#include "list.hpp"
#include "array.hpp"

/******************************************************************************
* The object representation class
******************************************************************************/

static list<SCM> destroy_list;

object_rep::object_rep (SCM obj) {
  while (!nil (destroy_list)) {
    SCM handle= destroy_list->item;
    SCM_SETCAR (handle, scm_listify (SCM_UNDEFINED));
    while (scm_is_pair (SCM_CDR (handle)) && scm_is_null (SCM_CADR (handle)))
      SCM_SETCDR (handle, SCM_CDDR (handle));
    destroy_list= destroy_list->next;
  }
  handle= scm_cons (scm_listify (obj, SCM_UNDEFINED), SCM_CAR (object_stack));
  SCM_SETCAR (object_stack, handle);
}

object_rep::~object_rep () {
  // Be careful: can't call Scheme code from this destructor,
  // because the destructor can be called during garbage collection.
  destroy_list= list<SCM> (handle, destroy_list);
}

SCM
object_rep::lookup () {
  return SCM_CAAR (handle);
}

/******************************************************************************
* Routines on objects
******************************************************************************/

ostream&
operator << (ostream& out, object obj) {
  out.flush ();
  if (out == cout) call ("write", obj);
  else if (out == cerr) call ("write-err", obj);
  else fatal_error ("Not yet implemented", "object::operator <<", "object.cpp");
  call ("force-output");
  return out;
}

bool
operator == (object obj1, object obj2) {
  SCM o1= obj1->lookup (), o2= obj2->lookup ();
  return SCM_NFALSEP (scm_equal_p (o1, o2));
}

bool
operator != (object obj1, object obj2) {
  return !(obj1 == obj2);
}

/******************************************************************************
* Utilities
******************************************************************************/

object null_object () {
  return object (SCM_NULL); }
object cons (object obj1, object obj2) {
  return object (scm_cons (obj1->lookup(), obj2->lookup())); }
object car (object obj) {
  return object (SCM_CAR (obj->lookup())); }
object cdr (object obj) {
  return object (SCM_CDR (obj->lookup())); }
object caar (object obj) {
  return object (SCM_CAAR (obj->lookup())); }
object cdar (object obj) {
  return object (SCM_CDAR (obj->lookup())); }
object cadr (object obj) {
  return object (SCM_CADR (obj->lookup())); }
object cddr (object obj) {
  return object (SCM_CDDR (obj->lookup())); }
object caddr (object obj) {
  return object (SCM_CADDR (obj->lookup())); }
object cadddr (object obj) {
  return object (SCM_CADDDR (obj->lookup())); }

/******************************************************************************
* Predicates
******************************************************************************/

bool is_null (object obj) { return scm_is_null (obj->lookup()); }
bool is_list (object obj) { return scm_is_list (obj->lookup()); }
bool is_bool (object obj) { return scm_is_bool (obj->lookup()); }
bool is_int (object obj) { return scm_is_int (obj->lookup()); }
bool is_string (object obj) { return scm_is_string (obj->lookup()); }
bool is_tree (object obj) { return scm_is_tree (obj->lookup()); }
bool is_path (object obj) { return scm_is_path (obj->lookup()); }
bool is_url (object obj) { return scm_is_url (obj->lookup()); }

/******************************************************************************
* Basic conversions
******************************************************************************/

object::object (): rep (new object_rep (SCM_NULL)) {}
object::object (bool b): rep (new object_rep (bool_to_scm (b))) {}
object::object (int i): rep (new object_rep (int_to_scm (i))) {}
object::object (string s): rep (new object_rep (string_to_scm (s))) {}
object::object (tree t): rep (new object_rep (tree_to_scm (t))) {}
object::object (path p): rep (new object_rep (path_to_scm (p))) {}
object::object (url u): rep (new object_rep (url_to_scm (u))) {}

bool
as_bool (object obj) {
  SCM b= obj->lookup();
  if (!scm_is_bool (b)) return false;
  return scm_to_bool (b);
}

int
as_int (object obj) {
  SCM i= obj->lookup();
  if (!scm_is_int (i)) return 0;
  return scm_to_int (i);
}

string
as_string (object obj) {
  SCM s= obj->lookup();
  if (!scm_is_string (s)) return "";
  return scm_to_string (s);
}

tree
as_tree (object obj) {
  SCM t= obj->lookup();
  if (!scm_is_tree (t)) return tree ();
  return scm_to_tree (t);
}

scheme_tree
as_scheme_tree (object obj) {
  SCM t= obj->lookup();
  return scm_to_scheme_tree (t);
}

path
as_path (object obj) {
  SCM t= obj->lookup();
  if (!scm_is_path (t)) return path ();
  return scm_to_path (t);
}

url
as_url (object obj) {
  SCM t= obj->lookup();
  if (!scm_is_url (t)) return url ("");
  return scm_to_url (t);
}

widget
as_widget (object obj) {
  SCM w= obj->lookup();
  if (!scm_is_widget (w)) return widget ();
  return scm_to_widget (w);
}

object
tree_to_stree (scheme_tree t) {
  return call ("tree->stree", t);
}

tree
stree_to_tree (object obj) {
  return as_tree (call ("stree->tree", obj));
}

tree
content_to_tree (object obj) {
  return scm_to_content (obj->lookup());
  // return as_tree (call ("content->tree", obj));
}

object
string_to_object (string s) {
  return call ("string->object", s);
}

string
object_to_string (object obj) {
  return as_string (call ("object->string", obj));
}

object
scheme_cmd (string s) {
  return eval ("(lambda () " * s * ")");
}

/******************************************************************************
* Conversions to functional objects
******************************************************************************/

class object_command_rep: public command_rep {
  object obj;
public:
  object_command_rep (object obj2): obj (obj2) {}
  void apply () { (void) call_scheme (obj->lookup ()); }
  ostream& print (ostream& out) { return out << obj; }
};

command
as_command (object obj) {
  return new object_command_rep (obj);
}

class object_make_widget_rep: public make_widget_rep {
  object obj;
public:
  object_make_widget_rep (object obj2): obj (obj2) {}
  ostream& print (ostream& out) { return out << obj; }
  widget get_widget (display dis) { (void) dis;
    SCM result= call_scheme (obj->lookup ());
    if (scm_is_widget (result)) return scm_to_widget (result);
    else {
      fatal_error ("Widget expected", "eval_as_widget", "object.cpp");
      return glue_widget ();
    }
  }
};

make_widget
as_make_widget (object obj) {
  return new object_make_widget_rep (obj);
}

/******************************************************************************
* Evaluation and function calls
******************************************************************************/

object eval (char* expr) {
  return object (eval_scheme (expr)); }
object eval (string expr) {
  return object (eval_scheme (expr)); }
object eval (object expr) {
  return call ("eval", expr); }
object eval_secure (string expr) {
  return eval ("(wrap-eval-secure " * expr * ")"); }
object eval_file (string name) {
  return object (eval_scheme_file (name)); }
void eval_delayed (string expr) {
  (void) call ("exec-delayed", scheme_cmd (expr)); }

static inline array<SCM>
array_lookup (array<object> a) {
  const int n=N(a);
  array<SCM> scm(n);
  int i;
  for (i=0; i<n; i++) scm[i]= a[i]->lookup();
  return scm;
}

object call (char* fun) {
  return object (call_scheme (eval_scheme(fun))); }
object call (char* fun, object a1) {
  return object (call_scheme (eval_scheme(fun), a1->lookup())); }
object call (char* fun, object a1, object a2) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(), a2->lookup())); }
object call (char* fun, object a1, object a2, object a3) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(),
			      a2->lookup(), a3->lookup())); }
object call (char* fun, array<object> a) {
  return object (call_scheme (eval_scheme(fun), array_lookup(a))); }

object call (string fun) {
  return object (call_scheme (eval_scheme(fun))); }
object call (string fun, object a1) {
  return object (call_scheme (eval_scheme(fun), a1->lookup())); }
object call (string fun, object a1, object a2) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(), a2->lookup())); }
object call (string fun, object a1, object a2, object a3) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(),
			      a2->lookup(), a3->lookup())); }
object call (string fun, array<object> a) {
  return object (call_scheme (eval_scheme(fun), array_lookup(a))); }

object call (object fun) {
  return object (call_scheme (fun->lookup())); }
object call (object fun, object a1) {
  return object (call_scheme (fun->lookup(), a1->lookup())); }
object call (object fun, object a1, object a2) {
  return object (call_scheme (fun->lookup(), a1->lookup(), a2->lookup())); }
object call (object fun, object a1, object a2, object a3) {
  return object (call_scheme (fun->lookup(), a1->lookup(),
			      a2->lookup(), a3->lookup())); }
object call (object fun, array<object> a) {
  return object (call_scheme (fun->lookup(), array_lookup(a))); }
