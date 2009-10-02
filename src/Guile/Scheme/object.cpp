
/******************************************************************************
* MODULE     : object.cpp
* DESCRIPTION: Scheme objects
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Glue/glue.hpp"
#include "guile.hpp"
#include "Scheme/evaluate.hpp"
#include "Scheme/object.hpp"
#include "list.hpp"
#include "array.hpp"
#include "promise.hpp"
#include "widget.hpp"

/******************************************************************************
* The object representation class
******************************************************************************/

static list<SCM> destroy_list;

object_rep::object_rep (SCM obj) {
  while (!is_nil (destroy_list)) {
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
  else FAILED ("not yet implemented");
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
object list_object (object obj1) {
  return cons (obj1, null_object ()); }
object list_object (object obj1, object obj2) {
  return cons (obj1, cons (obj2, null_object ())); }
object list_object (object obj1, object obj2, object obj3) {
  return cons (obj1, cons (obj2, cons (obj3, null_object ()))); }
object symbol_object (string s) {
  return object (symbol_to_scm (s)); }
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
bool is_symbol (object obj) { return scm_is_symbol (obj->lookup()); }
bool is_tree (object obj) { return scm_is_tree (obj->lookup()); }
bool is_path (object obj) { return scm_is_path (obj->lookup()); }
bool is_url (object obj) { return scm_is_url (obj->lookup()); }

/******************************************************************************
* Basic conversions
******************************************************************************/

object::object (): rep (tm_new<object_rep> (SCM_NULL)) {}
object::object (bool b): rep (tm_new<object_rep> (bool_to_scm (b))) {}
object::object (int i): rep (tm_new<object_rep> (int_to_scm (i))) {}
object::object (const char* s):
  rep (tm_new<object_rep> (string_to_scm (string (s)))) {}
object::object (string s): rep (tm_new<object_rep> (string_to_scm (s))) {}
object::object (tree t): rep (tm_new<object_rep> (tree_to_scm (t))) {}
object::object (list<string> l): rep (tm_new<object_rep> (list_string_to_scm(l))) {}
object::object (list<tree> l): rep (tm_new<object_rep> (list_tree_to_scm (l))) {}
object::object (path p): rep (tm_new<object_rep> (path_to_scm (p))) {}
object::object (url u): rep (tm_new<object_rep> (url_to_scm (u))) {}

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

string
as_symbol (object obj) {
  SCM s= obj->lookup();
  if (!scm_is_symbol (s)) return "";
  return scm_to_symbol (s);
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

list<string>
as_list_string (object obj) {
  SCM l= obj->lookup();
  if (!scm_is_list_string (l)) return list<string> ();
  return scm_to_list_string (l);
}

list<tree>
as_list_tree (object obj) {
  SCM l= obj->lookup();
  if (!scm_is_list_tree (l)) return list<tree> ();
  return scm_to_list_tree (l);
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
scheme_cmd (const char* s) {
  return eval ("(lambda () " * string (s) * ")");
}

object
scheme_cmd (string s) {
  return eval ("(lambda () " * s * ")");
}

object
scheme_cmd (object cmd) {
  cmd= cons (cmd, null_object ());
  cmd= cons (null_object (), cmd);
  cmd= cons (eval ("'lambda"), cmd);
  return eval (cmd);
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
  return tm_new<object_command_rep> (obj);
}

class object_promise_widget_rep: public promise_rep<widget> {
  object obj;
public:
  object_promise_widget_rep (object obj2): obj (obj2) {}
  ostream& print (ostream& out) { return out << obj; }
  widget eval () {
    SCM result= call_scheme (obj->lookup ());
    if (scm_is_widget (result)) return scm_to_widget (result);
    else {
      FAILED ("widget expected");
      return glue_widget ();
    }
  }
};

promise<widget>
as_promise_widget (object obj) {
  return tm_new<object_promise_widget_rep> (obj);
}

/******************************************************************************
* Evaluation and function calls
******************************************************************************/

object eval (const char* expr) {
  return object (eval_scheme (expr)); }
object eval (string expr) {
  return object (eval_scheme (expr)); }
object eval (object expr) {
  return call ("eval", expr); }
object eval_secure (string expr) {
  return eval ("(wrap-eval-secure " * expr * ")"); }
object eval_file (string name) {
  return object (eval_scheme_file (name)); }
bool exec_file (url u) {
  object ret= eval_file (materialize (u));
  return ret != object ("#<unspecified>"); }

static inline array<SCM>
array_lookup (array<object> a) {
  const int n=N(a);
  array<SCM> scm(n);
  int i;
  for (i=0; i<n; i++) scm[i]= a[i]->lookup();
  return scm;
}

object call (const char* fun) {
  return object (call_scheme (eval_scheme(fun))); }
object call (const char* fun, object a1) {
  return object (call_scheme (eval_scheme(fun), a1->lookup())); }
object call (const char* fun, object a1, object a2) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(), a2->lookup())); }
object call (const char* fun, object a1, object a2, object a3) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(),
			      a2->lookup(), a3->lookup())); }
object call (const char* fun, object a1, object a2, object a3, object a4) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(),
			      a2->lookup(), a3->lookup(), a4->lookup())); }
object call (const char* fun, array<object> a) {
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
object call (string fun, object a1, object a2, object a3, object a4) {
  return object (call_scheme (eval_scheme(fun), a1->lookup(),
			      a2->lookup(), a3->lookup(), a4->lookup())); }
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
object call (object fun, object a1, object a2, object a3, object a4) {
  return object (call_scheme (fun->lookup(), a1->lookup(),
			      a2->lookup(), a3->lookup(), a4->lookup())); }
object call (object fun, array<object> a) {
  return object (call_scheme (fun->lookup(), array_lookup(a))); }

/******************************************************************************
* Delayed evaluation
******************************************************************************/

static array<object> delayed_queue;
static array<object> delayed_pause_queue;
static array<int>    start_time_queue;

void
exec_delayed (object cmd) {
  delayed_queue << cmd;
}

void
exec_delayed_pause (object cmd) {
  delayed_pause_queue << cmd;
  start_time_queue << ((int) texmacs_time ());
}

void
exec_pending_commands () {
  {
    array<object> a= delayed_queue;
    delayed_queue= array<object> (0);
    int i, n= N(a);
    for (i=0; i<n; i++) {
      object obj= call (a[i]);
      if (is_bool (obj) && !as_bool (obj))
	delayed_queue << a[i];
    }
  }

  {
    array<object> a= delayed_pause_queue;
    array<int> b= start_time_queue;
    delayed_pause_queue= array<object> (0);
    start_time_queue= array<int> (0);
    int i, n= N(a);
    for (i=0; i<n; i++) {
      int now= (int) texmacs_time ();
      if (now >= b[i]) {
	object obj= call (a[i]);
	if (is_int (obj)) {
	  //cout << "pause= " << obj << "\n";
	  delayed_pause_queue << a[i];
	  start_time_queue << (now + as_int (obj));
	}
      }
      else {
	delayed_pause_queue << a[i];
	start_time_queue << b[i];
      }
    }
  }
}
