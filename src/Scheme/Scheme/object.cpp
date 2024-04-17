
/******************************************************************************
* MODULE     : object.cpp
* DESCRIPTION: Implementation of scheme objects
* COPYRIGHT  : (C) 1999-2011 Joris van der Hoeven and Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "object.hpp"
#include "glue.hpp"

#include "config.h"
#include "list.hpp"
#include "array.hpp"
#include "promise.hpp"
#include "widget.hpp"
#include "boot.hpp"
#include "editor.hpp"
#include "modification.hpp"
#include "patch.hpp"

/******************************************************************************
* The object representation class
******************************************************************************/

static list<tmscm > destroy_list;
extern tmscm object_stack;

tmscm_object_rep::tmscm_object_rep (tmscm obj) {
  while (!is_nil (destroy_list)) {
    tmscm handle= destroy_list->item;
    
    tmscm_set_car (handle, tmscm_null ());
    while (tmscm_is_pair (tmscm_cdr (handle)) && tmscm_is_null (tmscm_cadr (handle)))
      tmscm_set_cdr (handle, tmscm_cddr( (handle)) );
    destroy_list= destroy_list->next;
  }
  handle = tmscm_cons ( tmscm_cons (obj, tmscm_null ()), tmscm_car (object_stack) );
  tmscm_set_car (object_stack, handle);
}

tmscm_object_rep::~tmscm_object_rep () {
    // Be careful: can't call Scheme code from this destructor,
    // because the destructor can be called during garbage collection.
  destroy_list= list<tmscm > ( handle, destroy_list);
}


/******************************************************************************
* Routines on objects
******************************************************************************/

tm_ostream&
operator << (tm_ostream& out, object obj) {
  out.flush ();
  if (out == cout) call ("write", obj);
  else if (out == cerr) call ("write-err", obj);
  else {
    object ret= call ("object->string", obj);
    return out << as_string (ret);
  }
  call ("force-output");
  return out;
}

bool
operator == (object obj1, object obj2) {
  tmscm o1= object_to_tmscm (obj1), o2= object_to_tmscm (obj2);
  return tmscm_is_equal (o1, o2);
}

bool
operator != (object obj1, object obj2) {
  return !(obj1 == obj2);
}

int
hash (object obj) {
  return as_int (call ("hash", obj, object (1234567)));
}


/******************************************************************************
* Utilities
******************************************************************************/

object null_object () {
  return tmscm_to_object (tmscm_null ()); }
object cons (object obj1, object obj2) {
  return tmscm_to_object (tmscm_cons (object_to_tmscm (obj1), object_to_tmscm (obj2))); }
object list_object (object obj1) {
  return cons (obj1, null_object ()); }
object list_object (object obj1, object obj2) {
  return cons (obj1, cons (obj2, null_object ())); }
object list_object (object obj1, object obj2, object obj3) {
  return cons (obj1, cons (obj2, cons (obj3, null_object ()))); }
object as_list_object (array<object> objs) {
  object r= null_object ();
  for (int i=N(objs)-1; i>=0; i--) r= cons (objs[i], r);
  return r; }
object symbol_object (string s) {
  return tmscm_to_object ( symbol_to_tmscm (s) ); }
object car (object obj) {
  return tmscm_to_object (tmscm_car (object_to_tmscm (obj))); }
object cdr (object obj) {
  return tmscm_to_object (tmscm_cdr (object_to_tmscm (obj))); }
object caar (object obj) {
  return tmscm_to_object (tmscm_caar (object_to_tmscm (obj))); }
object cdar (object obj) {
  return tmscm_to_object (tmscm_cdar (object_to_tmscm (obj))); }
object cadr (object obj) {
  return tmscm_to_object (tmscm_cadr (object_to_tmscm (obj))); }
object cddr (object obj) {
  return tmscm_to_object (tmscm_cddr (object_to_tmscm (obj))); }
object caddr (object obj) {
  return tmscm_to_object (tmscm_caddr (object_to_tmscm (obj))); }
object cadddr (object obj) {
  return tmscm_to_object (tmscm_cadddr (object_to_tmscm (obj))); }


/******************************************************************************
* Predicates
******************************************************************************/

bool is_null (object obj) { return tmscm_is_null (object_to_tmscm (obj)); }
bool is_list (object obj) { return tmscm_is_list (object_to_tmscm (obj)); }
bool is_bool (object obj) { return tmscm_is_bool (object_to_tmscm (obj)); }
bool is_int (object obj) { return tmscm_is_int (object_to_tmscm (obj)); }
bool is_double (object obj) { return tmscm_is_double (object_to_tmscm (obj)); }
bool is_string (object obj) { return tmscm_is_string (object_to_tmscm (obj)); }
bool is_symbol (object obj) { return tmscm_is_symbol (object_to_tmscm (obj)); }
bool is_tree (object obj) { return tmscm_is_tree (object_to_tmscm (obj)); }
bool is_path (object obj) { return tmscm_is_path (object_to_tmscm (obj)); }
bool is_url (object obj) { return tmscm_is_url (object_to_tmscm (obj)); }
bool is_array_double (object obj) {
  return tmscm_is_array_double (object_to_tmscm (obj)); }
bool is_widget (object obj) { return tmscm_is_widget (object_to_tmscm (obj)); }
bool is_patch (object obj) { return tmscm_is_patch (object_to_tmscm (obj)); }
bool is_modification (object obj) {
  return tmscm_is_modification (object_to_tmscm (obj)); }

/******************************************************************************
* Basic conversions
******************************************************************************/

object::object (tmscm_object_rep* o): rep (static_cast<object_rep*>(o)) {}
object::object (): rep (tm_new<tmscm_object_rep> (tmscm_null ())) {}
object::object (bool b): rep (tm_new<tmscm_object_rep> (bool_to_tmscm (b))) {}
object::object (int i): rep (tm_new<tmscm_object_rep> (int_to_tmscm (i))) {}
object::object (double x):
  rep (tm_new<tmscm_object_rep> (double_to_tmscm (x))) {}
object::object (const char* s):
  rep (tm_new<tmscm_object_rep> (string_to_tmscm (string (s)))) {}
object::object (string s):
  rep (tm_new<tmscm_object_rep> (string_to_tmscm (s))) {}
object::object (tree t):
  rep (tm_new<tmscm_object_rep> (tree_to_tmscm (t))) {}
object::object (list<string> l):
  rep (tm_new<tmscm_object_rep> (list_string_to_tmscm (l))) {}
object::object (list<tree> l):
  rep (tm_new<tmscm_object_rep> (list_tree_to_tmscm (l))) {}
object::object (path p): rep (tm_new<tmscm_object_rep> (path_to_tmscm (p))) {}
object::object (url u): rep (tm_new<tmscm_object_rep> (url_to_tmscm (u))) {}
object::object (array<double> a):
  rep (tm_new<tmscm_object_rep> (array_double_to_tmscm (a))) {}
object::object (patch m):
  rep (tm_new<tmscm_object_rep> (patch_to_tmscm (m))) {}
object::object (modification m):
  rep (tm_new<tmscm_object_rep> (modification_to_tmscm (m))) {}

bool
as_bool (object obj) {
  tmscm b= object_to_tmscm (obj);
  if (!tmscm_is_bool (b)) return false;
  return tmscm_to_bool (b);
}

int
as_int (object obj) {
  tmscm i= object_to_tmscm (obj);
  if (!tmscm_is_int (i)) return 0;
  return tmscm_to_int (i);
}

double
as_double (object obj) {
  tmscm x= object_to_tmscm (obj);
  if (!tmscm_is_double (x)) return 0.0;
  return tmscm_to_double (x);
}

string
as_string (object obj) {
  tmscm s= object_to_tmscm (obj);
  if (!tmscm_is_string (s)) return "";
  return tmscm_to_string (s);
}

string
as_symbol (object obj) {
  tmscm s= object_to_tmscm (obj);
  if (!tmscm_is_symbol (s)) return "";
  return tmscm_to_symbol (s);
}

tree
as_tree (object obj) {
  tmscm t= object_to_tmscm (obj);
  if (!tmscm_is_tree (t)) return tree ();
  return tmscm_to_tree (t);
}

scheme_tree
as_tmscm_tree (object obj) {
  tmscm t= object_to_tmscm (obj);
  return tmscm_to_scheme_tree (t);
}

list<string>
as_list_string (object obj) {
  tmscm l= object_to_tmscm (obj);
  if (!tmscm_is_list_string (l)) return list<string> ();
  return tmscm_to_list_string (l);
}

list<tree>
as_list_tree (object obj) {
  tmscm l= object_to_tmscm (obj);
  if (!tmscm_is_list_tree (l)) return list<tree> ();
  return tmscm_to_list_tree (l);
}

path
as_path (object obj) {
  tmscm t= object_to_tmscm (obj);
  if (!tmscm_is_path (t)) return path ();
  return tmscm_to_path (t);
}

array<object>
as_array_object (object obj) {
  ASSERT (is_list (obj), "list expected");
  array<object> ret;
  while (!is_null (obj)) {
    ret << car (obj);
    obj= cdr (obj);
  }
  return ret;
}

url
as_url (object obj) {
  tmscm t= object_to_tmscm (obj);
  if (!tmscm_is_url (t)) return url ("");
  return tmscm_to_url (t);
}

array<double>
as_array_double (object obj) {
  ASSERT (is_array_double (obj), "array<double> expected");
  tmscm t= object_to_tmscm (obj);
  return tmscm_to_array_double (t);
}

modification
as_modification (object obj) {
  tmscm m= object_to_tmscm (obj);
  if (!tmscm_is_modification (m))
    return mod_assign (path (), "");
  return tmscm_to_modification (m);
}

patch
as_patch (object obj) {
  tmscm p= object_to_tmscm (obj);
  if (!tmscm_is_patch (p))
    return patch (array<patch> ());
  return tmscm_to_patch (p);
}

widget
as_widget (object obj) {
  tmscm w= object_to_tmscm (obj);
  if (!tmscm_is_widget (w)) return widget ();
  return tmscm_to_widget (w);
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
  return tmscm_to_content (object_to_tmscm (obj));
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

static inline array<tmscm >
array_lookup (array<object> a) {
  const int n=N(a);
  array<tmscm > tmscm (n);
  int i;
  for (i=0; i<n; i++) tmscm [i]= object_to_tmscm (a[i]);
  return tmscm ;
}

class object_command_rep: public command_rep {
  object obj;
public:
  object_command_rep (object obj2): obj (obj2) {}
  void apply () { (void) call_scheme (object_to_tmscm (obj)); }
  void apply (object args) {
    (void) call_scheme (object_to_tmscm (obj),
                        array_lookup (as_array_object (args))); }
  tm_ostream& print (tm_ostream& out) {
    object bis= call ("sourcify", obj);
    return out << "<command " << bis << ">"; }
};

command
as_command (object obj) {
  return tm_new<object_command_rep> (obj);
}

class object_promise_widget_rep: public promise_rep<widget> {
  object obj;
public:
  object_promise_widget_rep (object obj2): obj (obj2) {}
  tm_ostream& print (tm_ostream& out) { return out << obj; }
  widget eval () {
    tmscm result= call_scheme (object_to_tmscm (obj));
    if (tmscm_is_widget (result)) return tmscm_to_widget (result);
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
  return tmscm_to_object (eval_scheme (expr)); }
object eval (string expr) {
  return tmscm_to_object (eval_scheme (expr)); }
object eval (object expr) {
  return call ("tm-eval", expr); }
object eval_secure (string expr) {
  return eval ("(wrap-eval-secure " * expr * ")"); }
object eval_file (string name) {
  return tmscm_to_object (eval_scheme_file (name)); }
bool exec_file (url u) {
  object ret= eval_file (materialize (u));
  return ret != object ("#<unspecified>"); }

object call (const char* fun) {
  return tmscm_to_object (call_scheme (eval_scheme(fun))); }
object call (const char* fun, object a1) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1))); }
object call (const char* fun, object a1, object a2) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1), object_to_tmscm (a2))); }
object call (const char* fun, object a1, object a2, object a3) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1),
                                       object_to_tmscm (a2), object_to_tmscm (a3))); }
object call (const char* fun, object a1, object a2, object a3, object a4) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1),
                                       object_to_tmscm (a2), object_to_tmscm (a3), object_to_tmscm (a4))); }
object call (const char* fun, array<object> a) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), array_lookup(a))); }

object call (string fun) {
  return tmscm_to_object (call_scheme (eval_scheme(fun))); }
object call (string fun, object a1) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1))); }
object call (string fun, object a1, object a2) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1), object_to_tmscm (a2))); }
object call (string fun, object a1, object a2, object a3) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1),
                                       object_to_tmscm (a2), object_to_tmscm (a3))); }
object call (string fun, object a1, object a2, object a3, object a4) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), object_to_tmscm (a1),
                                       object_to_tmscm (a2), object_to_tmscm (a3), object_to_tmscm (a4))); }
object call (string fun, array<object> a) {
  return tmscm_to_object (call_scheme (eval_scheme(fun), array_lookup(a))); }

object call (object fun) {
  return tmscm_to_object (call_scheme (object_to_tmscm (fun))); }
object call (object fun, object a1) {
  return tmscm_to_object (call_scheme (object_to_tmscm (fun), object_to_tmscm (a1))); }
object call (object fun, object a1, object a2) {
  return tmscm_to_object (call_scheme (object_to_tmscm (fun), object_to_tmscm (a1), object_to_tmscm (a2))); }
object call (object fun, object a1, object a2, object a3) {
  return tmscm_to_object (call_scheme (object_to_tmscm (fun), object_to_tmscm (a1),
                                       object_to_tmscm (a2), object_to_tmscm (a3))); }
object call (object fun, object a1, object a2, object a3, object a4) {
  return tmscm_to_object (call_scheme (object_to_tmscm (fun), object_to_tmscm (a1),
                                       object_to_tmscm (a2), object_to_tmscm (a3), object_to_tmscm (a4))); }
object call (object fun, array<object> a) {
  return tmscm_to_object (call_scheme (object_to_tmscm (fun), array_lookup(a))); }

/******************************************************************************
* User preferences
******************************************************************************/

static bool preferences_ok= false;

void
notify_preferences_booted () {
  preferences_ok= true;
}

void
set_preference (string var, string val) {
  if (!preferences_ok) set_user_preference (var, val);
  else (void) call ("set-preference", var, val);
}

void
notify_preference (string var) {
  if (preferences_ok) (void) call ("notify-preference", var);
}

string
get_preference (string var, string def) {
  if (!preferences_ok)
    return get_user_preference (var, def);
  else {
    string pref= as_string (call ("get-preference", var));
    if (pref == "default") return def; else return pref;
  }
}

/******************************************************************************
* Delayed evaluation
******************************************************************************/

#ifndef QTTEXMACS
static array<object> delayed_queue;
static array<time_t> start_queue;

void
exec_delayed (object cmd) {
  delayed_queue << cmd;
  start_queue << (((time_t) texmacs_time ()) - 1000000000);
}

void
exec_delayed_pause (object cmd) {
  delayed_queue << cmd;
  start_queue << ((time_t) texmacs_time ());
}

void
exec_pending_commands () {
  array<object> a= delayed_queue;
  array<time_t> b= start_queue;
  delayed_queue= array<object> (0);
  start_queue  = array<time_t> (0);
  int i, n= N(a);
  for (i=0; i<n; i++) {
    time_t now= (time_t) texmacs_time ();
    if ((now - b[i]) >= 0) {
      object obj= call (a[i]);
      if (is_int (obj) && (now - b[i] < 1000000000)) {
          //cout << "pause= " << obj << "\n";
        delayed_queue << a[i];
        start_queue << (now + as_int (obj));
      }
    }
    else {
      delayed_queue << a[i];
      start_queue << b[i];
    }
  }
}

void
clear_pending_commands () {
  delayed_queue= array<object> (0);
  start_queue  = array<time_t> (0);
}
#endif // QTTEXMACS

/******************************************************************************
* Protected evaluation
******************************************************************************/

void
protected_call (object cmd) {
#ifdef USE_EXCEPTIONS
  try {
#endif
    get_current_editor()->before_menu_action ();
    call (cmd);
    get_current_editor()->after_menu_action ();
#ifdef USE_EXCEPTIONS
  }
  catch (string s) {
    get_current_editor()->cancel_menu_action ();
  }
  handle_exceptions ();
#endif
}
