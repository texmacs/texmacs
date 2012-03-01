
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

/******************************************************************************
 * The object representation class
 ******************************************************************************/



static list<scm> destroy_list;

extern scm object_stack;

scm_object_rep::scm_object_rep (scm obj) {
	while (!is_nil (destroy_list)) {
		scm handle= destroy_list->item;
		
		scm_set_car (handle, scm_null ());
		while (scm_is_pair (scm_cdr (handle)) && scm_is_null (scm_cadr (handle)))
			scm_set_cdr (handle, scm_cddr( (handle)) );
		destroy_list= destroy_list->next;
	}
	handle = scm_cons ( scm_cons (obj, scm_null ()), scm_car (object_stack) );
	scm_set_car (object_stack, handle);
}

scm_object_rep::~scm_object_rep () {
	// Be careful: can't call Scheme code from this destructor,
	// because the destructor can be called during garbage collection.
	destroy_list= list<scm> ( handle, destroy_list);
}




/******************************************************************************
 * Routines on objects
 ******************************************************************************/

tm_ostream&
operator << (tm_ostream& out, object obj) {
	out.flush ();
	if (out == cout) call ("write", obj);
	else if (out == cerr) call ("write-err", obj);
	else FAILED ("not yet implemented");
	call ("force-output");
	return out;
}

bool
operator == (object obj1, object obj2) {
	scm o1= object_to_scm(obj1), o2= object_to_scm(obj2);
	return scm_is_equal (o1, o2);
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
	return scm_to_object (scm_null ()); }
object cons (object obj1, object obj2) {
	return scm_to_object (scm_cons (object_to_scm(obj1), object_to_scm(obj2))); }
object list_object (object obj1) {
	return cons (obj1, null_object ()); }
object list_object (object obj1, object obj2) {
	return cons (obj1, cons (obj2, null_object ())); }
object list_object (object obj1, object obj2, object obj3) {
	return cons (obj1, cons (obj2, cons (obj3, null_object ()))); }
object symbol_object (string s) {
	return scm_to_object ( symbol_to_scm (s) ); }
object car (object obj) {
	return scm_to_object (scm_car (object_to_scm(obj))); }
object cdr (object obj) {
	return scm_to_object (scm_cdr (object_to_scm(obj))); }
object caar (object obj) {
	return scm_to_object (scm_caar (object_to_scm(obj))); }
object cdar (object obj) {
	return scm_to_object (scm_cdar (object_to_scm(obj))); }
object cadr (object obj) {
	return scm_to_object (scm_cadr (object_to_scm(obj))); }
object cddr (object obj) {
	return scm_to_object (scm_cddr (object_to_scm(obj))); }
object caddr (object obj) {
	return scm_to_object (scm_caddr (object_to_scm(obj))); }
object cadddr (object obj) {
	return scm_to_object (scm_cadddr (object_to_scm(obj))); }


/******************************************************************************
 * Predicates
 ******************************************************************************/

bool is_null (object obj) { return scm_is_null (object_to_scm(obj)); }
bool is_list (object obj) { return scm_is_list (object_to_scm(obj)); }
bool is_bool (object obj) { return scm_is_bool (object_to_scm(obj)); }
bool is_int (object obj) { return scm_is_int (object_to_scm(obj)); }
bool is_double (object obj) { return scm_is_double (object_to_scm(obj)); }
bool is_string (object obj) { return scm_is_string (object_to_scm(obj)); }
bool is_symbol (object obj) { return scm_is_symbol (object_to_scm(obj)); }
bool is_tree (object obj) { return scm_is_tree (object_to_scm(obj)); }
bool is_path (object obj) { return scm_is_path (object_to_scm(obj)); }
bool is_url (object obj) { return scm_is_url (object_to_scm(obj)); }

/******************************************************************************
 * Basic conversions
 ******************************************************************************/
object::object (scm_object_rep* o): rep (static_cast<object_rep*>(o)) {}
object::object (): rep (tm_new<scm_object_rep> (scm_null ())) {}
object::object (bool b): rep (tm_new<scm_object_rep> (bool_to_scm (b))) {}
object::object (int i): rep (tm_new<scm_object_rep> (int_to_scm (i))) {}
object::object (double x): rep (tm_new<scm_object_rep> (double_to_scm (x))) {}
object::object (const char* s):
rep (tm_new<scm_object_rep> (string_to_scm (string (s)))) {}
object::object (string s): rep (tm_new<scm_object_rep> (string_to_scm (s))) {}
object::object (tree t): rep (tm_new<scm_object_rep> (tree_to_scm (t))) {}
object::object (list<string> l):
rep (tm_new<scm_object_rep> (list_string_to_scm(l))) {}
object::object (list<tree> l):
rep (tm_new<scm_object_rep> (list_tree_to_scm (l))) {}
object::object (path p): rep (tm_new<scm_object_rep> (path_to_scm (p))) {}
object::object (url u): rep (tm_new<scm_object_rep> (url_to_scm (u))) {}

bool
as_bool (object obj) {
	scm b= object_to_scm(obj);
	if (!scm_is_bool (b)) return false;
	return scm_to_bool (b);
}

int
as_int (object obj) {
	scm i= object_to_scm(obj);
	if (!scm_is_int (i)) return 0;
	return scm_to_int (i);
}

double
as_double (object obj) {
	scm x= object_to_scm(obj);
	if (!scm_is_double (x)) return 0.0;
	return scm_to_double (x);
}

string
as_string (object obj) {
	scm s= object_to_scm(obj);
	if (!scm_is_string (s)) return "";
	return scm_to_string (s);
}

string
as_symbol (object obj) {
	scm s= object_to_scm(obj);
	if (!scm_is_symbol (s)) return "";
	return scm_to_symbol (s);
}

tree
as_tree (object obj) {
	scm t= object_to_scm(obj);
	if (!scm_is_tree (t)) return tree ();
	return scm_to_tree (t);
}

scheme_tree
as_scheme_tree (object obj) {
	scm t= object_to_scm(obj);
	return scm_to_scheme_tree (t);
}

list<string>
as_list_string (object obj) {
	scm l= object_to_scm(obj);
	if (!scm_is_list_string (l)) return list<string> ();
	return scm_to_list_string (l);
}

list<tree>
as_list_tree (object obj) {
	scm l= object_to_scm(obj);
	if (!scm_is_list_tree (l)) return list<tree> ();
	return scm_to_list_tree (l);
}

path
as_path (object obj) {
	scm t= object_to_scm(obj);
	if (!scm_is_path (t)) return path ();
	return scm_to_path (t);
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
	scm t= object_to_scm(obj);
	if (!scm_is_url (t)) return url ("");
	return scm_to_url (t);
}

widget
as_widget (object obj) {
	scm w= object_to_scm(obj);
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
	return scm_to_content (object_to_scm(obj));
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

static inline array<scm>
array_lookup (array<object> a) {
	const int n=N(a);
	array<scm> scm(n);
	int i;
	for (i=0; i<n; i++) scm[i]= object_to_scm(a[i]);
	return scm;
}

class object_command_rep: public command_rep {
	object obj;
public:
	object_command_rep (object obj2): obj (obj2) {}
	void apply () { (void) call_scheme (object_to_scm (obj)); }
	void apply (object args) {
		(void) call_scheme (object_to_scm(obj),
							array_lookup (as_array_object (args))); }
	tm_ostream& print (tm_ostream& out) { return out << obj; }
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
		scm result= call_scheme (object_to_scm (obj));
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
	return scm_to_object (eval_scheme (expr)); }
object eval (string expr) {
	return scm_to_object (eval_scheme (expr)); }
object eval (object expr) {
	return call ("eval", expr); }
object eval_secure (string expr) {
	return eval ("(wrap-eval-secure " * expr * ")"); }
object eval_file (string name) {
	return scm_to_object (eval_scheme_file (name)); }
bool exec_file (url u) {
	object ret= eval_file (materialize (u));
	return ret != object ("#<unspecified>"); }

object call (const char* fun) {
	return scm_to_object (call_scheme (eval_scheme(fun))); }
object call (const char* fun, object a1) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1))); }
object call (const char* fun, object a1, object a2) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1), object_to_scm(a2))); }
object call (const char* fun, object a1, object a2, object a3) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1),
								object_to_scm(a2), object_to_scm(a3))); }
object call (const char* fun, object a1, object a2, object a3, object a4) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1),
								object_to_scm(a2), object_to_scm(a3), object_to_scm(a4))); }
object call (const char* fun, array<object> a) {
	return scm_to_object (call_scheme (eval_scheme(fun), array_lookup(a))); }

object call (string fun) {
	return scm_to_object (call_scheme (eval_scheme(fun))); }
object call (string fun, object a1) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1))); }
object call (string fun, object a1, object a2) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1), object_to_scm(a2))); }
object call (string fun, object a1, object a2, object a3) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1),
								object_to_scm(a2), object_to_scm(a3))); }
object call (string fun, object a1, object a2, object a3, object a4) {
	return scm_to_object (call_scheme (eval_scheme(fun), object_to_scm(a1),
								object_to_scm(a2), object_to_scm(a3), object_to_scm(a4))); }
object call (string fun, array<object> a) {
	return scm_to_object (call_scheme (eval_scheme(fun), array_lookup(a))); }

object call (object fun) {
	return scm_to_object (call_scheme (object_to_scm(fun))); }
object call (object fun, object a1) {
	return scm_to_object (call_scheme (object_to_scm(fun), object_to_scm(a1))); }
object call (object fun, object a1, object a2) {
	return scm_to_object (call_scheme (object_to_scm(fun), object_to_scm(a1), object_to_scm(a2))); }
object call (object fun, object a1, object a2, object a3) {
	return scm_to_object (call_scheme (object_to_scm(fun), object_to_scm(a1),
								object_to_scm(a2), object_to_scm(a3))); }
object call (object fun, object a1, object a2, object a3, object a4) {
	return scm_to_object (call_scheme (object_to_scm(fun), object_to_scm(a1),
								object_to_scm(a2), object_to_scm(a3), object_to_scm(a4))); }
object call (object fun, array<object> a) {
	return scm_to_object (call_scheme (object_to_scm(fun), array_lookup(a))); }

/******************************************************************************
 * User preferences
 ******************************************************************************/

static bool preferences_ok= false;

void
notify_preferences_loaded () {
	preferences_ok= true;
}


string
get_preference (string var) {
	if (!preferences_ok) return "uninitialized";
	else return as_string (call ("get-preference", var));
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

