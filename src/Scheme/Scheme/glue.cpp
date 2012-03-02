
/******************************************************************************
 * MODULE     : glue.cpp
 * DESCRIPTION: Glue for linking TeXmacs commands to scheme
 * COPYRIGHT  : (C) 1999-2011  Joris van der Hoeven and Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "glue.hpp"


#include "promise.hpp"
#include "tree.hpp"

#include "boxes.hpp"
#include "editor.hpp"
#include "analyze.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "tmfs.hpp"


scm
blackboxP (scm t) {
	bool b= scm_is_blackbox (t);
	return bool_to_scm (b);
}


/******************************************************************************
 * Miscellaneous routines for use by glue only
 ******************************************************************************/

string
texmacs_version (string which) {
	if (which == "tgz") return TM_DEVEL;
	if (which == "rpm") return TM_DEVEL_RELEASE;
	if (which == "stgz") return TM_STABLE;
	if (which == "srpm") return TM_STABLE_RELEASE;
	if (which == "devel") return TM_DEVEL;
	if (which == "stable") return TM_STABLE;
	if (which == "devel-release") return TM_DEVEL_RELEASE;
	if (which == "stable-release") return TM_STABLE_RELEASE;
	return TEXMACS_VERSION;
}

void
set_fast_environments (bool b) {
	enable_fastenv= b;
}

void
win32_display (string s) {
	cout << s;
	cout.flush ();
}

void
tm_output (string s) {
	cout << s;
	cout.flush ();
}

void
tm_errput (string s) {
	cerr << s;
	cerr.flush ();
}

void
cpp_error () {
	//char *np= 0; *np= 1;
	FAILED ("an error occurred");
}

array<int>
get_bounding_rectangle (tree t) {
  editor ed= get_server () -> get_editor ();
  rectangle wr= ed -> get_window_extents ();
  path p= reverse (obtain_ip (t));
  selection sel= ed->search_selection (p * 0, p * right_index (t));
  rectangle selr= least_upper_bound (sel->rs) / 5;
  rectangle r= translate (selr, wr->x1, wr->y2);
  array<int> ret;
  ret << (r->x1) << (r->y1) << (r->x2) << (r->y2);
  //ret << (r->x1/PIXEL) << (r->y1/PIXEL) << (r->x2/PIXEL) << (r->y2/PIXEL);
  return ret;
}

/******************************************************************************
 * Redirections
 ******************************************************************************/

void
cout_buffer () {
	cout.buffer ();
}

string
cout_unbuffer () {
	return cout.unbuffer ();
}



/******************************************************************************
 * Basic assertions
 ******************************************************************************/


#define SCM_ASSERT_STRING(s,arg,rout) \
SCM_ASSERT (scm_is_string (s), s, arg, rout)
#define SCM_ASSERT_BOOL(flag,arg,rout) \
SCM_ASSERT (scm_is_bool (flag), flag, arg, rout)
#define SCM_ASSERT_INT(i,arg,rout) \
SCM_ASSERT (scm_is_int (i), i, arg, rout);
#define SCM_ASSERT_DOUBLE(i,arg,rout) \
  SCM_ASSERT (scm_is_double (i), i, arg, rout);
//SCM_ASSERT (SCM_REALP (i), i, arg, rout);
#define SCM_ASSERT_URL(u,arg,rout) \
SCM_ASSERT (scm_is_url (u) || scm_is_string (u), u, arg, rout)
#define SCM_ASSERT_BLACKBOX(t,arg,rout) \
SCM_ASSERT (scm_is_blackbox (t), t, arg, rout)
#define SCM_ASSERT_SYMBOL(s,arg,rout) \
  SCM_ASSERT (scm_is_symbol (s), s, arg, rout)
//SCM_ASSERT (SCM_NFALSEP (scm_symbol_p (s)), s, arg, rout)

#define SCM_ASSERT_OBJECT(a,b,c)
// no check

/******************************************************************************
 * Tree labels
 ******************************************************************************/

#define SCM_ASSERT_TREE_LABEL(p,arg,rout) SCM_ASSERT_SYMBOL(p,arg,rout)

scm
tree_label_to_scm (tree_label l) {
	string s= as_string (l);
	return symbol_to_scm (s);
}

tree_label
scm_to_tree_label (scm p) {
	string s= scm_to_symbol (p);
	return make_tree_label (s);
}

/******************************************************************************
 * Trees
 ******************************************************************************/

#define SCM_ASSERT_TREE(t,arg,rout) SCM_ASSERT (scm_is_tree (t), t, arg, rout)


bool
scm_is_tree (scm u) {
	return (scm_is_blackbox (u) && (type_box (scm_to_blackbox(u)) == type_helper<tree>::id));
}

scm
tree_to_scm (tree o) {
	return blackbox_to_scm (close_box<tree> (o));
}

tree
scm_to_tree (scm obj) {
	return open_box<tree>(scm_to_blackbox (obj));
}

scm
treeP (scm t) {
	bool b= scm_is_blackbox (t) && 
	(type_box (scm_to_blackbox(t)) == type_helper<tree>::id);
	return bool_to_scm (b);
}

tree
coerce_string_tree (string s) {
	return s;
}

string
coerce_tree_string (tree t) {
	return as_string (t);
}

tree
tree_ref (tree t, int i) {
	return t[i];
}

tree
tree_set (tree t, int i, tree u) {
	t[i]= u;
	return u;
}

tree
tree_range (tree t, int i, int j) {
	return t(i,j);
}

tree
tree_append (tree t1, tree t2) {
	return t1 * t2;
}

bool
tree_active (tree t) {
	path ip= obtain_ip (t);
	return is_nil (ip) || last_item (ip) != DETACHED;
}

tree
tree_child_insert (tree t, int pos, tree x) {
	//cout << "t= " << t << "\n";
	//cout << "x= " << x << "\n";
	int i, n= N(t);
	tree r (t, n+1);
	for (i=0; i<pos; i++) r[i]= t[i];
	r[pos]= x;
	for (i=pos; i<n; i++) r[i+1]= t[i];
	return r;
}

/******************************************************************************
 * Document modification routines
 ******************************************************************************/

extern tree the_et;

tree
tree_assign (tree r, tree t) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		assign (reverse (ip), copy (t));
		return subtree (the_et, reverse (ip));
	}
	else {
		assign (r, copy (t));
		return r;
	}
}

tree
tree_insert (tree r, int pos, tree t) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		insert (reverse (path (pos, ip)), copy (t));
		return subtree (the_et, reverse (ip));
	}
	else {
		insert (r, pos, copy (t));
		return r;
	}
}

tree
tree_remove (tree r, int pos, int nr) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		remove (reverse (path (pos, ip)), nr);
		return subtree (the_et, reverse (ip));
	}
	else {
		remove (r, pos, nr);
		return r;
	}
}

tree
tree_split (tree r, int pos, int at) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		split (reverse (path (at, pos, ip)));
		return subtree (the_et, reverse (ip));
	}
	else {
		split (r, pos, at);
		return r;
	}
}

tree
tree_join (tree r, int pos) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		join (reverse (path (pos, ip)));
		return subtree (the_et, reverse (ip));
	}
	else {
		join (r, pos);
		return r;
	}
}

tree
tree_assign_node (tree r, tree_label op) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		assign_node (reverse (ip), op);
		return subtree (the_et, reverse (ip));
	}
	else {
		assign_node (r, op);
		return r;
	}
}

tree
tree_insert_node (tree r, int pos, tree t) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		insert_node (reverse (path (pos, ip)), copy (t));
		return subtree (the_et, reverse (ip));
	}
	else {
		insert_node (r, pos, copy (t));
		return r;
	}
}

tree
tree_remove_node (tree r, int pos) {
	path ip= copy (obtain_ip (r));
	if (ip_attached (ip)) {
		remove_node (reverse (path (pos, ip)));
		return subtree (the_et, reverse (ip));
	}
	else {
		remove_node (r, pos);
		return r;
	}
}

/******************************************************************************
 * Scheme trees
 ******************************************************************************/

#define SCM_ASSERT_SCHEME_TREE(p,arg,rout)

scm
scheme_tree_to_scm (scheme_tree t) {
	if (is_atomic (t)) {
		string s= t->label;
		if (s == "#t") return scm_true ();
		if (s == "#f") return scm_false ();
		if (is_int (s)) return int_to_scm (as_int (s));
		if (is_quoted (s))
			return string_to_scm (scm_unquote (s));
		//if ((N(s)>=2) && (s[0]=='\42') && (s[N(s)-1]=='\42'))
		//return string_to_scm (s (1, N(s)-1));
		return symbol_to_scm (s);
	}
	else {
		int i;
		scm p= scm_null ();
		for (i=N(t)-1; i>=0; i--)
			p= scm_cons (scheme_tree_to_scm (t[i]), p);
		return p;
	}
}

scheme_tree
scm_to_scheme_tree (scm p) {
	if (scm_is_list (p)) {
		tree t (TUPLE);
		while (!scm_is_null (p)) {
			t << scm_to_scheme_tree (scm_car (p));
			p= scm_cdr (p);
		}
		return t;
	}
	if (scm_is_symbol (p)) return scm_to_symbol (p);
	if (scm_is_string (p)) return scm_quote (scm_to_string (p));
	//if (scm_is_string (p)) return "\"" * scm_to_string (p) * "\"";
	if (scm_is_int (p)) return as_string ((int) scm_to_int (p));
	if (scm_is_bool (p)) return (scm_to_bool (p)? string ("#t"): string ("#f"));
	if (scm_is_tree (p)) return tree_to_scheme_tree (scm_to_tree (p));
	return "?";
}

/******************************************************************************
 * Content
 ******************************************************************************/

bool
scm_is_content (scm p) {
	if (scm_is_string (p) || scm_is_tree (p)) return true;
	else if (!scm_is_pair (p) || !scm_is_symbol (scm_car (p))) return false;
	else {
		for (p= scm_cdr (p); !scm_is_null (p); p= scm_cdr (p))
			if (!scm_is_content (scm_car (p))) return false;
		return true;
	}
}

#define content tree
#define SCM_ASSERT_CONTENT(p,arg,rout) \
   SCM_ASSERT (scm_is_content (p), p, arg, rout)
#define content_to_scm tree_to_scm

tree
scm_to_content (scm p) {
	if (scm_is_string (p)) return scm_to_string (p);
	if (scm_is_tree (p)) return scm_to_tree (p);
	if (scm_is_pair (p)) {
		if (!scm_is_symbol (scm_car (p))) return "?";
		tree t (make_tree_label (scm_to_symbol (scm_car (p))));
		p= scm_cdr (p);
		while (!scm_is_null (p)) {
			t << scm_to_content (scm_car (p));
			p= scm_cdr (p);
		}
		return t;
	}
	return "?";
}

scm
contentP (scm t) {
	bool b= scm_is_content (t);
	return bool_to_scm (b);
}

/******************************************************************************
 * Paths
 ******************************************************************************/

bool
scm_is_path (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_int (scm_car (p)) && scm_is_path (scm_cdr (p));
}

#define SCM_ASSERT_PATH(p,arg,rout) \
SCM_ASSERT (scm_is_path (p), p, arg, rout)

scm
path_to_scm (path p) {
	if (is_nil (p)) return scm_null ();
	else return scm_cons (int_to_scm (p->item), path_to_scm (p->next));
}

path
scm_to_path (scm p) {
	if (scm_is_null (p)) return path ();
	else return path ((int) scm_to_int (scm_car (p)), scm_to_path (scm_cdr (p)));
}


/******************************************************************************
 * Observers
 ******************************************************************************/

#define SCM_ASSERT_OBSERVER(o,arg,rout) \
SCM_ASSERT (scm_is_observer (o), o, arg, rout)


bool
scm_is_observer (scm o) {
	return (scm_is_blackbox (o) && (type_box (scm_to_blackbox(o)) == type_helper<observer>::id));
}

scm
observer_to_scm (observer o) {
  return blackbox_to_scm (close_box<observer> (o));
}

static observer
scm_to_observer (scm obj) {
	return open_box<observer>(scm_to_blackbox (obj));
}

scm
observerP (scm t) {
	bool b= scm_is_blackbox (t) && 
	(type_box (scm_to_blackbox(t)) == type_helper<observer>::id);
	return bool_to_scm (b);
}


/******************************************************************************
 * Widgets
 ******************************************************************************/

#define SCM_ASSERT_WIDGET(o,arg,rout) \
SCM_ASSERT (scm_is_widget (o), o, arg, rout)

bool
scm_is_widget (scm u) {
	return (scm_is_blackbox (u) && (type_box (scm_to_blackbox(u)) == type_helper<widget>::id));
}


static scm
widget_to_scm (widget o) {
	return blackbox_to_scm (close_box<widget> (o));
}

widget
scm_to_widget (scm o) {
	return open_box<widget> (scm_to_blackbox (o));
}

/******************************************************************************
 * Commands
 ******************************************************************************/

#define SCM_ASSERT_COMMAND(o,arg,rout) \
SCM_ASSERT (scm_is_command (o), o, arg, rout)

bool
scm_is_command (scm u) {
	return (scm_is_blackbox (u) && (type_box (scm_to_blackbox(u)) == type_helper<command>::id));
}

static scm
command_to_scm (command o) {
	return blackbox_to_scm (close_box<command> (o));
}

static command
scm_to_command (scm o) {
	return open_box<command> (scm_to_blackbox (o));
}


/******************************************************************************
 *  Widget Factory
 ******************************************************************************/

typedef promise<widget> promise_widget;

#define SCM_ASSERT_PROMISE_WIDGET(o,arg,rout) \
SCM_ASSERT (scm_is_promise_widget (o), o, arg, rout)

bool
scm_is_promise_widget (scm u) {
	return (scm_is_blackbox (u) && (type_box (scm_to_blackbox(u)) == type_helper<promise_widget>::id));
}



static scm
promise_widget_to_scm (promise_widget o) {
	return blackbox_to_scm (close_box<promise_widget> (o));
}

static promise_widget
scm_to_promise_widget (scm o) {
	return open_box<promise_widget> (scm_to_blackbox (o));
}



/******************************************************************************
 * Urls
 ******************************************************************************/



bool
scm_is_url (scm u) {
	return (scm_is_blackbox (u) && (type_box (scm_to_blackbox(u)) == type_helper<url>::id)) || (scm_is_string(u));
}

scm
url_to_scm (url u) {
	return blackbox_to_scm (close_box<url> (u));
}

url
scm_to_url (scm obj) {
	if (scm_is_string (obj))
#ifdef __MINGW32__
		return url_system (scm_to_string (obj));
#else
	return scm_to_string (obj);
#endif
	return open_box<url> (scm_to_blackbox (obj));
}

scm
urlP (scm t) {
	bool b= scm_is_url (t);
	return bool_to_scm (b);
}

url url_concat (url u1, url u2) { return u1 * u2; }
url url_or (url u1, url u2) { return u1 | u2; }
void string_save (string s, url u) { (void) save_string (u, s); }
string string_load (url u) {
	string s; (void) load_string (u, s, false); return s; }
url url_ref (url u, int i) { return u[i]; }


/******************************************************************************
 * Table types
 ******************************************************************************/

typedef hashmap<string,string> table_string_string;

static bool
scm_is_table_string_string (scm p) {
	if (scm_is_null (p)) return true;
	else if (!scm_is_pair (p)) return false;
	else {
		scm f= scm_car (p);
		return scm_is_pair (f) &&
		scm_is_string (scm_car (f)) &&
		scm_is_string (scm_cdr (f)) &&
		scm_is_table_string_string (scm_cdr (p));
	}
}

#define SCM_ASSERT_TABLE_STRING_STRING(p,arg,rout) \
SCM_ASSERT (scm_is_table_string_string (p), p, arg, rout)

scm
table_string_string_to_scm (hashmap<string,string> t) {
	scm p= scm_null ();
	iterator<string> it= iterate (t);
	while (it->busy ()) {
		string s= it->next ();
		scm n= scm_cons (string_to_scm (s), string_to_scm (t[s]));
		p= scm_cons (n, p);
	}
	return p;
}

hashmap<string,string>
scm_to_table_string_string (scm p) {
	hashmap<string,string> t;
	while (!scm_is_null (p)) {
		scm n= scm_car (p);
		t (scm_to_string (scm_car (n)))= scm_to_string (scm_cdr (n));
		p= scm_cdr (p);
	}
	return t;
}

#define scm_is_solution scm_is_table_string_string
#define SCM_ASSERT_SOLUTION(p,arg,rout) \
SCM_ASSERT (scm_is_solution(p), p, arg, rout)
#define solution_to_scm table_string_string_to_scm
#define scm_to_solution scm_to_table_string_string

/******************************************************************************
 * Several array types
 ******************************************************************************/

typedef array<int> array_int;
typedef array<string> array_string;
typedef array<tree> array_tree;
typedef array<widget> array_widget;
typedef array<double> array_double;
typedef array<array<double> > array_array_double;
typedef array<array<array<double> > > array_array_array_double;

static bool
scm_is_array_int (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) &&
		scm_is_int (scm_car (p)) &&
		scm_is_array_int (scm_cdr (p));
}

#define SCM_ASSERT_ARRAY_INT(p,arg,rout) \
SCM_ASSERT (scm_is_array_int (p), p, arg, rout)

/* static */ scm
array_int_to_scm (array<int> a) {
	int i, n= N(a);
	scm p= scm_null ();
	for (i=n-1; i>=0; i--) p= scm_cons (int_to_scm (a[i]), p);
	return p;
}

/* static */ array<int>
scm_to_array_int (scm p) {
	array<int> a;
	while (!scm_is_null (p)) {
		a << ((int) scm_to_int (scm_car (p)));
		p= scm_cdr (p);
	}
	return a;
}

static bool
scm_is_array_string (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) && 
		scm_is_string (scm_car (p)) &&
		scm_is_array_string (scm_cdr (p));
}


static bool
scm_is_array_double (scm p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
    scm_is_double (scm_car (p)) &&
    scm_is_array_double (scm_cdr (p));
}

#define SCM_ASSERT_ARRAY_DOUBLE(p,arg,rout) \
SCM_ASSERT (scm_is_array_double (p), p, arg, rout)

/* static */ scm
array_double_to_scm (array<double> a) {
  int i, n= N(a);
  scm p= scm_null();
  for (i=n-1; i>=0; i--) p= scm_cons (double_to_scm (a[i]), p);
  return p;
}

/* static */ array<double>
scm_to_array_double (scm p) {
  array<double> a;
  while (!scm_is_null (p)) {
    a << ((double) scm_to_double (scm_car (p)));
    p= scm_cdr (p);
  }
  return a;
}

static bool
scm_is_array_array_double (scm  p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
    scm_is_array_double (scm_car (p)) &&
    scm_is_array_array_double (scm_cdr (p));
}

#define SCM_ASSERT_ARRAY_ARRAY_DOUBLE(p,arg,rout) \
SCM_ASSERT (scm_is_array_array_double (p), p, arg, rout)

/* static */ scm
array_array_double_to_scm (array<array_double> a) {
  int i, n= N(a);
  scm p= scm_null ();
  for (i=n-1; i>=0; i--) p= scm_cons (array_double_to_scm (a[i]), p);
  return p;
}

/* static */ array<array_double>
scm_to_array_array_double (scm p) {
  array<array_double> a;
  while (!scm_is_null (p)) {
    a << ((array_double) scm_to_array_double (scm_car (p)));
    p= scm_cdr (p);
  }
  return a;
}

static bool
scm_is_array_array_array_double (scm p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
    scm_is_array_array_double (scm_car (p)) &&
    scm_is_array_array_array_double (scm_cdr (p));
}

#define SCM_ASSERT_ARRAY_ARRAY_ARRAY_DOUBLE(p,arg,rout) \
SCM_ASSERT (scm_is_array_array_array_double (p), p, arg, rout)

/* static */ scm
array_array_array_double_to_scm (array<array_array_double> a) {
  int i, n= N(a);
  scm p= scm_null ();
  for (i=n-1; i>=0; i--) p= scm_cons (array_array_double_to_scm (a[i]), p);
  return p;
}

/* static */ array<array_array_double>
scm_to_array_array_array_double (scm p) {
  array<array_array_double> a;
  while (!scm_is_null (p)) {
    a << ((array_array_double) scm_to_array_array_double (scm_car (p)));
    p= scm_cdr (p);
  }
  return a;
}

void register_glyph (string s, array_array_array_double gl);
string recognize_glyph (array_array_array_double gl);



#define SCM_ASSERT_ARRAY_STRING(p,arg,rout) \
SCM_ASSERT (scm_is_array_string (p), p, arg, rout)

/* static */ scm
array_string_to_scm (array<string> a) {
	int i, n= N(a);
	scm p= scm_null ();
	for (i=n-1; i>=0; i--) p= scm_cons (string_to_scm (a[i]), p);
	return p;
}

/* static */ array<string>
scm_to_array_string (scm p) {
	array<string> a;
	while (!scm_is_null (p)) {
		a << scm_to_string (scm_car (p));
		p= scm_cdr (p);
	}
	return a;
}

#define scm_is_property scm_is_array_string
#define SCM_ASSERT_PROPERTY(p,arg,rout) SCM_ASSERT_ARRAY_STRING (p,arg,rout)
#define property_to_scm array_string_to_scm
#define scm_to_property scm_to_array_string

static bool
scm_is_array_tree (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) && 
		scm_is_tree (scm_car (p)) &&
		scm_is_array_tree (scm_cdr (p));
}

#define SCM_ASSERT_ARRAY_TREE(p,arg,rout) \
SCM_ASSERT (scm_is_array_tree (p), p, arg, rout)

/* static */ scm
array_tree_to_scm (array<tree> a) {
	int i, n= N(a);
	scm p= scm_null ();
	for (i=n-1; i>=0; i--) p= scm_cons (tree_to_scm (a[i]), p);
	return p;
}

/* static */ array<tree>
scm_to_array_tree (scm p) {
	array<tree> a;
	while (!scm_is_null (p)) {
		a << scm_to_tree (scm_car (p));
		p= scm_cdr (p);
	}
	return a;
}

static bool
scm_is_array_widget (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) &&
		scm_is_widget (scm_car (p)) &&
		scm_is_array_widget (scm_cdr (p));
}

#define SCM_ASSERT_ARRAY_WIDGET(p,arg,rout) \
SCM_ASSERT (scm_is_array_widget (p), p, arg, rout)

/* static */ scm
array_widget_to_scm (array<widget> a) {
	int i, n= N(a);
	scm p= scm_null ();
	for (i=n-1; i>=0; i--) p= scm_cons (widget_to_scm (a[i]), p);
	return p;
}

/* static */ array<widget>
scm_to_array_widget (scm p) {
	array<widget> a;
	while (!scm_is_null (p)) {
		a << scm_to_widget (scm_car (p));
		p= scm_cdr (p);
	}
	return a;
}

static bool
scm_is_properties (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) &&
		scm_is_property (scm_car (p)) &&
		scm_is_properties (scm_cdr (p));
}

#define SCM_ASSERT_PROPERTIES(p,arg,rout) \
SCM_ASSERT (scm_is_properties (p), p, arg, rout)

scm
properties_to_scm (array<property> a) {
	int i, n= N(a);
	scm p= scm_null ();
	for (i=n-1; i>=0; i--) p= scm_cons (property_to_scm (a[i]), p);
	return p;
}

array<property>
scm_to_properties (scm p) {
	array<property> a;
	while (!scm_is_null (p)) {
		a << scm_to_property (scm_car (p));
		p= scm_cdr (p);
	}
	return a;
}

static bool
scm_is_solutions (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) &&
		scm_is_solution (scm_car (p)) &&
		scm_is_solutions (scm_cdr (p));
}

#define SCM_ASSERT_SOLUTIONS(p,arg,rout) \
SCM_ASSERT (scm_is_solutions (p), p, arg, rout)

scm
solutions_to_scm (array<solution> a) {
	int i, n= N(a);
	scm p= scm_null ();
	for (i=n-1; i>=0; i--) p= scm_cons (solution_to_scm (a[i]), p);
	return p;
}

array<solution>
scm_to_solutions (scm p) {
	array<solution> a;
	while (!scm_is_null (p)) {
		a << scm_to_solution (scm_car (p));
		p= scm_cdr (p);
	}
	return a;
}

/******************************************************************************
 * List types
 ******************************************************************************/

typedef list<string> list_string;

bool
scm_is_list_string (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) &&
		scm_is_string (scm_car (p)) &&
		scm_is_list_string (scm_cdr (p));
}

#define SCM_ASSERT_LIST_STRING(p,arg,rout) \
SCM_ASSERT (scm_is_list_string (p), p, arg, rout)

scm
list_string_to_scm (list_string l) {
	if (is_nil (l)) return scm_null ();
	return scm_cons (string_to_scm (l->item),
					 list_string_to_scm (l->next));
}

list_string
scm_to_list_string (scm p) {
	if (scm_is_null (p)) return list_string ();
	return list_string (scm_to_string (scm_car (p)),
						scm_to_list_string (scm_cdr (p)));
}

typedef list<tree> list_tree;

bool
scm_is_list_tree (scm p) {
	if (scm_is_null (p)) return true;
	else return scm_is_pair (p) &&
		scm_is_tree (scm_car (p)) &&
		scm_is_list_tree (scm_cdr (p));
}

#define SCM_ASSERT_LIST_TREE(p,arg,rout) \
SCM_ASSERT (scm_is_list_tree (p), p, arg, rout)

scm
list_tree_to_scm (list_tree l) {
	if (is_nil (l)) return scm_null ();
	return scm_cons (tree_to_scm (l->item),
					 list_tree_to_scm (l->next));
}

list_tree
scm_to_list_tree (scm p) {
	if (scm_is_null (p)) return list_tree ();
	return list_tree (scm_to_tree (scm_car (p)),
					  scm_to_list_tree (scm_cdr (p)));
}

/******************************************************************************
 * Other wrapper types
 ******************************************************************************/

#define SCM_ASSERT_COLLECTION(p,arg,rout) \
SCM_ASSERT (scm_is_array_string (p), p, arg, rout)

scm
collection_to_scm (collection ss) {
	return array_string_to_scm (as_strings (ss));
}

collection
scm_to_collection (scm p) {
	return as_collection (scm_to_array_string (p));
}


/******************************************************************************
 * Gluing
 ******************************************************************************/

#include "server.hpp"
#include "tm_window.hpp"
#include "boot.hpp"
#include "connect.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "sys_utils.hpp"
#include "tmfs.hpp"
#include "client_server.hpp"
#include "analyze.hpp"
#include "tree_traverse.hpp"
#include "tree_analyze.hpp"
#include "tree_correct.hpp"
#include "tree_modify.hpp"
#include "tm_frame.hpp"
#include "Concat/concater.hpp"
#include "converter.hpp"
#include "timer.hpp"
#include "Metafont/tex_files.hpp"
#include "Freetype/free_type.hpp"
#include "Freetype/tt_file.hpp"
#include "Bibtex/bibtex.hpp"
#include "Bibtex/bibtex_functions.hpp"
#include "link.hpp"
#include "dictionary.hpp"
#include "patch.hpp"
#include "packrat.hpp"


#define SCM scm
#define SCM_UNSPECIFIED scm_null ()

#include "../Glue/glue_basic.cpp"
#include "../Glue/glue_editor.cpp"
#include "../Glue/glue_server.cpp"


void
initialize_glue () {
	scm_install_procedure ("tree?",  treeP, 1, 0, 0);
	scm_install_procedure ("tm?",  contentP, 1, 0, 0);
	scm_install_procedure ("observer?",  observerP, 1, 0, 0);
	scm_install_procedure ("url?",  urlP, 1, 0, 0);
	scm_install_procedure ("blackbox?",  blackboxP, 1, 0, 0);
	
	initialize_glue_basic ();
	initialize_glue_editor ();
	initialize_glue_server ();
}

