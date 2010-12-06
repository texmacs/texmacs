
/******************************************************************************
* MODULE     : glue.cpp
* DESCRIPTION: Glue for linking TeXmacs commands to guile
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <tm_configure.hpp>
#include "Glue/glue.hpp"
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
#include <string.h>
#include <libguile.h>

#define SCM_ARG8 8
#define SCM_ARG9 9

#ifdef DOTS_OK
typedef SCM (*FN)(...);
#else
typedef SCM (*FN)();
#endif

extern void initialize_glue_basic ();
extern void initialize_glue_editor ();
extern void initialize_glue_server ();

/******************************************************************************
* Miscellaneous routines for use by glue only
******************************************************************************/

string
scheme_dialect () {
#ifdef GUILE_A
  return "guile-a";
#else
#ifdef GUILE_B
  return "guile-b";
#else
#ifdef GUILE_C
  return "guile-c";
#else
  return "unknown";
#endif
#endif
#endif
}

string
texmacs_version (string which) {
  if (which == "tgz") return TEXMACS_TGZ;
  if (which == "rpm") return TEXMACS_RPM;
  if (which == "stgz") return TEXMACS_STGZ;
  if (which == "srpm") return TEXMACS_SRPM;
  if (which == "win") return TEXMACS_WIN;
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

#ifdef GUILE_C
#define SET_SMOB(smob,data,type)   \
  SCM_NEWSMOB (smob, SCM_UNPACK (type), data);
#else
#define SET_SMOB(smob,data,type)   \
  SCM_NEWCELL (smob);              \
  SCM_SETCAR (smob, (SCM) (type)); \
  SCM_SETCDR (smob, (SCM) (data));
#endif

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
* Direct access to scheme objects
******************************************************************************/

#define SCM_ASSERT_OBJECT(flag,arg,rout)

/*static*/ SCM
object_to_scm (object obj) {
  return obj->lookup();
}

static object
scm_to_object (SCM obj) {
  return object (obj);
}

/******************************************************************************
* Booleans
******************************************************************************/

#define SCM_ASSERT_BOOL(flag,arg,rout) \
  SCM_ASSERT (scm_is_bool (flag), flag, arg, rout)

SCM
bool_to_scm (bool flag) {
  return scm_bool2scm (flag);
}

#ifndef GUILE_C
int
scm_to_bool (SCM flag) {
  return scm_scm2bool (flag);
}
#endif

/******************************************************************************
* Integers
******************************************************************************/

#define SCM_ASSERT_INT(i,arg,rout) \
  SCM_ASSERT (scm_is_int (i), i, arg, rout);

SCM
int_to_scm (int i) {
  return scm_long2scm ((long) i);
}

#ifndef GUILE_C
int
scm_to_int (SCM i) {
  return (int) scm_scm2long (i);
}
#endif

/******************************************************************************
* Floating point numbers
******************************************************************************/

#define SCM_ASSERT_DOUBLE(i,arg,rout) \
  SCM_ASSERT (SCM_REALP (i), i, arg, rout);

SCM
double_to_scm (double i) {
  return scm_double2scm (i);
}

#ifndef GUILE_C
double
scm_to_double (SCM i) {
  return scm_scm2double (i);
}
#endif

/******************************************************************************
* Strings
******************************************************************************/

#define SCM_ASSERT_STRING(s,arg,rout) \
  SCM_ASSERT (scm_is_string (s), s, arg, rout)

SCM
string_to_scm (string s) {
  char* _s= as_charp (s);
  SCM r= scm_str2scm (_s, N(s));
  tm_delete_array (_s);
  return r;
}

string
scm_to_string (SCM s) {
  guile_str_size_t len_r;
  char* _r= scm_scm2str (s, &len_r);
  string r (_r, len_r);
#ifdef OS_WIN32
  scm_must_free(_r);
#else
  free (_r);
#endif
  return r;
}

/******************************************************************************
* Symbols
******************************************************************************/

#define SCM_ASSERT_SYMBOL(s,arg,rout) \
  SCM_ASSERT (SCM_NFALSEP (scm_symbol_p (s)), s, arg, rout)

SCM
symbol_to_scm (string s) {
  char* _s= as_charp (s);
  SCM r= scm_symbol2scm (_s);
  tm_delete_array (_s);
  return r;
}

string
scm_to_symbol (SCM s) {
  guile_str_size_t len_r;
  char* _r= scm_scm2symbol (s, &len_r);
  string r (_r, len_r);
#ifdef OS_WIN32
  scm_must_free(_r);
#else
  free (_r);
#endif
  return r;
}

/******************************************************************************
* Tree labels
******************************************************************************/

#define SCM_ASSERT_TREE_LABEL(p,arg,rout) SCM_ASSERT_SYMBOL(p,arg,rout)

SCM
tree_label_to_scm (tree_label l) {
  string s= as_string (l);
  return symbol_to_scm (s);
}

tree_label
scm_to_tree_label (SCM p) {
  string s= scm_to_symbol (p);
  return make_tree_label (s);
}

/******************************************************************************
* Trees
******************************************************************************/

static long tree_tag;

#define SCM_TREEP(t) \
  (SCM_NIMP (t) && (((long) SCM_CAR (t)) == tree_tag))
#define SCM_ASSERT_TREE(t,arg,rout) \
  SCM_ASSERT (SCM_TREEP (t), t, arg, rout)

bool
scm_is_tree (SCM t) {
  return SCM_TREEP (t);
}

SCM
tree_to_scm (tree t) {
  SCM tree_smob;
  SET_SMOB (tree_smob, (void*) (tm_new<tree> (t)), (SCM) tree_tag);
  return tree_smob;
}

tree
scm_to_tree (SCM tree_smob) {
  return *((tree*) SCM_CDR (tree_smob));
}

static SCM
mark_tree (SCM tree_smob) {
  (void) tree_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_tree (SCM tree_smob) {
  tree *ptr = (tree *) SCM_CDR (tree_smob);
  tm_delete (ptr);
  return 0;
}

static int
print_tree (SCM tree_smob, SCM port, scm_print_state *pstate) {
  (void) pstate;
  tree   t= scm_to_tree (tree_smob);
  string s= "<tree " * tree_to_texmacs (t) * ">";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_tree (SCM t1, SCM t2) {
  return scm_bool2scm (scm_to_tree (t1) == scm_to_tree (t2));
}

static SCM
treeP (SCM t) {
  bool b= scm_is_tree (t);
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

SCM
scheme_tree_to_scm (scheme_tree t) {
  if (is_atomic (t)) {
    string s= t->label;
    if (s == "#t") return SCM_BOOL_T;
    if (s == "#f") return SCM_BOOL_F;
    if (is_int (s)) return int_to_scm (as_int (s));
    if (is_quoted (s))
      return string_to_scm (scm_unquote (s));
    //if ((N(s)>=2) && (s[0]=='\42') && (s[N(s)-1]=='\42'))
    //return string_to_scm (s (1, N(s)-1));
    return symbol_to_scm (s);
  }
  else {
    int i;
    SCM p= SCM_NULL;
    for (i=N(t)-1; i>=0; i--)
      p= scm_cons (scheme_tree_to_scm (t[i]), p);
    return p;
  }
}

scheme_tree
scm_to_scheme_tree (SCM p) {
  if (scm_is_list (p)) {
    tree t (TUPLE);
    while (!scm_is_null (p)) {
      t << scm_to_scheme_tree (SCM_CAR (p));
      p= SCM_CDR (p);
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
scm_is_content (SCM p) {
  if (scm_is_string (p) || scm_is_tree (p)) return true;
  else if (!scm_is_pair (p) || !scm_is_symbol (SCM_CAR (p))) return false;
  else {
    for (p= SCM_CDR (p); !scm_is_null (p); p= SCM_CDR (p))
      if (!scm_is_content (SCM_CAR (p))) return false;
    return true;
  }
}

#define content tree
#define SCM_ASSERT_CONTENT(p,arg,rout) \
  SCM_ASSERT (scm_is_content (p), p, arg, rout)
#define content_to_scm tree_to_scm

tree
scm_to_content (SCM p) {
  if (scm_is_string (p)) return scm_to_string (p);
  if (scm_is_tree (p)) return scm_to_tree (p);
  if (scm_is_pair (p)) {
    if (!scm_is_symbol (SCM_CAR (p))) return "?";
    tree t (make_tree_label (scm_to_symbol (SCM_CAR (p))));
    p= SCM_CDR (p);
    while (!scm_is_null (p)) {
      t << scm_to_content (SCM_CAR (p));
      p= SCM_CDR (p);
    }
    return t;
  }
  return "?";
}

static SCM
contentP (SCM t) {
  bool b= scm_is_content (t);
  return bool_to_scm (b);
}

/******************************************************************************
* Paths
******************************************************************************/

bool
scm_is_path (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_int (SCM_CAR (p)) && scm_is_path (SCM_CDR (p));
}

#define SCM_ASSERT_PATH(p,arg,rout) \
  SCM_ASSERT (scm_is_path (p), p, arg, rout)

SCM
path_to_scm (path p) {
  if (is_nil (p)) return SCM_NULL;
  else return scm_cons (int_to_scm (p->item), path_to_scm (p->next));
}

path
scm_to_path (SCM p) {
  if (scm_is_null (p)) return path ();
  else return path ((int) scm_to_int (SCM_CAR (p)), scm_to_path (SCM_CDR (p)));
}

/******************************************************************************
* Observers
******************************************************************************/

static long observer_tag;

#define scm_is_observer(o) \
  (SCM_NIMP (o) && (((long) SCM_CAR (o)) == observer_tag))
#define SCM_ASSERT_OBSERVER(o,arg,rout) \
  SCM_ASSERT (scm_is_observer (o), o, arg, rout)

/*static*/ SCM
observer_to_scm (observer o) {
  SCM observer_smob;
  SET_SMOB (observer_smob, (void*) (tm_new<observer> (o)), (SCM) observer_tag);
  return observer_smob;
}

static observer
scm_to_observer (SCM observer_smob) {
  return *((observer*) SCM_CDR (observer_smob));
}

static SCM
mark_observer (SCM observer_smob) {
  (void) observer_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_observer (SCM observer_smob) {
  observer *ptr = (observer *) SCM_CDR (observer_smob);
  tm_delete (ptr);
  return 0;
}

static int
print_observer (SCM observer_smob, SCM port, scm_print_state *pstate) {
  (void) observer_smob; (void) pstate;
  string s= "<observer>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_observer (SCM o1, SCM o2) {
  return scm_bool2scm (scm_to_observer (o1) == scm_to_observer (o2));
}

static SCM
observerP (SCM t) {
  bool b= scm_is_observer (t);
  return bool_to_scm (b);
}

/******************************************************************************
* Widgets
******************************************************************************/

static long widget_tag;

#define SCM_WIDGETP(wid) \
  (SCM_NIMP (wid) && (((long) SCM_CAR (wid)) == widget_tag))
#define SCM_ASSERT_WIDGET(wid,arg,rout) \
  SCM_ASSERT (scm_is_widget (wid), wid, arg, rout)

bool
scm_is_widget (SCM wid) {
  return SCM_WIDGETP (wid);
}

static SCM
widget_to_scm (widget wid) {
  SCM widget_smob;
  SET_SMOB (widget_smob, (void*) (tm_new<widget> (wid)), (SCM) widget_tag);
  return widget_smob;
}

/*static*/ widget
scm_to_widget (SCM widget_smob) {
  return *((widget*) SCM_CDR (widget_smob));
}

static SCM
mark_widget (SCM widget_smob) {
  (void) widget_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_widget (SCM widget_smob) {
  widget *ptr = (widget *) SCM_CDR (widget_smob);
  tm_delete (ptr);
  return 0;
}

static int
print_widget (SCM widget_smob, SCM port, scm_print_state *pstate) {
  (void) widget_smob; (void) pstate;
  string s= "<widget>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_widget (SCM wid1, SCM wid2) {
  return scm_bool2scm (scm_to_widget (wid1) == scm_to_widget (wid2));
}

/******************************************************************************
* Widget factory
******************************************************************************/

typedef promise<widget> promise_widget;

static long promise_widget_tag;

#define SCM_ASSERT_PROMISE_WIDGET(pw,arg,rout) \
  SCM_ASSERT (scm_is_promise_widget (pw), pw, arg, rout)

#define scm_is_promise_widget(pw) \
  (SCM_NIMP (pw) && (((long) SCM_CAR (pw)) == promise_widget_tag))

static SCM
promise_widget_to_scm (promise_widget pw) {
  SCM promise_widget_smob;
  SET_SMOB (promise_widget_smob,
	    (void*) (tm_new<promise_widget> (pw)),
	    (SCM) promise_widget_tag);
  return promise_widget_smob;
}

static promise_widget
scm_to_promise_widget (SCM promise_widget_smob) {
  return *((promise_widget*) SCM_CDR (promise_widget_smob));
}

static SCM
mark_promise_widget (SCM promise_widget_smob) {
  (void) promise_widget_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_promise_widget (SCM promise_widget_smob) {
  promise_widget *ptr = (promise_widget *) SCM_CDR (promise_widget_smob);
  tm_delete (ptr);
  // should be replaced by total size of the widget factory
  return 0;
}

static int
print_promise_widget (SCM promise_widget_smob, SCM port,
		      scm_print_state *pstate)
{
  (void) promise_widget_smob; (void) pstate;
  string s= "<promise-widget>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_promise_widget (SCM pw1, SCM pw2) {
  return scm_bool2scm (scm_to_promise_widget (pw1) ==
		       scm_to_promise_widget (pw2));
}

/******************************************************************************
* Commands objects
******************************************************************************/

static long command_tag;

#define scm_is_command(cmd) \
  (SCM_NIMP (cmd) && (((long) SCM_CAR (cmd)) == command_tag))
#define SCM_ASSERT_COMMAND(cmd,arg,rout) \
  SCM_ASSERT (scm_is_command (cmd), cmd, arg, rout)

static SCM
command_to_scm (command cmd) {
  SCM command_smob;
  SET_SMOB (command_smob, (void*) (tm_new<command> (cmd)), (SCM) command_tag);
  return command_smob;
}

static command
scm_to_command (SCM command_smob) {
  return *((command*) SCM_CDR (command_smob));
}

static SCM
mark_command (SCM command_smob) {
  (void) command_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_command (SCM command_smob) {
  command *ptr = (command *) SCM_CDR (command_smob);
  tm_delete (ptr);
  return 0;
}

static int
print_command (SCM command_smob, SCM port, scm_print_state *pstate) {
  (void) command_smob; (void) pstate;
  string s= "<command>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_command (SCM cmd1, SCM cmd2) {
  return scm_bool2scm (scm_to_command (cmd1) == scm_to_command (cmd2));
}

/******************************************************************************
* Urls
******************************************************************************/

static long url_tag;

#define SCM_URLP(u) \
  (SCM_NIMP (u) && (((long) SCM_CAR (u)) == url_tag))
#define SCM_ASSERT_URL(u,arg,rout) \
  SCM_ASSERT (scm_is_url (u) || scm_is_string (u), u, arg, rout)

bool
scm_is_url (SCM u) {
  return SCM_URLP (u);
}

SCM
url_to_scm (url u) {
  SCM url_smob;
  SET_SMOB (url_smob, (void*) (tm_new<url> (u)), (SCM) url_tag);
  return url_smob;
}

url
scm_to_url (SCM url_smob) {
  if (scm_is_string (url_smob))
#ifdef __MINGW32__
    return url_system(scm_to_string (url_smob));
#else
    return scm_to_string (url_smob);
#endif
  return *((url*) SCM_CDR (url_smob));
}

static SCM
mark_url (SCM url_smob) {
  (void) url_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_url (SCM url_smob) {
  url *ptr = (url *) SCM_CDR (url_smob);
  tm_delete (ptr);
  return 0;
}

static int
print_url (SCM url_smob, SCM port, scm_print_state *pstate) {
  (void) pstate;
  url    u= scm_to_url (url_smob);
  string s= "<url " * as_string (u) * ">";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_url (SCM u1, SCM u2) {
  return scm_bool2scm (scm_to_url (u1) == scm_to_url (u2));
}

static SCM
urlP (SCM t) {
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
scm_is_table_string_string (SCM p) {
  if (scm_is_null (p)) return true;
  else if (!scm_is_pair (p)) return false;
  else {
    SCM f= SCM_CAR (p);
    return scm_is_pair (f) &&
           scm_is_string (SCM_CAR (f)) &&
           scm_is_string (SCM_CDR (f)) &&
           scm_is_table_string_string (SCM_CDR (p));
  }
}

#define SCM_ASSERT_TABLE_STRING_STRING(p,arg,rout) \
  SCM_ASSERT (scm_is_table_string_string (p), p, arg, rout)

SCM
table_string_string_to_scm (hashmap<string,string> t) {
  SCM p= SCM_NULL;
  iterator<string> it= iterate (t);
  while (it->busy ()) {
    string s= it->next ();
    SCM n= scm_cons (string_to_scm (s), string_to_scm (t[s]));
    p= scm_cons (n, p);
  }
  return p;
}

hashmap<string,string>
scm_to_table_string_string (SCM p) {
  hashmap<string,string> t;
  while (!scm_is_null (p)) {
    SCM n= SCM_CAR (p);
    t (scm_to_string (SCM_CAR (n)))= scm_to_string (SCM_CDR (n));
    p= SCM_CDR (p);
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

static bool
scm_is_array_int (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
	      scm_is_int (SCM_CAR (p)) &&
	      scm_is_array_int (SCM_CDR (p));
}

#define SCM_ASSERT_ARRAY_INT(p,arg,rout) \
  SCM_ASSERT (scm_is_array_int (p), p, arg, rout)

/* static */ SCM
array_int_to_scm (array<int> a) {
  int i, n= N(a);
  SCM p= SCM_NULL;
  for (i=n-1; i>=0; i--) p= scm_cons (int_to_scm (a[i]), p);
  return p;
}

/* static */ array<int>
scm_to_array_int (SCM p) {
  array<int> a;
  while (!scm_is_null (p)) {
    a << ((int) scm_to_int (SCM_CAR (p)));
    p= SCM_CDR (p);
  }
  return a;
}

static bool
scm_is_array_string (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) && 
	      scm_is_string (SCM_CAR (p)) &&
	      scm_is_array_string (SCM_CDR (p));
}

#define SCM_ASSERT_ARRAY_STRING(p,arg,rout) \
  SCM_ASSERT (scm_is_array_string (p), p, arg, rout)

/* static */ SCM
array_string_to_scm (array<string> a) {
  int i, n= N(a);
  SCM p= SCM_NULL;
  for (i=n-1; i>=0; i--) p= scm_cons (string_to_scm (a[i]), p);
  return p;
}

/* static */ array<string>
scm_to_array_string (SCM p) {
  array<string> a;
  while (!scm_is_null (p)) {
    a << scm_to_string (SCM_CAR (p));
    p= SCM_CDR (p);
  }
  return a;
}

#define scm_is_property scm_is_array_string
#define SCM_ASSERT_PROPERTY(p,arg,rout) SCM_ASSERT_ARRAY_STRING (p,arg,rout)
#define property_to_scm array_string_to_scm
#define scm_to_property scm_to_array_string

static bool
scm_is_array_tree (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) && 
	      SCM_TREEP (SCM_CAR (p)) &&
	      scm_is_array_tree (SCM_CDR (p));
}

#define SCM_ASSERT_ARRAY_TREE(p,arg,rout) \
  SCM_ASSERT (scm_is_array_tree (p), p, arg, rout)

/* static */ SCM
array_tree_to_scm (array<tree> a) {
  int i, n= N(a);
  SCM p= SCM_NULL;
  for (i=n-1; i>=0; i--) p= scm_cons (tree_to_scm (a[i]), p);
  return p;
}

/* static */ array<tree>
scm_to_array_tree (SCM p) {
  array<tree> a;
  while (!scm_is_null (p)) {
    a << scm_to_tree (SCM_CAR (p));
    p= SCM_CDR (p);
  }
  return a;
}

static bool
scm_is_array_widget (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
	      scm_is_widget (SCM_CAR (p)) &&
	      scm_is_array_widget (SCM_CDR (p));
}

#define SCM_ASSERT_ARRAY_WIDGET(p,arg,rout) \
  SCM_ASSERT (scm_is_array_widget (p), p, arg, rout)

/* static */ SCM
array_widget_to_scm (array<widget> a) {
  int i, n= N(a);
  SCM p= SCM_NULL;
  for (i=n-1; i>=0; i--) p= scm_cons (widget_to_scm (a[i]), p);
  return p;
}

/* static */ array<widget>
scm_to_array_widget (SCM p) {
  array<widget> a;
  while (!scm_is_null (p)) {
    a << scm_to_widget (SCM_CAR (p));
    p= SCM_CDR (p);
  }
  return a;
}

static bool
scm_is_properties (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
	      scm_is_property (SCM_CAR (p)) &&
	      scm_is_properties (SCM_CDR (p));
}

#define SCM_ASSERT_PROPERTIES(p,arg,rout) \
  SCM_ASSERT (scm_is_properties (p), p, arg, rout)

SCM
properties_to_scm (array<property> a) {
  int i, n= N(a);
  SCM p= SCM_NULL;
  for (i=n-1; i>=0; i--) p= scm_cons (property_to_scm (a[i]), p);
  return p;
}

array<property>
scm_to_properties (SCM p) {
  array<property> a;
  while (!scm_is_null (p)) {
    a << scm_to_property (SCM_CAR (p));
    p= SCM_CDR (p);
  }
  return a;
}

static bool
scm_is_solutions (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
	      scm_is_solution (SCM_CAR (p)) &&
	      scm_is_solutions (SCM_CDR (p));
}

#define SCM_ASSERT_SOLUTIONS(p,arg,rout) \
  SCM_ASSERT (scm_is_solutions (p), p, arg, rout)

SCM
solutions_to_scm (array<solution> a) {
  int i, n= N(a);
  SCM p= SCM_NULL;
  for (i=n-1; i>=0; i--) p= scm_cons (solution_to_scm (a[i]), p);
  return p;
}

array<solution>
scm_to_solutions (SCM p) {
  array<solution> a;
  while (!scm_is_null (p)) {
    a << scm_to_solution (SCM_CAR (p));
    p= SCM_CDR (p);
  }
  return a;
}

/******************************************************************************
* List types
******************************************************************************/

typedef list<string> list_string;

bool
scm_is_list_string (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
	      scm_is_string (SCM_CAR (p)) &&
	      scm_is_list_string (SCM_CDR (p));
}

#define SCM_ASSERT_LIST_STRING(p,arg,rout) \
  SCM_ASSERT (scm_is_list_string (p), p, arg, rout)

SCM
list_string_to_scm (list_string l) {
  if (is_nil (l)) return SCM_NULL;
  return scm_cons (string_to_scm (l->item),
		   list_string_to_scm (l->next));
}

list_string
scm_to_list_string (SCM p) {
  if (scm_is_null (p)) return list_string ();
  return list_string (scm_to_string (SCM_CAR (p)),
		      scm_to_list_string (SCM_CDR (p)));
}

typedef list<tree> list_tree;

bool
scm_is_list_tree (SCM p) {
  if (scm_is_null (p)) return true;
  else return scm_is_pair (p) &&
	      scm_is_tree (SCM_CAR (p)) &&
	      scm_is_list_tree (SCM_CDR (p));
}

#define SCM_ASSERT_LIST_TREE(p,arg,rout) \
  SCM_ASSERT (scm_is_list_tree (p), p, arg, rout)

SCM
list_tree_to_scm (list_tree l) {
  if (is_nil (l)) return SCM_NULL;
  return scm_cons (tree_to_scm (l->item),
		   list_tree_to_scm (l->next));
}

list_tree
scm_to_list_tree (SCM p) {
  if (scm_is_null (p)) return list_tree ();
  return list_tree (scm_to_tree (SCM_CAR (p)),
		    scm_to_list_tree (SCM_CDR (p)));
}

/******************************************************************************
* Other wrapper types
******************************************************************************/

#define SCM_ASSERT_COLLECTION(p,arg,rout) \
  SCM_ASSERT (scm_is_array_string (p), p, arg, rout)

SCM
collection_to_scm (collection ss) {
  return array_string_to_scm (as_strings (ss));
}

collection
scm_to_collection (SCM p) {
  return as_collection (scm_to_array_string (p));
}

/******************************************************************************
* Initialization
******************************************************************************/

#ifdef SCM_NEWSMOB

void
initialize_glue () {
  tree_tag= scm_make_smob_type (const_cast<char*> ("tree"), 0);
  scm_set_smob_mark (tree_tag, mark_tree);
  scm_set_smob_free (tree_tag, free_tree);
  scm_set_smob_print (tree_tag, print_tree);
  scm_set_smob_equalp (tree_tag, cmp_tree);
  observer_tag= scm_make_smob_type (const_cast<char*> ("observer"), 0);
  scm_set_smob_mark (observer_tag, mark_observer);
  scm_set_smob_free (observer_tag, free_observer);
  scm_set_smob_print (observer_tag, print_observer);
  scm_set_smob_equalp (observer_tag, cmp_observer);
  widget_tag= scm_make_smob_type (const_cast<char*> ("widget"), 0);
  scm_set_smob_mark (widget_tag, mark_widget);
  scm_set_smob_free (widget_tag, free_widget);
  scm_set_smob_print (widget_tag, print_widget);
  scm_set_smob_equalp (widget_tag, cmp_widget);
  promise_widget_tag=
    scm_make_smob_type (const_cast<char*> ("promise-widget"), 0);
  scm_set_smob_mark (promise_widget_tag, mark_promise_widget);
  scm_set_smob_free (promise_widget_tag, free_promise_widget);
  scm_set_smob_print (promise_widget_tag, print_promise_widget);
  scm_set_smob_equalp (promise_widget_tag, cmp_promise_widget);
  command_tag= scm_make_smob_type (const_cast<char*> ("command"), 0);
  scm_set_smob_mark (command_tag, mark_command);
  scm_set_smob_free (command_tag, free_command);
  scm_set_smob_print (command_tag, print_command);
  scm_set_smob_equalp (command_tag, cmp_command);
  url_tag= scm_make_smob_type (const_cast<char*> ("url"), 0);
  scm_set_smob_mark (url_tag, mark_url);
  scm_set_smob_free (url_tag, free_url);
  scm_set_smob_print (url_tag, print_url);
  scm_set_smob_equalp (url_tag, cmp_url);
  scm_new_procedure ("tree?", (FN) treeP, 1, 0, 0);
  scm_new_procedure ("tm?", (FN) contentP, 1, 0, 0);
  scm_new_procedure ("observer?", (FN) observerP, 1, 0, 0);
  scm_new_procedure ("url?", (FN) urlP, 1, 0, 0);
  initialize_glue_basic ();
  initialize_glue_editor ();
  initialize_glue_server ();
}

#else

scm_smobfuns tree_smob_funcs = {
  mark_tree, free_tree, print_tree, cmp_tree
};

scm_smobfuns observer_smob_funcs = {
  mark_observer, free_observer, print_observer, cmp_observer
};

scm_smobfuns widget_smob_funcs = {
  mark_widget, free_widget, print_widget, cmp_widget
};

scm_smobfuns promise_widget_smob_funcs = {
  mark_promise_widget, free_promise_widget,
  print_promise_widget, cmp_promise_widget
};

scm_smobfuns command_smob_funcs = {
  mark_command, free_command, print_command, cmp_command
};

scm_smobfuns url_smob_funcs = {
  mark_url, free_url, print_url, cmp_url
};

void
initialize_glue () {
  tree_tag= scm_newsmob (&tree_smob_funcs);
  observer_tag= scm_newsmob (&observer_smob_funcs);
  widget_tag= scm_newsmob (&widget_smob_funcs);
  promise_widget_tag= scm_newsmob (&promise_widget_smob_funcs);
  command_tag= scm_newsmob (&command_smob_funcs);
  url_tag= scm_newsmob (&url_smob_funcs);
  scm_new_procedure ("tree?", (FN) treeP, 1, 0, 0);
  scm_new_procedure ("tm?", (FN) contentP, 1, 0, 0);
  scm_new_procedure ("observer?", (FN) observerP, 1, 0, 0);
  scm_new_procedure ("url?", (FN) urlP, 1, 0, 0);
  initialize_glue_basic ();
  initialize_glue_editor ();
  initialize_glue_server ();
}

#endif
