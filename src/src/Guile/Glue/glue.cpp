
/******************************************************************************
* MODULE     : glue.cpp
* DESCRIPTION: Glue for linking TeXmacs commands to guile
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Glue/glue.hpp"
#include "server.hpp"
#include "connect.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "tex_files.hpp"
#include "analyze.hpp"
#include "tm_layout.hpp"
#include "converter.hpp"
#include "timer.hpp"
#include "Freetype/free_type.hpp"
#include "Freetype/tt_file.hpp"
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

string
scheme_dialect () {
#ifdef GUILE_A
  return "guile-a";
#else
#ifdef GUILE_B
  return "guile-b";
#else
  return "unknown";
#endif
#endif
}

int get_d_exp () { return d_exp; }
int get_d_hide_exp () { return 1; }

string
texmacs_version (string which) {
  if (which == "tgz") return TEXMACS_TGZ;
  if (which == "rpm") return TEXMACS_RPM;
  if (which == "stgz") return TEXMACS_STGZ;
  if (which == "srpm") return TEXMACS_SRPM;
  return TEXMACS_VERSION;
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

bool
scm_to_bool (SCM flag) {
  return scm_scm2bool (flag);
}

/******************************************************************************
* Integers
******************************************************************************/

#define SCM_ASSERT_INT(i,arg,rout) \
  SCM_ASSERT (SCM_INUMP (i), i, arg, rout);

SCM
int_to_scm (int i) {
  return scm_long2scm ((long) i);
}

int
scm_to_int (SCM i) {
  return (int) scm_scm2long (i);
}

/******************************************************************************
* Floating point numbers
******************************************************************************/

#define SCM_ASSERT_DOUBLE(i,arg,rout) \
  SCM_ASSERT (SCM_REALP (i), i, arg, rout);

static SCM
double_to_scm (double i) {
  return scm_double2scm (i);
}

static double
scm_to_double (SCM i) {
  return scm_scm2double (i);
}

/******************************************************************************
* Strings
******************************************************************************/

#define SCM_MYSTRINGP(s) (SCM_NIMP (s) && SCM_STRINGP (s))

#define SCM_ASSERT_STRING(s,arg,rout) \
  SCM_ASSERT (SCM_MYSTRINGP (s), s, arg, rout)

SCM
string_to_scm (string s) {
  char* _s= as_charp (s);
  SCM r= scm_str2scm (_s, N(s));
  delete[] _s;
  return r;
}

string
scm_to_string (SCM s) {
  guile_str_size_t len_r;
  char* _r= scm_scm2str (s, &len_r);
  string r (_r, len_r);
  free (_r);
  return r;
}

/******************************************************************************
* Symbols
******************************************************************************/

#define SCM_ASSERT_SYMBOL(s,arg,rout) \
  SCM_ASSERT (SCM_NFALSEP (scm_symbol_p (s)), s, arg, rout)

static SCM
symbol_to_scm (string s) {
  char* _s= as_charp (s);
  SCM r= scm_symbol2scm (_s);
  delete[] _s;
  return r;
}

static string
scm_to_symbol (SCM s) {
  guile_str_size_t len_r;
  char* _r= scm_scm2symbol (s, &len_r);
  string r (_r, len_r);
  free (_r);
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
  SCM_NEWCELL (tree_smob);
  SCM_SETCDR (tree_smob, (SCM) ((void*) (new tree (t))));
  SCM_SETCAR (tree_smob, tree_tag);
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
  delete ptr;
  return sizeof (tree); // should be replaced by total size of the tree
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

void
tree_set (tree t, int i, tree u) {
  t[i]= u;
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
    if ((N(s)>=2) && (s[0]=='\42') && (s[N(s)-1]=='\42'))
      return string_to_scm (s (1, N(s)-1));
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
  if (gh_symbol_p (p)) return scm_to_symbol (p);
  if (scm_is_string (p)) return "\"" * scm_to_string (p) * "\"";
  if (SCM_INUMP (p)) return as_string (scm_to_int (p));
  if (scm_is_bool (p)) return (scm_to_bool (p)? string ("#t"): string ("#f"));
  if (scm_is_tree (p)) return tree_to_scheme_tree (scm_to_tree (p));
  return "?";
}

/******************************************************************************
* TeXmacs trees
******************************************************************************/

#define texmacs_tree tree
#define SCM_ASSERT_TEXMACS_TREE SCM_ASSERT_TREE
#define texmacs_tree_to_scm tree_to_scm
#define scm_to_texmacs_tree scm_to_tree

/******************************************************************************
* Paths
******************************************************************************/

bool
scm_is_path (SCM p) {
  if (scm_is_null (p)) return true;
  else return SCM_INUMP (SCM_CAR (p)) && scm_is_path (SCM_CDR (p));
}

#define SCM_ASSERT_PATH(p,arg,rout) \
  SCM_ASSERT (scm_is_path (p), p, arg, rout)

SCM
path_to_scm (path p) {
  if (nil (p)) return SCM_NULL;
  else return scm_cons (int_to_scm (p->item), path_to_scm (p->next));
}

path
scm_to_path (SCM p) {
  if (scm_is_null (p)) return path ();
  else return path (scm_to_int (SCM_CAR (p)), scm_to_path (SCM_CDR (p)));
}

/******************************************************************************
* Displays
******************************************************************************/

static long display_tag;

#define scm_is_display(t) \
  (SCM_NIMP (t) && (((long) SCM_CAR (t)) == display_tag))
#define SCM_ASSERT_DISPLAY(t,arg,rout) \
  SCM_ASSERT (scm_is_display (t), t, arg, rout)

/*static*/ SCM
display_to_scm (display t) {
  SCM display_smob;
  SCM_NEWCELL (display_smob);
  SCM_SETCDR (display_smob, (SCM) ((void*) (new display (t))));
  SCM_SETCAR (display_smob, display_tag);
  return display_smob;
}

static display
scm_to_display (SCM display_smob) {
  return *((display*) SCM_CDR (display_smob));
}

static SCM
mark_display (SCM display_smob) {
  (void) display_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_display (SCM display_smob) {
  display *ptr = (display *) SCM_CDR (display_smob);
  delete ptr;
  return sizeof (display); // should be replaced by total size of the display
}

static int
print_display (SCM display_smob, SCM port, scm_print_state *pstate) {
  (void) display_smob; (void) pstate;
  string s= "<display>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_display (SCM t1, SCM t2) {
  return scm_bool2scm (scm_to_display (t1) == scm_to_display (t2));
}

/******************************************************************************
* Widgets
******************************************************************************/

static long widget_tag;

#define SCM_WIDGETP(t) \
  (SCM_NIMP (t) && (((long) SCM_CAR (t)) == widget_tag))
#define SCM_ASSERT_WIDGET(t,arg,rout) \
  SCM_ASSERT (scm_is_widget (t), t, arg, rout)

bool
scm_is_widget (SCM t) {
  return SCM_WIDGETP (t);
}

static SCM
widget_to_scm (widget t) {
  SCM widget_smob;
  SCM_NEWCELL (widget_smob);
  SCM_SETCDR (widget_smob, (SCM) ((void*) (new widget (t))));
  SCM_SETCAR (widget_smob, widget_tag);
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
  delete ptr;
  return sizeof (widget); // should be replaced by total size of the widget
}

static int
print_widget (SCM widget_smob, SCM port, scm_print_state *pstate) {
  (void) widget_smob; (void) pstate;
  string s= "<widget>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_widget (SCM t1, SCM t2) {
  return scm_bool2scm (scm_to_widget (t1) == scm_to_widget (t2));
}

/******************************************************************************
* Widget factory
******************************************************************************/

static long make_widget_tag;

#define SCM_ASSERT_MAKE_WIDGET(t,arg,rout) \
  SCM_ASSERT (scm_is_make_widget (t), t, arg, rout)

#define scm_is_make_widget(t) \
  (SCM_NIMP (t) && (((long) SCM_CAR (t)) == make_widget_tag))

static SCM
make_widget_to_scm (make_widget t) {
  SCM make_widget_smob;
  SCM_NEWCELL (make_widget_smob);
  SCM_SETCDR (make_widget_smob, (SCM) ((void*) (new make_widget (t))));
  SCM_SETCAR (make_widget_smob, make_widget_tag);
  return make_widget_smob;
}

static make_widget
scm_to_make_widget (SCM make_widget_smob) {
  return *((make_widget*) SCM_CDR (make_widget_smob));
}

static SCM
mark_make_widget (SCM make_widget_smob) {
  (void) make_widget_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_make_widget (SCM make_widget_smob) {
  make_widget *ptr = (make_widget *) SCM_CDR (make_widget_smob);
  delete ptr;
  // should be replaced by total size of the widget factory
  return sizeof (make_widget);
}

static int
print_make_widget (SCM make_widget_smob, SCM port, scm_print_state *pstate) {
  (void) make_widget_smob; (void) pstate;
  string s= "<make-widget>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_make_widget (SCM t1, SCM t2) {
  return scm_bool2scm (scm_to_make_widget (t1) == scm_to_make_widget (t2));
}

/******************************************************************************
* Commands objects
******************************************************************************/

static long command_tag;

#define scm_is_command(t) \
  (SCM_NIMP (t) && (((long) SCM_CAR (t)) == command_tag))
#define SCM_ASSERT_COMMAND(t,arg,rout) \
  SCM_ASSERT (scm_is_command (t), t, arg, rout)

static SCM
command_to_scm (command t) {
  SCM command_smob;
  SCM_NEWCELL (command_smob);
  SCM_SETCDR (command_smob, (SCM) ((void*) (new command (t))));
  SCM_SETCAR (command_smob, command_tag);
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
  delete ptr;
  return sizeof (command); // should be replaced by total size of the command
}

static int
print_command (SCM command_smob, SCM port, scm_print_state *pstate) {
  (void) command_smob; (void) pstate;
  string s= "<command>";
  scm_display (string_to_scm (s), port);
  return 1;
}

static SCM
cmp_command (SCM t1, SCM t2) {
  return scm_bool2scm (scm_to_command (t1) == scm_to_command (t2));
}

/******************************************************************************
* Urls
******************************************************************************/

static long url_tag;

#define SCM_URLP(u) \
  (SCM_NIMP (u) && (((long) SCM_CAR (u)) == url_tag))
#define SCM_ASSERT_URL(u,arg,rout) \
  SCM_ASSERT (scm_is_url (u) || SCM_MYSTRINGP (u), u, arg, rout)

bool
scm_is_url (SCM u) {
  return SCM_URLP (u);
}

SCM
url_to_scm (url u) {
  SCM url_smob;
  SCM_NEWCELL (url_smob);
  SCM_SETCDR (url_smob, (SCM) ((void*) (new url (u))));
  SCM_SETCAR (url_smob, url_tag);
  return url_smob;
}

url
scm_to_url (SCM url_smob) {
  if (scm_is_string (url_smob))
    return scm_to_string (url_smob);
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
  delete ptr;
  return sizeof (url); // should be replaced by total size of the url
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

url url_concat (url u1, url u2) { return u1 * u2; }
url url_or (url u1, url u2) { return u1 | u2; }
void string_save (string s, url u) { (void) save_string (u, s); }
string string_load (url u) { string s; (void) load_string (u, s); return s; }
url url_ref (url u, int i) { return u[i]; }

/******************************************************************************
* Several array types
******************************************************************************/

typedef array<string> array_string;
typedef array<tree> array_tree;
typedef array<widget> array_widget;

static bool
scm_is_array_string (SCM p) {
  if (scm_is_null (p)) return true;
  else return SCM_MYSTRINGP (SCM_CAR (p)) && scm_is_array_string (SCM_CDR (p));
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

static bool
scm_is_array_tree (SCM p) {
  if (scm_is_null (p)) return true;
  else return SCM_TREEP (SCM_CAR (p)) && scm_is_array_tree (SCM_CDR (p));
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
  else return scm_is_widget (SCM_CAR (p)) && scm_is_array_widget (SCM_CDR (p));
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

/******************************************************************************
* Initialization
******************************************************************************/

#ifdef SCM_NEWSMOB

void
initialize_glue () {
  tree_tag= scm_make_smob_type ("tree", 0);
  scm_set_smob_mark (tree_tag, mark_tree);
  scm_set_smob_free (tree_tag, free_tree);
  scm_set_smob_print (tree_tag, print_tree);
  scm_set_smob_equalp (tree_tag, cmp_tree);
  display_tag= scm_make_smob_type ("display", 0);
  scm_set_smob_mark (display_tag, mark_display);
  scm_set_smob_free (display_tag, free_display);
  scm_set_smob_print (display_tag, print_display);
  scm_set_smob_equalp (display_tag, cmp_display);
  widget_tag= scm_make_smob_type ("widget", 0);
  scm_set_smob_mark (widget_tag, mark_widget);
  scm_set_smob_free (widget_tag, free_widget);
  scm_set_smob_print (widget_tag, print_widget);
  scm_set_smob_equalp (widget_tag, cmp_widget);
  make_widget_tag= scm_make_smob_type ("make-widget", 0);
  scm_set_smob_mark (make_widget_tag, mark_make_widget);
  scm_set_smob_free (make_widget_tag, free_make_widget);
  scm_set_smob_print (make_widget_tag, print_make_widget);
  scm_set_smob_equalp (make_widget_tag, cmp_make_widget);
  command_tag= scm_make_smob_type ("command", 0);
  scm_set_smob_mark (command_tag, mark_command);
  scm_set_smob_free (command_tag, free_command);
  scm_set_smob_print (command_tag, print_command);
  scm_set_smob_equalp (command_tag, cmp_command);
  url_tag= scm_make_smob_type ("url", 0);
  scm_set_smob_mark (url_tag, mark_url);
  scm_set_smob_free (url_tag, free_url);
  scm_set_smob_print (url_tag, print_url);
  scm_set_smob_equalp (url_tag, cmp_url);
  initialize_glue_basic ();
  initialize_glue_editor ();
  initialize_glue_server ();
}

#else

scm_smobfuns tree_smob_funcs = {
  mark_tree, free_tree, print_tree, cmp_tree
};

scm_smobfuns display_smob_funcs = {
  mark_display, free_display, print_display, cmp_display
};

scm_smobfuns widget_smob_funcs = {
  mark_widget, free_widget, print_widget, cmp_widget
};

scm_smobfuns make_widget_smob_funcs = {
  mark_make_widget, free_make_widget, print_make_widget, cmp_make_widget
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
  display_tag= scm_newsmob (&display_smob_funcs);
  widget_tag= scm_newsmob (&widget_smob_funcs);
  make_widget_tag= scm_newsmob (&make_widget_smob_funcs);
  command_tag= scm_newsmob (&command_smob_funcs);
  url_tag= scm_newsmob (&url_smob_funcs);
  initialize_glue_basic ();
  initialize_glue_editor ();
  initialize_glue_server ();
}

#endif
