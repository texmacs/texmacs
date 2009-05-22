
/******************************************************************************
* MODULE     : texmacs.cpp
* DESCRIPTION: main program
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boot.hpp"
#include "file.hpp"
#include "server.hpp"
#include "timer.hpp"
#include "data_cache.hpp"
#ifdef AQUATEXMACS
void mac_fix_paths ();
#endif


#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#include <QApplication>
#endif

extern bool   char_clip;
extern bool   reverse_colors;

extern url    tm_init_file;
extern url    tm_init_buffer_file;
extern string my_init_cmds;

extern int geometry_w, geometry_h;
extern int geometry_x, geometry_y;

extern tree the_et;
extern bool texmacs_started;

/******************************************************************************
* For testing
******************************************************************************/

//#define ENABLE_TESTS
#ifdef ENABLE_TESTS
void
test_routines () {
  extern void test_math ();
  test_math ();
}
#endif

/******************************************************************************
* Texmacs paths
******************************************************************************/

void
TeXmacs_init_paths (int& argc, char** argv) {
#ifdef QTTEXMACS
  url exedir = url_system (qt_application_directory ());
#else
  url exedir = url_system(argv[0]);
  if (! is_rooted(exedir)) {
    exedir = url_pwd() * exedir * "..";
  }
#endif

#if defined(AQUATEXMACS) ||(defined(QTTEXMACS) && defined(Q_WS_MAC))
  // Mac bundle environment initialization
  // We set some environment variables when the executable
  // is in a .app bundle on MacOSX
  if (get_env ("TEXMACS_PATH") == "")
    set_env ("TEXMACS_PATH", as_string(exedir * "../Resources/share/TeXmacs"));
  //cout << get_env("PATH") * ":" * as_string(url("$PWD") * argv[0]
  // * "../../Resources/share/TeXmacs/bin") << LF;
  set_env ("PATH", get_env("PATH") * ":" *
	           as_string (exedir * "../Resources/share/TeXmacs/bin"));
  //system("set");
#endif

#ifdef __MINGW32__
  // Win bundle environment initialization
  // TEXMACS_PATH is set by assuming that the executable is in TeXmacs/bin/
  // HOME is set to USERPROFILE
  // PWD is set to HOME
  // if PWD is lacking, then the path resolution machinery may not work
  
  if (get_env ("TEXMACS_PATH") == "")
    set_env ("TEXMACS_PATH", as_string (exedir * ".."));
  if (get_env ("HOME") == "")
    set_env ("HOME", get_env("USERPROFILE"));
  // HACK
  // In WINE the variable PWD is already in the outer Unix environment 
  // so we need to override it to have a correct behaviour
  if ((get_env ("PWD") == "") || (get_env ("PWD")[0] == '/'))  {
    set_env ("PWD", as_string (exedir));
    // set_env ("PWD", get_env("HOME"));
  }
  // system("set");
#endif
}

/******************************************************************************
* Real main program for encaptulation of guile
******************************************************************************/

void
TeXmacs_main (int argc, char** argv) {
  int i;
  bool flag= true;
  string the_default_font;
  for (i=1; i<argc; i++)
    if (argv[i][0] == '\0') argc= i;
    else if (((argv[i][0] == '-') ||
	      (argv[i][0] == '+')) && (argv[i][1] != '\0'))
    {
      string s= argv[i];
      if ((N(s)>=2) && (s(0,2)=="--")) s= s (1, N(s));
      if ((s == "-s") || (s == "-silent")) flag= false;
      else if ((s == "-V") || (s == "-verbose"))
	debug (DEBUG_FLAG_VERBOSE, true);
      else if ((s == "-d") || (s == "-debug")) debug (DEBUG_FLAG_STD, true);
      else if (s == "-debug-events") debug (DEBUG_FLAG_EVENTS, true);
      else if (s == "-debug-io") debug (DEBUG_FLAG_IO, true);
      else if (s == "-debug-bench") debug (DEBUG_FLAG_BENCH, true);
      else if (s == "-debug-all") {
	debug (DEBUG_FLAG_EVENTS, true);
	debug (DEBUG_FLAG_STD, true);
	debug (DEBUG_FLAG_IO, true);
	debug (DEBUG_FLAG_BENCH, true);
      }
      else if ((s == "-fn") || (s == "-font")) {
	i++;
	if (i<argc) the_default_font= argv[i];
      }
      else if ((s == "-g") || (s == "-geometry")) {
	i++;
	if (i<argc) {
	  string g= argv[i];
	  int j=0, j1, j2, j3;
	  for (j=0; j<N(g); j++)
	    if (g[j] == 'x') break;
	  j1=j; if (j<N(g)) j++;
	  for (; j<N(g); j++)
	    if ((g[j] == '+') || (g[j] == '-')) break;
	  j2=j; if (j<N(g)) j++;
	  for (; j<N(g); j++)
	    if ((g[j] == '+') || (g[j] == '-')) break;
	  j3=j;
	  if (j1<N(g)) {
	    geometry_w= max (as_int (g (0, j1)), 320);
	    geometry_h= max (as_int (g (j1+1, j2)), 200);
	  }
	  if (j3<N(g)) {
	    if (g[j2] == '-') geometry_x= as_int (g (j2, j3)) - 1;
	    else geometry_x= as_int (g (j2+1, j3));
	    if (g[j3] == '-') geometry_y= as_int (g (j3, N(g))) - 1;
	    else geometry_y= as_int (g (j3+1, N(g)));
	  }
	}
      }
      else if ((s == "-b") || (s == "-initialize-buffer")) {
	i++;
	if (i<argc) tm_init_buffer_file= url_system (argv[i]);
      }
      else if ((s == "-i") || (s == "-initialize")) {
	i++;
	if (i<argc) tm_init_file= url_system (argv[i]);
      }
      else if ((s == "-v") || (s == "-version")) {
	cout << "\n";
	cout << "TeXmacs version " << TEXMACS_VERSION << "\n";
	cout << TEXMACS_COPYRIGHT << "\n";
	cout << "\n";
	exit (0);
      }
      else if ((s == "-p") || (s == "-path")) {
	cout << get_env ("TEXMACS_PATH") << "\n";
	exit (0);
      }
      else if ((s == "-bp") || (s == "-binpath")) {
	cout << get_env ("TEXMACS_BIN_PATH") << "\n";
	exit (0);
      }
      else if ((s == "-q") || (s == "-quit"))
	my_init_cmds= my_init_cmds * " (quit-TeXmacs)";
      else if ((s == "-r") || (s == "-reverse"))
	reverse_colors= true;
      else if ((s == "-c") || (s == "-convert")) {
	i+=2;
	if (i<argc) {
	  url in  ("$PWD", argv[i-1]);
	  url out ("$PWD", argv[ i ]);
	  my_init_cmds= my_init_cmds * " " *
	    "(texmacs-load-buffer " * scm_quote (as_string (in)) *
	    " \"generic\" 0 #f) " *
	    "(export-buffer " * scm_quote (as_string (out)) * ")";
	}
      }
      else if ((s == "-x") || (s == "-execute")) {
	i++;
	if (i<argc) my_init_cmds= (my_init_cmds * " ") * argv[i];
      }
      else if ((s == "-Oc") || (s == "-no-char-clipping")) char_clip= false;
      else if ((s == "+Oc") || (s == "-char-clipping")) char_clip= true;
      else if ((s == "-S") || (s == "-setup") ||
	       (s == "-delete-cache") || (s == "-delete-font-cache") ||
	       (s == "-delete-style-cache") || (s == "-delete-file-cache") ||
	       (s == "-delete-doc-cache"));
      else if (starts (s, "-psn"));
      else {
	cout << "\n";
	cout << "Options for TeXmacs:\n\n";
	cout << "  -b [file]  Specify scheme buffers initialization file\n";
	cout << "  -c [i] [o] Convert file 'i' into file 'o'\n";
	cout << "  -d         For debugging purposes\n";
	cout << "  -fn [font] Set the default TeX font\n";
	cout << "  -g [geom]  Set geometry of window in pixels\n";
	cout << "  -h         Display this help message\n";
	cout << "  -i [file]  Specify scheme initialization file\n";
	cout << "  -p         Get the TeXmacs path\n";
	cout << "  -q         Shortcut for -x \"(quit-TeXmacs)\"\n";
	cout << "  -r         Reverse video mode\n";
	cout << "  -s         Suppress information messages\n";
	cout << "  -S         Rerun TeXmacs setup program before starting\n";
	cout << "  -v         Display current TeXmacs version\n";
	cout << "  -V         Show some informative messages\n";
	cout << "  -x [cmd]   Execute scheme command\n";
	cout << "  -Oc        TeX characters bitmap clipping off\n";
	cout << "  +Oc        TeX characters bitmap clipping on (default)\n";
	cout << "\nPlease report bugs to <bugs@texmacs.org>\n";
	cout << "\n";
	exit (0);
      }
    }
  if (flag) debug (DEBUG_FLAG_AUTO, true);

  if (DEBUG_STD) cout << "TeXmacs] Installing internal plug-ins...\n";
  bench_start ("initialize plugins");
  init_plugins ();
  bench_cumul ("initialize plugins");
  if (DEBUG_STD) cout << "TeXmacs] Opening display...\n";
  gui_open (argc, argv);
  set_default_font (the_default_font);
  if (DEBUG_STD) cout << "TeXmacs] Starting server...\n";
  { // opening scope for server sv
  server sv;

  for (i=1; i<argc; i++) {
    if (argv[i] == NULL) break;
    string s= argv[i];
    if ((N(s)>=2) && (s(0,2)=="--")) s= s (1, N(s));
    if ((s[0] != '-') && (s[0] != '+')) {
      if (DEBUG_STD) cout << "TeXmacs] Loading " << s << "...\n";
      sv->load_buffer (url_system (s), "generic", 1);
    }
    if ((s == "-b") || (s == "-initialize-buffer") ||
	(s == "-c") || (s == "-convert") ||
	(s == "-fn") || (s == "-font") ||
	(s == "-i") || (s == "-initialize") ||
	(s == "-g") || (s == "-geometry") ||
	(s == "-x") || (s == "-execute")) i++;
  }
  if (install_status == 1) {
    if (DEBUG_STD) cout << "TeXmacs] Loading welcome message...\n";
    sv->load_buffer (
      "$TEXMACS_PATH/doc/about/welcome/first.en.tm", "help", 1);
  }
  else if (install_status == 2) {
    if (DEBUG_STD) cout << "TeXmacs] Loading upgrade message...\n";
    sv->load_buffer (
      "$TEXMACS_HOME_PATH/doc/about/changes/changes-recent.en.tm", "help", 1);
  }
  if (sv->no_bufs ()) {
    if (DEBUG_STD) cout << "TeXmacs] Creating 'no name' buffer...\n";
    sv->open_window ();
  }

  bench_print ();
  bench_reset ("initialize texmacs");
  bench_reset ("initialize plugins");
  bench_reset ("initialize scheme");

  if (DEBUG_STD) cout << "TeXmacs] Starting event loop...\n";
  texmacs_started= true;
  gui_start_loop ();

  if (DEBUG_STD) cout << "TeXmacs] Stopping server...\n";
  } // ending scope for server sv

  if (DEBUG_STD) cout << "TeXmacs] Closing display...\n";
  gui_close ();
  if (DEBUG_STD) cout << "TeXmacs] Good bye...\n";
}

/******************************************************************************
* Main program
******************************************************************************/

void
immediate_options (int argc, char** argv) {
  if (get_env ("TEXMACS_HOME_PATH") == "")
#ifdef __MINGW32__
    set_env ("TEXMACS_HOME_PATH", get_env ("APPDATA") * "/TeXmacs");
#else
  set_env ("TEXMACS_HOME_PATH", get_env ("HOME") * "/.TeXmacs");
#endif
  if (get_env ("TEXMACS_HOME_PATH") == "") return;
  for (int i=1; i<argc; i++) {
    string s= argv[i];
    if ((N(s)>=2) && (s(0,2)=="--")) s= s (1, N(s));
    if ((s == "-S") || (s == "-setup")) {
      remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("*"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
    }
    else if (s == "-delete-cache")
      remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("*"));
    else if (s == "-delete-style-cache")
      remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("__*"));
    else if (s == "-delete-font-cache")
      remove (url ("$TEXMACS_HOME_PATH/system/cache/font_cache.scm"));
    else if (s == "-delete-doc-cache") {
      remove (url ("$TEXMACS_HOME_PATH/system/cache/doc_cache"));
      remove (url ("$TEXMACS_HOME_PATH/system/cache/dir_cache.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/cache/stat_cache.scm"));
    }
    else if (s == "-delete-file-cache") {
      remove (url ("$TEXMACS_HOME_PATH/system/cache/doc_cache"));
      remove (url ("$TEXMACS_HOME_PATH/system/cache/file_cache"));
      remove (url ("$TEXMACS_HOME_PATH/system/cache/dir_cache.scm"));
      remove (url ("$TEXMACS_HOME_PATH/system/cache/stat_cache.scm"));
    }
  }
}

int
main (int argc, char** argv) {
#ifdef QTTEXMACS
  // initialize the Qt application infrastructure
  new QApplication (argc, argv);
#endif
  TeXmacs_init_paths (argc, argv);
  //cout << "Bench  ] Started TeXmacs\n";
  the_et     = tuple ();
  the_et->obs= ip_observer (path ());
  immediate_options (argc, argv);
  cache_initialize ();
  bench_start ("initialize texmacs");
  init_texmacs ();
  bench_cumul ("initialize texmacs");
#ifdef ENABLE_TESTS
  test_routines ();
#endif
//#ifdef EXPERIMENTAL
//  test_environments ();
//#endif
  start_guile (argc, argv, TeXmacs_main);
  return 0;
}
