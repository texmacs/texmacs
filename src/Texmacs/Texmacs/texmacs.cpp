
/******************************************************************************
* MODULE     : texmacs.cpp
* DESCRIPTION: main program
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <locale.h> // for setlocale
#include <signal.h>
#ifdef STACK_SIZE
#include <sys/resource.h>
#endif

#include "tm_ostream.hpp"
#include "boot.hpp"
#include "file.hpp"
#include "server.hpp"
#include "tm_timer.hpp"
#include "data_cache.hpp"
#include "tm_window.hpp"
#ifdef AQUATEXMACS
void mac_fix_paths ();
#endif

#ifdef QTTEXMACS
#include "Qt/QTMApplication.hpp"
#include "Qt/qt_utilities.hpp"
#include <QDir>
#endif

#ifdef OS_MINGW
#include "Windows/win-utf8-compat.hpp"
#endif

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_utilities.h"
#endif

#if defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS)
#include "MacOS/mac_app.h"
#endif

extern bool   char_clip;

extern url    tm_init_file;
extern url    tm_init_buffer_file;
extern string my_init_cmds;
extern string original_path;

extern int geometry_w, geometry_h;
extern int geometry_x, geometry_y;

extern tree the_et;
extern bool texmacs_started;

bool disable_error_recovery= false;
bool start_server_flag= false;
string extra_init_cmd;
void server_start ();

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
* Clean exit on segmentation faults
******************************************************************************/

void 
clean_exit_on_segfault (int sig_num) {
  (void) sig_num;
  FAILED ("segmentation fault");
}

/******************************************************************************
* Texmacs paths
******************************************************************************/

void
TeXmacs_init_paths (int& argc, char** argv) {
  (void) argc; (void) argv;
#ifdef QTTEXMACS
  url exedir = url_system (qt_application_directory ());
#else
  url exedir = url_system(argv[0]) * ".." ;
  if (! is_rooted(exedir)) {
    exedir = url_pwd() * exedir ;
  }
#endif

  string current_texmacs_path = get_env ("TEXMACS_PATH");

#if (defined(QTTEXMACS) && defined(Q_WS_MAC)) 
  // the following line can inibith external plugin loading
  // QCoreApplication::setLibraryPaths(QStringList());
  // ideally we would like to control the external plugins
  // and add the most useful (gif, jpeg, svg converters)
  // to the bundle package. I still do not have a reliable solution
  // so just allow everything that is reachable.
        
  // plugins need to be installed in TeXmacs.app/Contents/Plugins        
  QCoreApplication::addLibraryPath( QDir::cleanPath(QCoreApplication::applicationDirPath().append("/../Plugins")) );
  // cout << from_qstring ( QCoreApplication::libraryPaths () .join("\n") ) << LF;
  {
    // ensure that private versions of the Qt frameworks have priority on
    // other instances.
    // in the event that we load qt plugins which could possibly link to
    // other instances of the Qt libraries
    string buf;
    buf = as_string(exedir * "../Frameworks");
    if (get_env("DYLD_FRAMEWORK_PATH") != "") buf = buf * ":" * get_env("DYLD_FRAMEWORK_PATH");    
    set_env ("DYLD_FRAMEWORK_PATH", buf);    
    buf = as_string(exedir * "../Resources/lib");
    if (get_env("DYLD_LIBRARY_PATH") != "") buf = buf * ":" * get_env("DYLD_LIBRARY_PATH");    
    set_env ("DYLD_LIBRARY_PATH", buf);    
  }
#endif

#if defined(AQUATEXMACS) ||(defined(QTTEXMACS) && defined(Q_WS_MAC)) || (defined(X11TEXMACS) && defined (MACOSX_EXTENSIONS))
  // Mac bundle environment initialization
  // We set some environment variables when the executable
  // is in a .app bundle on MacOSX
  if (is_empty (current_texmacs_path))
    set_env ("TEXMACS_PATH", as_string(exedir * "../Resources/share/TeXmacs"));
  //cout << get_env("PATH") * ":" * as_string(url("$PWD") * argv[0]
  // * "../../Resources/share/TeXmacs/bin") << LF;
  if (exists("/bin/bash")) {
    string shell_env = var_eval_system ("PATH='' /bin/bash -l -c 'echo $PATH'");
    set_env ("PATH", get_env("PATH") * ":" * shell_env * ":" *
             as_string (exedir * "../Resources/share/TeXmacs/bin"));
  } else {
    set_env ("PATH", get_env("PATH") * ":" *
             as_string (exedir * "../Resources/share/TeXmacs/bin"));
  }
  // system("set");
#endif

#ifdef OS_MINGW
  // Win bundle environment initialization
  // TEXMACS_PATH is set by assuming that the executable is in TeXmacs/bin/
  // HOME is set to USERPROFILE
  // PWD is set to HOME
  // if PWD is lacking, then the path resolution machinery may not work
  
  if (is_empty (current_texmacs_path))
    set_env ("TEXMACS_PATH", as_string (exedir * ".."));
  // if (get_env ("HOME") == "") //now set in immediate_options otherwise --setup option fails
  //   set_env ("HOME", get_env("USERPROFILE"));
  // HACK
  // In WINE the variable PWD is already in the outer Unix environment 
  // so we need to override it to have a correct behaviour
  if ((get_env ("PWD") == "") || (get_env ("PWD")[0] == '/'))  {
    set_env ("PWD", as_string (exedir));
    // set_env ("PWD", get_env("HOME"));
  }
  // system("set");
#endif

  // check on the latest $TEXMACS_PATH
  current_texmacs_path = get_env ("TEXMACS_PATH");
  if (is_empty (current_texmacs_path) ||
      !exists (url_system (current_texmacs_path))) {
    cout << "The required TEXMACS_PATH("
         << current_texmacs_path
         << ") does not exists" << LF;
    exit(1);
  }
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
      else if (s == "-debug-history") debug (DEBUG_FLAG_HISTORY, true);
      else if (s == "-debug-qt") debug (DEBUG_FLAG_QT, true);
      else if (s == "-debug-qt-widgets") debug (DEBUG_FLAG_QT_WIDGETS, true);
      else if (s == "-debug-keyboard") debug (DEBUG_FLAG_KEYBOARD, true);
      else if (s == "-debug-packrat") debug (DEBUG_FLAG_PACKRAT, true);
      else if (s == "-debug-flatten") debug (DEBUG_FLAG_FLATTEN, true);
      else if (s == "-debug-correct") debug (DEBUG_FLAG_CORRECT, true);
      else if (s == "-debug-convert") debug (DEBUG_FLAG_CONVERT, true);
      else if (s == "-debug-all") {
        debug (DEBUG_FLAG_EVENTS, true);
        debug (DEBUG_FLAG_STD, true);
        debug (DEBUG_FLAG_IO, true);
        debug (DEBUG_FLAG_HISTORY, true);
        debug (DEBUG_FLAG_BENCH, true);
        debug (DEBUG_FLAG_QT, true);
        debug (DEBUG_FLAG_QT_WIDGETS, true);
      }
      else if (s == "-disable-error-recovery") disable_error_recovery= true;
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
        cout << "SVN version " << TEXMACS_REVISION << "\n";
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
        set_reverse_colors (true);
      else if (s == "-no-retina") {
        retina_manual= true;
        retina_factor= 1;
        retina_icons = 1;
        retina_scale = 1.0;
      }
      else if ((s == "-R") || (s == "-retina")) {
        retina_manual= true;
        retina_factor= 2;
        retina_icons = 2;
        retina_scale = 1.4;
      }
      else if (s == "-no-retina-icons") {
        retina_iman  = true;
        retina_icons = 1;
      }
      else if (s == "-retina-icons") {
        retina_iman  = true;
        retina_icons = 2;
      }
      else if ((s == "-c") || (s == "-convert")) {
        i+=2;
        if (i<argc) {
          url in  ("$PWD", argv[i-1]);
          url out ("$PWD", argv[ i ]);
          my_init_cmds= my_init_cmds * " " *
            "(load-buffer " * scm_quote (as_string (in)) * " :strict) " *
            "(export-buffer " * scm_quote (as_string (out)) * ")";
        }
      }
      else if ((s == "-x") || (s == "-execute")) {
        i++;
        if (i<argc) my_init_cmds= (my_init_cmds * " ") * argv[i];
      }
      else if (s == "-server") start_server_flag= true;
      else if (s == "-log-file") i++;
      else if ((s == "-Oc") || (s == "-no-char-clipping")) char_clip= false;
      else if ((s == "+Oc") || (s == "-char-clipping")) char_clip= true;
      else if ((s == "-S") || (s == "-setup") ||
               (s == "-delete-cache") || (s == "-delete-font-cache") ||
               (s == "-delete-style-cache") || (s == "-delete-file-cache") ||
               (s == "-delete-doc-cache") || (s == "-delete-plugin-cache") ||
               (s == "-delete-server-data") || (s == "-delete-databases"));
      else if (s == "-build-manual") {
        if ((++i)<argc)
          extra_init_cmd << "(build-manual "
                         << scm_quote (argv[i]) << " delayed-quit)";
      }
      else if (s == "-reference-suite") {
        if ((++i)<argc)
          extra_init_cmd << "(build-ref-suite "
                         << scm_quote (argv[i]) << " delayed-quit)";
      }
      else if (s == "-test-suite") {
        if ((++i)<argc)
          extra_init_cmd << "(run-test-suite "
                         << scm_quote (argv[i]) << "delayed-quit)";
      }
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

  // Further options via environment variables
  if (get_env ("TEXMACS_RETINA") == "off") {
    retina_manual= true;
    retina_factor= 1;
    retina_icons = 1;
    retina_scale = 1.0;
  }
  if (get_env ("TEXMACS_RETINA") == "on") {
    retina_manual= true;
    retina_factor= 2;
    retina_icons = 2;
    retina_scale = 1.4;
  }
  if (get_env ("TEXMACS_RETINA_ICONS") == "off") {
    retina_iman  = true;
    retina_icons = 1;
  }
  if (get_env ("TEXMACS_RETINA_ICONS") == "on") {
    retina_iman  = true;
    retina_icons = 2;
  }
  // End options via environment variables

  // Further user preferences
  use_unified_toolbar= get_preference ("use unified toolbar", "on") == "on";
  // End user preferences

  if (DEBUG_STD) debug_boot << "Installing internal plug-ins...\n";
  bench_start ("initialize plugins");
  init_plugins ();
  bench_cumul ("initialize plugins");
  if (DEBUG_STD) debug_boot << "Opening display...\n";
  
#if defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS)
  init_mac_application ();
#endif
    
  gui_open (argc, argv);
  set_default_font (the_default_font);
  if (DEBUG_STD) debug_boot << "Starting server...\n";
  { // opening scope for server sv
  server sv;

  // HACK:
  // Qt and Guile want to change the locale. 
  // We need to force it to C to parse correctly the configuration files
  // (see as_double() in string.cpp)
  setlocale(LC_NUMERIC, "C");    
    
  string where= "";
  for (i=1; i<argc; i++) {
    if (argv[i] == NULL) break;
    string s= argv[i];
    if ((N(s)>=2) && (s(0,2)=="--")) s= s (1, N(s));
    if ((s[0] != '-') && (s[0] != '+')) {
      if (DEBUG_STD) debug_boot << "Loading " << s << "...\n";
      url u= url_system (s);
      if (!is_rooted (u)) u= resolve (url_pwd (), "") * u;
      string b= scm_quote (as_string (u));
      string cmd= "(load-buffer " * b * " " * where * ")";
      where= " :new-window";
      exec_delayed (scheme_cmd (cmd));
    }
    if      ((s == "-c") || (s == "-convert")) i+=2;
    else if ((s == "-b") || (s == "-initialize-buffer") ||
             (s == "-fn") || (s == "-font") ||
             (s == "-i") || (s == "-initialize") ||
             (s == "-g") || (s == "-geometry") ||
             (s == "-x") || (s == "-execute") ||
             (s == "-log-file") ||
             (s == "-build-manual") ||
             (s == "-reference-suite") || (s == "-test-suite")) i++;
  }
  if (install_status == 1) {
    if (DEBUG_STD) debug_boot << "Loading welcome message...\n";
    url u= "tmfs://help/plain/tm/doc/about/welcome/first.en.tm";
    string b= scm_quote (as_string (u));
    string cmd= "(load-buffer " * b * " " * where * ")";
    where= " :new-window";
    exec_delayed (scheme_cmd (cmd));
  }
  else if (install_status == 2) {
    if (DEBUG_STD) debug_boot << "Loading upgrade message...\n";
    url u= "tmfs://help/plain/tm/doc/about/changes/changes-recent.en.tm";
    string b= scm_quote (as_string (u));
    string cmd= "(load-buffer " * b * " " * where * ")";
    where= " :new-window";
    exec_delayed (scheme_cmd (cmd));
  }
  if (number_buffers () == 0) {
    if (DEBUG_STD) debug_boot << "Creating 'no name' buffer...\n";
    open_window ();
  }

  bench_print ();
  bench_reset ("initialize texmacs");
  bench_reset ("initialize plugins");
  bench_reset ("initialize scheme");

  if (DEBUG_STD) debug_boot << "Starting event loop...\n";
  texmacs_started= true;
  if (!disable_error_recovery) signal (SIGSEGV, clean_exit_on_segfault);
  if (start_server_flag) server_start ();
  if (N(extra_init_cmd) > 0) exec_delayed (scheme_cmd (extra_init_cmd));
  gui_start_loop ();

  if (DEBUG_STD) debug_boot << "Stopping server...\n";
  } // ending scope for server sv

  if (DEBUG_STD) debug_boot << "Closing display...\n";
  gui_close ();
  
#if defined(X11TEXMACS) && defined(MACOSX_EXTENSIONS)
  finalize_mac_application ();
#endif
  
  if (DEBUG_STD) debug_boot << "Good bye...\n";
}

/******************************************************************************
* Main program
******************************************************************************/

#ifdef OS_MACOS
#include <sys/resource.h>
#endif

void
boot_hacks () {
#ifdef OS_MACOS
// NOTE: under MACOS, there is a limited number of open file descriptors,
// by default 256.  Any open file descriptor can actually count several times
// whenever the files is stored in various chunks on disk.  Hence, the limit
// is easily exceeded, although this situation cannot easily be debugged.
// Our current hack is to allow for at least 4096 open file descriptors.
  rlimit lims;
  getrlimit (RLIMIT_NOFILE, &lims);
  lims.rlim_cur= max (lims.rlim_cur, 4096);
  setrlimit (RLIMIT_NOFILE, &lims);
  //getrlimit (RLIMIT_NOFILE, &lims);
  //printf ("cur: %i\n", lims.rlim_cur);
  //printf ("max: %i\n", lims.rlim_max);
#if defined(MAC_OS_X_VERSION_10_10)
  //if (release - 4 >= 10)
  mac_fix_yosemite_bug();
#endif

#ifdef QTTEXMACS
#if defined(MAC_OS_X_VERSION_10_9) || defined(MAC_OS_X_VERSION_10_10)
#if QT_VERSION <= QT_VERSION_CHECK(4,8,5)
  // Work around Qt bug: https://bugreports.qt-project.org/browse/QTBUG-32789
  QFont::insertSubstitution (".Lucida Grande UI", "Lucida Grande");
#endif
#endif
#endif

#endif
}

/******************************************************************************
* Main program
******************************************************************************/

void
immediate_options (int argc, char** argv) {
  if (get_env ("TEXMACS_HOME_PATH") == "")
#ifdef OS_MINGW
  {
    if (get_env ("HOME") == "")
        set_env ("HOME", get_env("USERPROFILE"));
    set_env ("TEXMACS_HOME_PATH", get_env ("APPDATA") * "\\TeXmacs");
	}
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
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-database.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-features.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
    }
    else if (s == "-delete-cache")
      remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("*"));
    else if (s == "-delete-style-cache")
      remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("__*"));
    else if (s == "-delete-font-cache") {
      remove (url ("$TEXMACS_HOME_PATH/system/cache/font_cache.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-database.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-features.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/font-characteristics.scm"));
      remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));
    }
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
    else if (s == "-delete-plugin-cache")
      remove (url ("$TEXMACS_HOME_PATH/system/cache/plugin_cache.scm"));
    else if (s == "-delete-server-data")
      system ("rm -rf", url ("$TEXMACS_HOME_PATH/server"));
    else if (s == "-delete-databases") {
      system ("rm -rf", url ("$TEXMACS_HOME_PATH/system/database"));
      system ("rm -rf", url ("$TEXMACS_HOME_PATH/users"));
    }
    else if (s == "-log-file" && i + 1 < argc) {
      i++;
      char* log_file = argv[i];
      tm_ostream logf (log_file);
      if (!logf->is_writable ())
        cerr << "TeXmacs] Error: could not open " << log_file << "\n";
      cout.redirect (logf);
      cerr.redirect (logf);
    }
  }
}

#include <cstdio>

int
main (int argc, char** argv) {

#ifdef STACK_SIZE
  struct rlimit limit;

  if (getrlimit(RLIMIT_STACK, &limit) == 0) {
    if (limit.rlim_max < STACK_SIZE) {
      cerr << "Max stack allowed value : " << limit.rlim_max << "\n";
      limit.rlim_cur= limit.rlim_max;
    } else limit.rlim_cur= STACK_SIZE;
    if(setrlimit(RLIMIT_STACK, &limit)) cerr << "Cannot set stack value\n";
  } else cerr << "Cannot get stack value\n";
#endif

#ifdef OS_MINGW
	nowide::args a(argc,argv); // Fix arguments - make them UTF-8
#endif


  original_path= get_env ("PATH");
  boot_hacks ();
  windows_delayed_refresh (1000000000);
  immediate_options (argc, argv);
#ifndef OS_MINGW
  set_env ("LC_NUMERIC", "POSIX");
#endif
#ifdef MACOSX_EXTENSIONS
  // Reset TeXmacs if Alt is pressed during startup
  if (mac_alternate_startup()) {
    cout << "TeXmacs] Performing setup (Alt on startup)" << LF; 
    remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("*"));
    remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));    
  }
#endif
#ifdef QTTEXMACS
  // initialize the Qt application infrastructure
  QTMApplication* qtmapp= new QTMApplication (argc, argv);  
#endif
  TeXmacs_init_paths (argc, argv);
#ifdef QTTEXMACS
  qtmapp->set_window_icon("/misc/images/texmacs-512.png");
#endif
  //cout << "Bench  ] Started TeXmacs\n";
  the_et     = tuple ();
  the_et->obs= ip_observer (path ());
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
  start_scheme (argc, argv, TeXmacs_main);
#ifdef QTTEXMACS
  delete qtmapp;
#endif
  return 0;
}
