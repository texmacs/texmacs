
/******************************************************************************
* MODULE     : init_texmacs.cpp
* DESCRIPTION: Initialization of TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boot.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "convert.hpp"
#include "merge_sort.hpp"
#include "drd_std.hpp"
#include "language.hpp"
#include <unistd.h>
#ifdef OS_MINGW
#include <time.h>
#include <direct.h>
#endif

tree texmacs_settings = tuple ();
int  install_status   = 0;
bool use_which        = false;
bool use_locate       = false;

extern void setup_tex (); // from Plugins/Metafont/tex_init.cpp
extern void init_tex  (); // from Plugins/Metafont/tex_init.cpp

/******************************************************************************
* Subroutines for paths
******************************************************************************/

static url
get_env_path (string which) {
  return url ("$" * which);
}

static void
set_env_path (string which, url val) {
  //cout << which << " := " << val << "\n";
  if ((!is_none (val)) && (val->t != ""))
    set_env (which, as_string (val));
}

static url
get_env_path (string which, url def) {
  url val= get_env_path (which);
  if (is_none (val) || (val->t == "")) {
    set_env_path (which, def);
    return def;
  }
  return val;
}

static url
plugin_path (string which) {
  url base= "$TEXMACS_HOME_PATH:/etc/TeXmacs:$TEXMACS_PATH:/usr/share/TeXmacs";
  url search= base * "plugins" * url_wildcard ("*") * which;
  return expand (complete (search, "r"));
}

scheme_tree
plugin_list () {
  bool flag;
  array<string> a= read_directory ("$TEXMACS_PATH/plugins", flag);
  a << read_directory ("/etc/TeXmacs/plugins", flag);
  a << read_directory ("$TEXMACS_HOME_PATH/plugins", flag);
  a << read_directory ("/usr/share/TeXmacs/plugins", flag);
  merge_sort (a);
  int i, n= N(a);
  tree t (TUPLE);
  for (i=0; i<n; i++)
    if ((a[i] != ".") && (a[i] != "..") && ((i==0) || (a[i] != a[i-1])))
      t << a[i];
  return t;
}

/******************************************************************************
* Initialize main paths
******************************************************************************/

static void
init_main_paths () {
#ifdef OS_MINGW
  if (is_none (get_env_path ("TEXMACS_HOME_PATH", get_env ("APPDATA") * "/TeXmacs"))) {
#else
  if (is_none (get_env_path ("TEXMACS_HOME_PATH", "~/.TeXmacs"))) {
#endif
    boot_error << "\n";
    boot_error << "Installation problem: please send a bug report.\n";
    boot_error << "'TEXMACS_HOME_PATH' could not be set to '~/.TeXmacs'.\n";
    boot_error << "You may try to set this environment variable manually\n";
    boot_error << "\n";
    FAILED ("installation problem");
    exit (1);
  }
}

/******************************************************************************
* Directory for temporary files
******************************************************************************/

static string main_tmp_dir= "$TEXMACS_HOME_PATH/system/tmp";

static void
make_dir (url which) {
  if (is_none(which))
    return ;
  if (!is_directory (which)) {
    make_dir (head (which));
    mkdir (which);
  }
}

static url
url_temp_dir_sub () {
#ifdef OS_MINGW
  static url tmp_dir=
    url_system (main_tmp_dir) * url_system (as_string (time (NULL)));
#else
  static url tmp_dir=
    url_system (main_tmp_dir) * url_system (as_string ((int) getpid ()));
#endif
  return (tmp_dir);
}

url
url_temp_dir () {
  static url u;
  if (u == url_none()) {
    u= url_temp_dir_sub ();
    make_dir (u);
  }
  return u;
}

bool
process_running (int pid) {
  string cmd= "ps -p " * as_string (pid);
  string ret= eval_system (cmd);
  return occurs ("texmacs", ret) && occurs (as_string (pid), ret);
}

static void
clean_temp_dirs () {
  bool err= false;
  array<string> a= read_directory (main_tmp_dir, err);
#ifndef OS_MINGW
  for (int i=0; i<N(a); i++)
    if (is_int (a[i]))
      if (!process_running (as_int (a[i])))
        if (a[i] != as_string ((int) getpid ()))
          system ("rm -rf", url (main_tmp_dir) * url (a[i]));
#else
  /* delete the directories after 7 days */
  time_t ts = as_int (basename (url_temp_dir_sub ())) - (3600 * 24 * 7 );
  for (int i=0; i<N(a); i++)     
    if (is_int (a[i])) {
      time_t td= as_int (a[i]);
      if (td < ts) {
        url cur = url (main_tmp_dir) * url (a[i]);
        array<string> f= read_directory (cur, err);
        for (int j=0; j<N(f); j++) remove (cur * url (f[j]));
        rmdir (as_charp (as_string (cur)));
      }
    }
#endif
}

/******************************************************************************
* Make user directories
******************************************************************************/

static void
init_user_dirs () {
  make_dir ("$TEXMACS_HOME_PATH");
  make_dir ("$TEXMACS_HOME_PATH/bin");
  make_dir ("$TEXMACS_HOME_PATH/doc");
  make_dir ("$TEXMACS_HOME_PATH/doc/about");
  make_dir ("$TEXMACS_HOME_PATH/doc/about/changes");
  make_dir ("$TEXMACS_HOME_PATH/fonts");
  make_dir ("$TEXMACS_HOME_PATH/fonts/enc");
  make_dir ("$TEXMACS_HOME_PATH/fonts/error");
  make_dir ("$TEXMACS_HOME_PATH/fonts/pk");
  make_dir ("$TEXMACS_HOME_PATH/fonts/tfm");
  make_dir ("$TEXMACS_HOME_PATH/fonts/truetype");
  make_dir ("$TEXMACS_HOME_PATH/fonts/type1");
  make_dir ("$TEXMACS_HOME_PATH/fonts/unpacked");
  make_dir ("$TEXMACS_HOME_PATH/fonts/virtual");
  make_dir ("$TEXMACS_HOME_PATH/langs");
  make_dir ("$TEXMACS_HOME_PATH/langs/mathematical");
  make_dir ("$TEXMACS_HOME_PATH/langs/mathematical/syntax");
  make_dir ("$TEXMACS_HOME_PATH/langs/natural");
  make_dir ("$TEXMACS_HOME_PATH/langs/natural/dic");
  make_dir ("$TEXMACS_HOME_PATH/langs/natural/hyphen");
  make_dir ("$TEXMACS_HOME_PATH/langs/programming");
  make_dir ("$TEXMACS_HOME_PATH/misc");
  make_dir ("$TEXMACS_HOME_PATH/misc/patterns");
  make_dir ("$TEXMACS_HOME_PATH/misc/pixmaps");
  make_dir ("$TEXMACS_HOME_PATH/packages");
  make_dir ("$TEXMACS_HOME_PATH/plugins");
  make_dir ("$TEXMACS_HOME_PATH/progs");
  make_dir ("$TEXMACS_HOME_PATH/server");
  make_dir ("$TEXMACS_HOME_PATH/styles");
  make_dir ("$TEXMACS_HOME_PATH/system");
  make_dir ("$TEXMACS_HOME_PATH/system/bib");
  make_dir ("$TEXMACS_HOME_PATH/system/cache");
  make_dir ("$TEXMACS_HOME_PATH/system/database");
  make_dir ("$TEXMACS_HOME_PATH/system/database/bib");
  make_dir ("$TEXMACS_HOME_PATH/system/make");
  make_dir ("$TEXMACS_HOME_PATH/system/tmp");
  make_dir ("$TEXMACS_HOME_PATH/texts");
  make_dir ("$TEXMACS_HOME_PATH/users");
  change_mode ("$TEXMACS_HOME_PATH/server", 7 << 6);
  change_mode ("$TEXMACS_HOME_PATH/system", 7 << 6);
  change_mode ("$TEXMACS_HOME_PATH/users", 7 << 6);
  clean_temp_dirs ();
}

/******************************************************************************
* Boot locks
******************************************************************************/

static void
acquire_boot_lock () {
  //cout << "Acquire lock\n";
  url lock_file= "$TEXMACS_HOME_PATH/system/boot_lock";
  if (exists (lock_file)) {
    remove (url ("$TEXMACS_HOME_PATH/system/settings.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/setup.scm"));
    remove (url ("$TEXMACS_HOME_PATH/system/cache") * url_wildcard ("*"));
    remove (url ("$TEXMACS_HOME_PATH/fonts/error") * url_wildcard ("*"));    
  }
  else save_string (lock_file, "", false);
}

void
release_boot_lock () {
  //cout << "Release lock\n";
  url lock_file= "$TEXMACS_HOME_PATH/system/boot_lock";
  remove (lock_file);
}

/******************************************************************************
* Detection of guile
******************************************************************************/

static void
init_guile () {
  url guile_path= "$TEXMACS_PATH/progs:$GUILE_LOAD_PATH";
  if (!exists (guile_path * "init-texmacs.scm")) {
    boot_error << "\n";
    boot_error << "Installation problem: please send a bug report.\n";
    boot_error << "The initialization file init-texmacs.scm"
               << " could not be found.\n";
    boot_error << "Please check the values of the environment variables\n";
    boot_error << "TEXMACS_PATH and GUILE_LOAD_PATH."
               << " init-texmacs.scm should\n";
    boot_error << "be readable and in the directory $TEXMACS_PATH/progs\n";
    boot_error << "or in the directory $GUILE_LOAD_PATH\n";
    boot_error << "\n";
    FAILED ("guile could not be found");
  }

  /*
  if (!exists ("$GUILE_LOAD_PATH/ice-9/boot-9.scm")) {
    int i;
    string guile_data    = var_eval_system ("guile-config info datadir");
    string guile_version = var_eval_system ("guile --version");
    for (i=0; i<N(guile_version); i++)
      if (guile_version[i] == '\n') break;
    guile_version= guile_version (0, i);
    for (i=N(guile_version); i>0; i--)
      if (guile_version[i-1] == ' ') break;
    guile_version= guile_version (i, N (guile_version));
    if (guile_version == "") {
      var_eval_system ("guile-config info top_srcdir");
      for (i=N(guile_version); i>0; i--)
        if (guile_version[i-1] == '-') break;
      guile_version= guile_version (i, N (guile_version));
      for (i=0; i<N(guile_version); i++)
        if ((guile_version[i] == '/') || (guile_version[i] == '\\')) {
          guile_version= guile_version (0, i);
          break;
        }
    }
    url guile_dir= url_system (guile_data) * url ("guile", guile_version);
    guile_path= guile_path | guile_dir;
    set_env_path ("GUILE_LOAD_PATH", guile_path);
    if (!exists ("$GUILE_LOAD_PATH/ice-9/boot-9.scm")) {
      failed_error << "\nGUILE_LOAD_PATH=" << guile_path << "\n";
      FAILED ("guile seems not to be installed on your system");
    }
  }
  */

  guile_path= guile_path | "$TEXMACS_HOME_PATH/progs" | plugin_path ("progs");
  set_env_path ("GUILE_LOAD_PATH", guile_path);
}

/******************************************************************************
* Set additional environment variables
******************************************************************************/

static void
init_env_vars () {
  // Handle binary, library and guile paths for plugins
  url bin_path= get_env_path ("PATH") | plugin_path ("bin");
#ifdef OS_MINGW
  bin_path= bin_path | url ("$TEXMACS_PATH/bin");
#endif
  set_env_path ("PATH", bin_path);
  url lib_path= get_env_path ("LD_LIBRARY_PATH") | plugin_path ("lib");
  set_env_path ("LD_LIBRARY_PATH", lib_path);

  // Get TeXmacs style and package paths
  url style_root=
    get_env_path ("TEXMACS_STYLE_ROOT",
                  "$TEXMACS_HOME_PATH/styles:$TEXMACS_PATH/styles" |
                  plugin_path ("styles"));
  url package_root=
    get_env_path ("TEXMACS_PACKAGE_ROOT",
                  "$TEXMACS_HOME_PATH/packages:$TEXMACS_PATH/packages" |
                  plugin_path ("packages"));
  url all_root= style_root | package_root;
  url style_path=
    get_env_path ("TEXMACS_STYLE_PATH",
                  search_sub_dirs (all_root));
  url text_root=
    get_env_path ("TEXMACS_TEXT_ROOT",
                  "$TEXMACS_HOME_PATH/texts:$TEXMACS_PATH/texts" |
                  plugin_path ("texts"));
  url text_path=
    get_env_path ("TEXMACS_TEXT_PATH",
                  search_sub_dirs (text_root));

  // Get other data paths
  (void) get_env_path ("TEXMACS_FILE_PATH",text_path | style_path);
  (void) set_env_path ("TEXMACS_DOC_PATH",
                       get_env_path ("TEXMACS_DOC_PATH") |
                       "$TEXMACS_HOME_PATH/doc:$TEXMACS_PATH/doc" |
                       plugin_path ("doc"));
  (void) set_env_path ("TEXMACS_SECURE_PATH",
                       get_env_path ("TEXMACS_SECURE_PATH") |
                       "$TEXMACS_PATH:$TEXMACS_HOME_PATH");
  (void) get_env_path ("TEXMACS_PATTERN_PATH",
                       "$TEXMACS_HOME_PATH/misc/patterns" |
                       url ("$TEXMACS_PATH/misc/patterns") |
                       url ("$TEXMACS_PATH/misc/pictures") |
                       plugin_path ("misc/patterns"));
  (void) get_env_path ("TEXMACS_PIXMAP_PATH",
                       "$TEXMACS_HOME_PATH/misc/pixmaps" |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/32x32/settings") |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/32x32/table") |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/24x24/main") |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/20x20/mode") |
                       url ("$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus") |
                       url ("$TEXMACS_PATH/misc/pixmaps/traditional/--x17") |
                       plugin_path ("misc/pixmaps"));
  (void) get_env_path ("TEXMACS_DIC_PATH",
                       "$TEXMACS_HOME_PATH/langs/natural/dic" |
                       url ("$TEXMACS_PATH/langs/natural/dic") |
                       plugin_path ("langs/natural/dic"));
#ifdef OS_WIN32
  set_env ("TEXMACS_SOURCE_PATH", "");
#else
  set_env ("TEXMACS_SOURCE_PATH", TEXMACS_SOURCES);
#endif
}

/******************************************************************************
* Miscellaneous initializations
******************************************************************************/

static void
init_misc () {
  // Test whether 'which' works
#ifdef OS_MINGW
  use_which = false;
#else
  use_which = (var_eval_system ("which texmacs 2> /dev/null") != "");
#endif
  //string loc= var_eval_system ("locate bin/locate 2> /dev/null");
  //use_locate= (search_forwards ("bin/locate", loc) > 0);

  // Set extra environment variables for Cygwin
#ifdef OS_CYGWIN
  set_env ("CYGWIN", "check_case:strict");
  set_env ("COMSPEC", "");
  set_env ("ComSpec", "");
#endif

}

static void
setup_inkscape_extension () {
debug_boot << "attempt install of inkscape extension \n ";
#ifdef OS_MINGW
  url ink_ext = url ("$APPDATA/inkscape/extensions");
#else
  url ink_ext = "~/.config/inkscape/extensions/";
#endif 
  if ( exists (ink_ext)) {
    url f1 = url (ink_ext * "texmacs.inx");
    url f2 = url (ink_ext * "texmacs_reedit.py");
    url f3 = url (ink_ext * "texmacs_latex.sty");
    url plug_source = url ("$TEXMACS_PATH/misc/inkscape_extension/");
    debug_boot << "installing or updating inkscape extension\n";
    copy (url (plug_source * "texmacs.inx"), f1);
    copy (url (plug_source * "texmacs_reedit.py"), f2);
    copy (url (plug_source * "texmacs_latex.sty"), f3);
    if (!(exists (f1) && exists (f2))) {
      debug_boot << "automatic install of inkscape extension failed\n; ";
      debug_boot << "see documentation for manual install\n";
    }
  }
}

/******************************************************************************
* Deprecated initializations
******************************************************************************/

static void
init_deprecated () {
#ifndef OS_WIN32
  // Check for Macaulay 2
  if (get_env ("M2HOME") == "")
    if (exists_in_path ("M2")) {
      string where= concretize (resolve_in_path ("M2"));
      string s    = var_eval_system ("grep 'M2HOME=' " * where);
      string dir  = s (search_forwards ("=", s) + 1, N(s));
      if (dir != "") set_env ("M2HOME", dir);
    }
#endif
}

/******************************************************************************
* Subroutines for the TeXmacs settings
******************************************************************************/

string
get_setting (string var, string def) {
  int i, n= N (texmacs_settings);
  for (i=0; i<n; i++)
    if (is_tuple (texmacs_settings[i], var, 1)) {
      return scm_unquote (as_string (texmacs_settings[i][1]));
    }
  return def;
}

void
set_setting (string var, string val) {
  int i, n= N (texmacs_settings);
  for (i=0; i<n; i++)
    if (is_tuple (texmacs_settings[i], var, 1)) {
      texmacs_settings[i][1]= scm_quote (val);
      return;
    }
  texmacs_settings << tuple (var, scm_quote (val));
}

/******************************************************************************
* First installation
******************************************************************************/

void
setup_texmacs () {
  url settings_file= "$TEXMACS_HOME_PATH/system/settings.scm";
  debug_boot << "Welcome to TeXmacs " TEXMACS_VERSION "\n";
  debug_boot << HRULE;

  set_setting ("VERSION", TEXMACS_VERSION);
  setup_tex ();
  setup_inkscape_extension ();
  
  string s= scheme_tree_to_block (texmacs_settings);
  //cout << "settings_t= " << texmacs_settings << "\n";
  //cout << "settings_s= " << s << "\n";
  if (save_string (settings_file, s) || load_string (settings_file, s, false)) {
    failed_error << HRULE;
    failed_error << "I could not save or reload the file\n\n";
    failed_error << "\t" << settings_file << "\n\n";
    failed_error << "Please give me full access control over this file and\n";
    failed_error << "rerun 'TeXmacs'.\n";
    failed_error << HRULE;
    FAILED ("unable to write settings");
  }
  
  debug_boot << HRULE;
  debug_boot << "Installation completed successfully !\n";
  debug_boot << "I will now start up the editor\n";
  debug_boot << HRULE;
}

/******************************************************************************
* Initialization of TeXmacs
******************************************************************************/

void
init_texmacs () {
  //cout << "Initialize -- Main paths\n";
  init_main_paths ();
  //cout << "Initialize -- User dirs\n";
  init_user_dirs ();
  //cout << "Initialize -- Boot lock\n";
  acquire_boot_lock ();
  //cout << "Initialize -- Succession status table\n";
  init_succession_status_table ();
  //cout << "Initialize -- Succession standard DRD\n";
  init_std_drd ();
  //cout << "Initialize -- User preferences\n";
  load_user_preferences ();
  //cout << "Initialize -- Guile\n";
  init_guile ();
  //cout << "Initialize -- Environment variables\n";
  init_env_vars ();
  //cout << "Initialize -- Miscellaneous\n";
  init_misc ();
  //cout << "Initialize -- Deprecated\n";
  init_deprecated ();
}

/******************************************************************************
* Initialization of built-in plug-ins
******************************************************************************/

void
init_plugins () {
  url old_settings= "$TEXMACS_HOME_PATH/system/TEX_PATHS";
  url new_settings= "$TEXMACS_HOME_PATH/system/settings.scm";

  install_status= 0;
  string s;
  if (load_string (new_settings, s, false)) {
    if (load_string (old_settings, s, false)) {
      setup_texmacs ();
      install_status= 1;
    }
    else get_old_settings (s);
  }
  else texmacs_settings= block_to_scheme_tree (s);

  if (get_setting ("VERSION") != TEXMACS_VERSION) {
    init_upgrade ();
    url ch ("$TEXMACS_HOME_PATH/doc/about/changes/changes-recent.en.tm");
    install_status= exists (ch)? 2: 0;
  }
  init_tex ();
}
