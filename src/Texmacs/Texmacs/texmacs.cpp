
/******************************************************************************
* MODULE     : texmacs.cpp
* DESCRIPTION: main program
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "boot.hpp"
#include "file.hpp"
#include "server.hpp"

extern bool   char_clip;
extern url    tm_init_file;
extern url    tm_init_buffer_file;
extern string my_init_cmds;

extern int geometry_w, geometry_h;
extern int geometry_x, geometry_y;

extern tree   the_et;

/******************************************************************************
* Real main program for encaptulation of guile
******************************************************************************/

void
TeXmacs_main (int argc, char** argv) {
  int i;
  bool flag= true;
  string the_default_font;
  for (i=1; i<argc; i++)
    if (((argv[i][0] == '-') || (argv[i][0] == '+')) && (argv[i][1] != '\0')) {
      string s= argv[i];
      if ((N(s)>=2) && (s(0,2)=="--")) s= s (1, N(s));
      if ((s == "-s") || (s == "-silent")) flag= false;
      else if ((s == "-d") || (s == "-debug")) debug (DEBUG_FLAG_STD, true);
      else if (s == "-debug-events") debug (DEBUG_FLAG_EVENTS, true);
      else if (s == "-debug-io") debug (DEBUG_FLAG_IO, true);
      else if (s == "-debug-all") {
	debug (DEBUG_FLAG_EVENTS, true);
	debug (DEBUG_FLAG_STD, true);
	debug (DEBUG_FLAG_IO, true);
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
      else if ((s == "-S") || (s == "-setup")) {
	remove ("$TEXMACS_HOME_PATH/system/settings.scm");
	remove ("$TEXMACS_HOME_PATH/system/setup.scm");
      }
      else if ((s == "-v") || (s == "-version")) {
	cout << "\n";
	cout << "TeXmacs version " << TEXMACS_VERSION << "\n";
	cout << TEXMACS_COPYRIGHT << "\n";
	cout << "\n";
	exit (0);
      }
      else if ((s == "-p") || (s == "-path")) {
	system ("echo $TEXMACS_PATH");
	exit (0);
      }
      else if ((s == "-bp") || (s == "-binpath")) {
	system ("echo $TEXMACS_BIN_PATH");
	exit (0);
      }
      else if ((s == "-q") || (s == "-quit"))
	my_init_cmds= my_init_cmds * " (quit-TeXmacs)";
      else if ((s == "-c") || (s == "-convert")) {
	i+=2;
	if (i<argc)
	  my_init_cmds= my_init_cmds * " " *
	    "(texmacs-load-buffer " * quote (argv[i-1]) *
	    " \"generic\" 0 #f) " *
	    "(export-buffer " * quote (argv[i]) * ")";
      }
      else if ((s == "-x") || (s == "-execute")) {
	i++;
	if (i<argc) my_init_cmds= (my_init_cmds * " ") * argv[i];
      }
      else if ((s == "-Oc") || (s == "-no-char-clipping")) char_clip= false;
      else if ((s == "+Oc") || (s == "-char-clipping")) char_clip= true;
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
	cout << "  -s         Suppress information messages\n";
	cout << "  -S         Rerun TeXmacs setup program before starting\n";
	cout << "  -v         Display current TeXmacs version\n";
	cout << "  -x [cmd]   Execute scheme command\n";
	cout << "  -Oc        TeX characters bitmap clipping off\n";
	cout << "  +Oc        TeX characters bitmap clipping on (default)\n";
	cout << "\nPlease report bugs to <bugs@texmacs.org>\n";
	cout << "\n";
	exit (0);
      }
    }
  if (flag) debug (DEBUG_FLAG_AUTO, true);

  if (DEBUG_AUTO) cout << "\n";
  if (DEBUG_STD) cout << "TeXmacs] Installing TeX...\n";
  init_plugins ();
  if (DEBUG_STD) cout << "TeXmacs] Opening display...\n";
  display dis= open_display (argc, argv);
  dis->set_default_font (the_default_font);
  if (DEBUG_STD) cout << "TeXmacs] Starting server...\n";
  server sv (dis);

  for (i=1; i<argc; i++) {
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
#ifndef OS_WIN32
    if ((my_init_cmds == "") &&
	exists ("$TEXMACS_HOME_PATH/system/autosave.tm"))
      sv->exec_delayed ("(interactive '(\"Recover autosave file (y/n)?\") 'conditional-recover-autosave)");
#endif
  }

  if (DEBUG_STD) cout << "TeXmacs] Starting event loop...\n";
  sv->delayed_autosave();
  dis->delayed_message (sv->get_meta(), "banner", 100);
  dis->event_loop ();

  if (DEBUG_STD) cout << "TeXmacs] Closing display...\n";
  close_display (dis);
  if (DEBUG_STD) cout << "TeXmacs] Good bye...\n";
}

/******************************************************************************
* Main program
******************************************************************************/

int
main (int argc, char** argv) {
  the_et     = tuple ();
  the_et->obs= ip_observer (path ());
  init_texmacs ();
  start_guile (argc, argv, TeXmacs_main);
  return 0;
}
