
/******************************************************************************
* MODULE     : init_first.cpp
* DESCRIPTION: initializations for the first launch of TeXmacs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "boot.hpp"
#include "file.hpp"
#include "path.hpp"
#include "sys_utils.hpp"
#include "tex_files.hpp"
#include "convert.hpp"

/******************************************************************************
* Determine installed programs
******************************************************************************/

static void
init_helper_binaries () {
  if (exists_in_path ("kpsepath")) {
    cerr << "TeXmacs] kpsepath works with your TeX distribution\n";
    set_setting ("KPSEPATH", "true");
  }
  else {
    cerr << "TeXmacs] kpsepath does not work with your TeX distribution\n";
    set_setting ("KPSEPATH", "false");
  }

  if (exists_in_path ("kpsewhich")) {
    cerr << "TeXmacs] kpsewhich works with your TeX distribution\n";
    set_setting ("KPSEWHICH", "true");
  }
  else {
    cerr << "TeXmacs] kpsewhich does not work with your TeX distribution\n";
    set_setting ("KPSEWHICH", "false");
  }

  if (exists_in_path ("mktextfm")) {
    cerr << "TeXmacs] mktextfm works with your TeX distribution\n";
    set_setting ("MAKETFM", "mktextfm");
  }
  else if (exists_in_path ("MakeTeXTFM")) {
    cerr << "TeXmacs] MakeTeXTFM works with your TeX distribution\n";
    set_setting ("MAKETFM", "MakeTeXTFM");
  }
  else {
    cerr << "TeXmacs] MakeTeXTFM does not work with your TeX distribution\n";
    set_setting ("MAKETFM", "false");
  }

  if (exists_in_path ("mktexpk")) {
    cerr << "TeXmacs] mktexpk works with your TeX distribution\n";
    set_setting ("MAKEPK", "mktexpk");
  }
  else if (exists_in_path ("MakeTeXPK")) {
    cerr << "TeXmacs] MakeTeXPK works with your TeX distribution\n";
    set_setting ("MAKEPK", "MakeTeXPK");
  }
  else {
    cerr << "TeXmacs] MakeTeXPK does not work with your TeX distribution\n";
    set_setting ("MAKEPK", "false");
  }

  if (exists_in_path ("texhash")) {
    cerr << "TeXmacs] texhash works with your TeX distribution\n";
    set_setting ("TEXHASH", "true");
  }
  else {
    cerr << "TeXmacs] texhash does not work with your TeX distribution\n";
    set_setting ("TEXHASH", "false");
  }
}

/******************************************************************************
* Heuristic determination of path with TeX files
******************************************************************************/

static void
locate (string name, url& p) {
#ifdef OS_WIN32
  (void) name; (void) p;
#else
  int start=0, i;
  string s= eval_system ("locate " * name);
  for (i=0; i<N(s); i++)
    if (s[i]=='\n') {
      int j;
      for (j=i-1; j>start; j--)
	if ((s[j] == '/') || (s[j] == '\\')) break;
      p= url_system (s (start, j)) | p;
      start= i+1;
    }
#endif
}

static void
search_sub_dirs_sub (url base, url u, url& tfm, url& pk, int status) {
  if (is_concat (u)) {
    if (u[1] == "tfm") status= 1;
    if (u[1] == "pk")  status= 2;
    search_sub_dirs_sub (base * u[1], u[2], tfm, pk, status);
  }
  if (is_or (u)) {
    search_sub_dirs_sub (base, u[2], tfm, pk, status);
    search_sub_dirs_sub (base, u[1], tfm, pk, status);
  }
  if ((status == 1) || (u == "tfm")) tfm= (base * u) | tfm;
  if ((status == 2) || (u == "pk" )) pk = (base * u) | pk;
}

static void
search_sub_dirs (url root, url& tfm, url& pk) {
  url dirs= complete (root * url_wildcard (), "dr");
  if (!is_none (dirs)) {
    cerr << "TeXmacs] found TeX directory " << root << "\n";
    search_sub_dirs_sub (url_here (), dirs, tfm, pk, 0);
  }
}

static void
init_heuristic_tex_paths () {
  // Not necessary if we can use kpsepath
  if (get_setting ("KPSEPATH") == "true") {
    set_setting ("TFM", "");
    set_setting ("PK", "");
    return;
  }

  // Try locate
  url tfm= url_none (), pk= url_none ();
  string test= eval_system ("locate cmr10.tfm");
  if (N(test) == 0)
    cerr << "TeXmacs] locate does not work; I will try something else\n";
  else {
    locate (".tfm", tfm);
    locate (".300pk", pk);
    locate (".360pk", pk);
    locate (".400pk", pk);
    locate (".600pk", pk);
    locate (".1200pk", pk);
    if (is_none (tfm)) cerr << "TeXmacs] I could not locate any tfm files\n";
    else cerr << "TeXmacs] located tfm files in " << tfm << "\n";
    if (is_none (pk)) cerr << "TeXmacs] I could not locate any pk files\n";
    else cerr << "TeXmacs] located pk files in " << pk << "\n";
  }

  // Try some 'standard' directories
#ifdef OS_WIN32
  search_sub_dirs (url_system ("c:\texmf"), tfm, pk);
  search_sub_dirs (url_system ("d:\texmf"), tfm, pk);
#else
  search_sub_dirs ("/usr/lib/tetex/fonts", tfm, pk);
  search_sub_dirs ("/usr/lib/texmf/fonts", tfm, pk);
  search_sub_dirs ("/var/texfonts", tfm, pk);
  search_sub_dirs ("/var/tmp/texfonts", tfm, pk);
  search_sub_dirs ("/usr/TeX/lib/texmf/fonts", tfm, pk);
  search_sub_dirs ("/usr/local/lib/texmf/fonts", tfm, pk);
  search_sub_dirs ("/usr/share/texmf/fonts", tfm, pk);
#endif

  // Does TeX work?
  if ((is_none (tfm) && (get_env ("TEX_TFM_PATH") == "")) ||
      (is_none (pk ) && (get_env ("TEX_PK_PATH" ) == ""))) {
    cerr << HRULE;
    cerr << "I could not find a TeX system on your system\n";
    cerr << "If you did install one, please set the system variables\n\n";
    cerr << "\tTEX_TFM_PATH\n";
    cerr << "\tTEX_PK_PATH\n\n";
    cerr << "with the paths where the tfm resp. pk file\n";
    cerr << "can be found on your system and restart TeXmacs\n";
    cerr << HRULE;
    exit (1);
  }

  // Done
  set_setting ("TFM", as_string (expand (factor (tfm))));
  set_setting ("PK", as_string (expand (factor (pk))));
}

/******************************************************************************
* Determine default TeX settings
******************************************************************************/

static bool
try_dpi (int dpi, int test) {
  cerr << "TeXmacs] Trying to create ecrm10." << test
       << "pk from " << dpi << " dpi\n";
  make_tex_pk ("ecrm10", test, dpi, "localfont");
  reset_pk_path ();
  if (!is_none (resolve_pk ("ecrm10." * as_string (test) * "pk"))) {
    set_setting ("DPI", as_string (dpi));
    set_setting ("EC", "true");
    cerr << "TeXmacs] Metafont works with " << dpi << " dpi ec-fonts\n";
    return true;
  }

  cerr << "TeXmacs] Trying to create cmr10." << test
       << "pk from " << dpi << " dpi\n";
  make_tex_pk ("cmr10", test, dpi, "localfont");
  reset_pk_path ();
  if (!is_none (resolve_pk ("cmr10." * as_string (test) * "pk"))) {
    set_setting ("DPI", as_string (dpi));
    cerr << "TeXmacs] Metafont works with " << dpi << " dpi cm-fonts\n";
    return true;
  }

  return false;
}

static void
init_default_tex_settings () {
  set_setting ("DPI", "300");
  set_setting ("EC", "false");
  if (get_setting ("MAKEPK") != "false") {
    if (try_dpi (300, 123));
    else if (try_dpi (600, 234));
    else if (try_dpi (1200, 345));
    else cerr << "TeXmacs] Your mktexpk/MakeTeXPK does not seem to work\n";
  }
}

/******************************************************************************
* First installation
******************************************************************************/

void
init_first () {
  url settings_file= "$TEXMACS_HOME_PATH/system/settings.scm";
  cerr << "Welcome to TeXmacs " TEXMACS_VERSION "\n";
  cerr << HRULE;
  cerr << "Since this seems to be the first time you run this\n";
  cerr << "version of TeXmacs, I will first analyze your system\n";
  cerr << "in order to set up some TeX paths in the correct way.\n";
  cerr << "This may take some seconds; the result can be found in\n\n";
  cerr << "\t" << settings_file << "\n\n";
  cerr << HRULE;

  set_setting ("VERSION", TEXMACS_VERSION);
  init_helper_binaries ();
  init_heuristic_tex_paths ();
  init_default_tex_settings ();
  
  string s= scheme_tree_to_block (texmacs_settings);
  if (save_string (settings_file, s) || load_string (settings_file, s)) {
    cerr << HRULE;
    cerr << "I could not save or reload the file\n\n";
    cerr << "\t" << settings_file << "\n\n";
    cerr << "Please give me full access control over this file and\n";
    cerr << "rerun 'TeXmacs'.\n";
    cerr << HRULE;
    exit (1);
  }
  
  cerr << HRULE;
  cerr << "Installation completed successfully !\n";
  cerr << "I will now start up the editor\n";
  cerr << HRULE;
}
