
/******************************************************************************
* MODULE     : tex_init.cpp
* DESCRIPTION: initializations for using Metafont
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boot.hpp"
#include "file.hpp"
#include "path.hpp"
#include "sys_utils.hpp"
#include "convert.hpp"
#include "tex_files.hpp"
#include "Freetype/free_type.hpp"

/******************************************************************************
* Determine installed programs
******************************************************************************/

static void
init_helper_binaries () {
  if (exists_in_path ("kpsepath")) {
    cerr << "TeXmacs] kpsepath works with your TeX distribution\n";
    set_setting ("KPSEPATH", "true");
  }
  else set_setting ("KPSEPATH", "false");

  if (exists_in_path ("kpsewhich")) {
    cerr << "TeXmacs] kpsewhich works with your TeX distribution\n";
    set_setting ("KPSEWHICH", "true");
  }
  else set_setting ("KPSEWHICH", "false");

  if (exists_in_path ("mktextfm")) { 	 
   cerr << "TeXmacs] mktextfm works with your TeX distribution\n"; 	 
   set_setting ("MAKETFM", "mktextfm"); 	 
  } 	 
  else if (exists_in_path ("MakeTeXTFM")) { 	 
    cerr << "TeXmacs] MakeTeXTFM works with your TeX distribution\n"; 	 
    set_setting ("MAKETFM", "MakeTeXTFM"); 	 
  } 	 
  else if (exists_in_path ("maketfm")){ 	 
    cerr << "TeXmacs] maketfm works with your TeX distribution\n"; 	 
    set_setting ("MAKETFM", "maketfm"); 	 
  } 	 
  else set_setting ("MAKETFM", "false");
  
  if (exists_in_path ("mktexpk")) { 	 
    cerr << "TeXmacs] mktexpk works with your TeX distribution\n"; 	 
    set_setting ("MAKEPK", "mktexpk"); 	 
  } 	 
  else if (exists_in_path ("MakeTeXPK")) { 	 
    cerr << "TeXmacs] MakeTeXPK works with your TeX distribution\n"; 	 
    set_setting ("MAKEPK", "MakeTeXPK"); 	 
  } 	 
  else if (exists_in_path ("makepk")){ 	 
    cerr << "TeXmacs] makepk works with your TeX distribution\n"; 	 
    set_setting ("MAKEPK", "makepk"); 	 
  } 	 
  else set_setting ("MAKEPK", "false");

  if (exists_in_path ("texhash")) { 	 
    cerr << "TeXmacs] texhash works with your TeX distribution\n"; 	 
    set_setting ("TEXHASH", "true"); 	 
  } 	 
  else set_setting ("TEXHASH", "false");
  
  set_setting ("DPI", "600");
}

/******************************************************************************
* Heuristic determination of path with TeX files
******************************************************************************/

static void
search_sub_dirs_sub (url base, url u, url& tfm, url& pk, url& pfb, int status) {
  if (is_concat (u)) {
    if (u[1] == "tfm") status= 1;
    if (u[1] == "pk" ) status= 2;
    if (u[1] == "pfb" || u[1] == "type1") status= 3;
    search_sub_dirs_sub (base * u[1], u[2], tfm, pk, pfb, status);
  }
  if (is_or (u)) {
    search_sub_dirs_sub (base, u[2], tfm, pk, pfb, status);
    search_sub_dirs_sub (base, u[1], tfm, pk, pfb, status);
  }
  if ((status == 1) || (u == "tfm")) tfm= (base * u) | tfm;
  if ((status == 2) || (u == "pk" )) pk = (base * u) | pk;
  if ((status == 3) || (u == "pfb") || (u == "pfb")) pfb = (base * u) | pfb;
}

static void
search_sub_dirs (url root, url& tfm, url& pk, url& pfb) {
  url dirs= complete (root * url_wildcard (), "dr");
  if (!is_none (dirs)) {
    cerr << "TeXmacs] found TeX directory " << root << "\n";
    search_sub_dirs_sub (url_here (), dirs, tfm, pk, pfb, 0);
  }
}

#ifdef OS_WIN32
static url
search_sub_dirs (url root) {
  url dirs= complete (root * url_wildcard (), "dr");
  return expand (dirs);
}
#endif

static void
init_heuristic_tex_paths () {
  url tfm= url_none (), pk= url_none (), pfb= url_none ();

  // Try some 'standard' directories
#ifdef OS_WIN32
  tfm= search_sub_dirs ("$TEX_HOME/fonts/tfm");
  pk = search_sub_dirs ("$TEX_HOME/fonts/pk");
  pfb= search_sub_dirs ("$TEX_HOME/fonts/type1");
#else
  search_sub_dirs ("/opt/local/share/texmf-texlive-dist/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/lib/tetex/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/lib/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/local/lib/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/share/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/usr/TeX/lib/texmf/fonts", tfm, pk, pfb);
  search_sub_dirs ("/var/texfonts", tfm, pk, pfb);
  search_sub_dirs ("/var/tmp/texfonts", tfm, pk, pfb);
#endif

#ifdef OS_WIN32
  set_setting ("TFM", as_string (tfm));
  set_setting ("PK" , as_string (pk ));
  set_setting ("PFB", as_string (pfb));
#else
  set_setting ("TFM", as_string (expand (factor (tfm))));
  set_setting ("PK" , as_string (expand (factor (pk ))));
  set_setting ("PFB", as_string (expand (factor (pfb))));
#endif
}

/******************************************************************************
* Setting up and initializing TeX fonts
******************************************************************************/

void
setup_tex () {
  remove ("$TEXMACS_HOME_PATH/fonts/font-index.scm");
  init_helper_binaries ();
  init_heuristic_tex_paths ();
}

void
init_tex () {
  reset_tfm_path (false);
  reset_pk_path (false);
  reset_pfb_path ();
}
