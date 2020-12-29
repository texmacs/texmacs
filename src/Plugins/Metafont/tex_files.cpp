
/******************************************************************************
* MODULE     : tex_files.cpp
* DESCRIPTION: manipulation of TeX font files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tex_files.hpp"
#include "boot.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "path.hpp"
#include "hashmap.hpp"
#include "analyze.hpp"
#include "tm_timer.hpp"
#include "data_cache.hpp"

static url the_tfm_path= url_none ();
static url the_pk_path = url_none ();
static url the_pfb_path= url_none ();

/******************************************************************************
* Finding a TeX font
******************************************************************************/

static string
kpsewhich (string name) {
  bench_start ("kpsewhich");
  string which= var_eval_system ("kpsewhich " * name);
  bench_cumul ("kpsewhich");
  return which;
}

static url
resolve_tfm (url name) {
  url r= resolve (the_tfm_path * name);
  if (!is_none (r)) return r;
  if (get_setting ("KPSEWHICH") == "true") {
    string which= kpsewhich (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
  return r;
}

static url
resolve_pk (url name) {
  url r= resolve (the_pk_path * name);
  if (!is_none (r)) return r;
#ifndef OS_WIN32 // The kpsewhich from MikTeX is bugged for pk fonts
  if (get_setting ("KPSEWHICH") == "true") {
    string which= kpsewhich (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
#endif
  return r;
}

static url
resolve_pfb (url name) {
  url r= resolve (the_pfb_path * name);
  if (!is_none (r)) return r;
#ifndef OS_WIN32 // The kpsewhich from MikTeX is bugged for pfb fonts
  if (get_setting ("KPSEWHICH") == "true") {
    string which= kpsewhich (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
#endif
  return r;
}

url
tfm_font_path () {
  return the_tfm_path;
}

/******************************************************************************
* Caching results
******************************************************************************/

url
resolve_tex (url name) {
  string s= as_string (name);
  if (is_cached ("font_cache.scm", s)) {
    url u= url_system (cache_get ("font_cache.scm", s) -> label);
    if (exists (u)) return u;
    cache_reset ("font_cache.scm", s);
  }

  bench_start ("resolve tex");
  url u= url_none ();
  if (ends (s, "mf" )) {
    u= resolve_tfm (name);
#ifdef OS_WIN32
    if (is_none (u))
      u= resolve_tfm (replace (s, ".mf", ".tfm"));
#endif
  }
  if (ends (s, "tfm")) u= resolve_tfm (name);
  if (ends (s, "pk" )) u= resolve_pk  (name);
  if (ends (s, "pfb")) u= resolve_pfb (name);
  bench_cumul ("resolve tex");

  if (!is_none (u)) cache_set ("font_cache.scm", s, as_string (u));
  //cout << "Resolve " << name << " -> " << u << "\n";
  return u;
}

bool
exists_in_tex (url u) {
  return !is_none (resolve_tex (u));
}

/******************************************************************************
* Automatically generate missing fonts
******************************************************************************/

void
make_tex_tfm (string name) {
  string s;
  int r= 0;
  if (get_setting ("MAKETFM") == "MakeTeXTFM") {
    s= "MakeTeXTFM " * name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (get_setting ("MAKETFM") == "mktextfm") {
    url tfm_dir ("$TEXMACS_HOME_PATH/fonts/tfm");
    s= "mktextfm " *
      string ("--destdir ") * as_string (tfm_dir) * " " *
      name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
    string superfluous= name * ".600pk";
    if (ends (name, ".tfm")) superfluous= name (0, N(name)-4) * ".600pk";
    remove (tfm_dir * superfluous);
  }
  if (get_setting ("MAKETFM") == "maketfm"){
    if (name(N(name) - 4, N(name)) == ".tfm")
      name = name (0, N(name) - 4);
    s = "maketfm --dest-dir \"" * get_env("$TEXMACS_HOME_PATH")
      * "\\fonts\\tfm\" " * name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (r) cout << "TeXmacs] system command failed: " << s << "\n";
}

void
make_tex_pk (string name, int dpi, int design_dpi) {
  string s;
  int r= 0;
  if (get_setting ("MAKEPK") == "MakeTeXPK") {
    s="MakeTeXPK " * name * " " *
      as_string (dpi) * " " * as_string (design_dpi) * " " *
      as_string (dpi) * "/" * as_string (design_dpi) * " localfont";
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (get_setting ("MAKEPK") == "mktexpk") {
    url pk_dir ("$TEXMACS_HOME_PATH/fonts/pk");
    s="mktexpk " *
      string ("--dpi ") * as_string (dpi) * " " *
      string ("--bdpi ") * as_string (design_dpi) * " " *
      string ("--mag ") * as_string (dpi) *"/"* as_string (design_dpi) * " " *
      string ("--destdir ") * as_string (pk_dir) * " " *
      name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (get_setting ("MAKEPK") == "makepk") {
#ifdef OS_WIN32
    s = "makepk --dest-dir \""
      * get_env("$TEXMACS_HOME_PATH") * "\\fonts\\pk\" "
      * name * " " * as_string(dpi) * " " * as_string(design_dpi)
      * " " * as_string(dpi) * "%//" * as_string(design_dpi);
#else
    s = "makepk --dest-dir \""
      * get_env("$TEXMACS_HOME_PATH") * "\\fonts\\pk\" "
      * name * " " * as_string(dpi) * " " * as_string(design_dpi)
      * " " * as_string(dpi) * "/" * as_string(design_dpi);
#endif
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (r) cout << "TeXmacs] system command failed: " << s << "\n";
}

/******************************************************************************
* Automatic determination of paths where TeX fonts might have been generated
******************************************************************************/

static url
get_kpsepath (string s) {
  // FIXME: adapt to WIN32
  if (get_setting ("KPSEPATH") != "true") return url_none ();
  string r= var_eval_system ("kpsepath " * s);
  if (N(r)==0) return url_none ();

  int i, start, end;
  url p= url_none ();
  for (i=0; i<N(r); i++) {
    while ((i<N(r)) && (r[i]=='!')) i++;
    start=i;
    while ((i<N(r)) && (r[i]!=':')) i++;
    end=i;
    while ((end>start) && (r[end-1]=='/')) end--;
    string dir= r (start, end);
    if (dir == ".") continue;
    p= search_sub_dirs (dir) | p;
  }
  return p;
}

void
reset_tfm_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string tfm= get_setting ("TFM");
  the_tfm_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/tfm") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/tfm") |
    "$TEX_TFM_PATH" |
    ((tfm == "" || tfm == "{}") ? url_none () : tfm);
  if ((get_setting ("MAKETFM") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    if (get_setting ("KPSEWHICH") != "true")
      the_tfm_path= the_tfm_path | get_kpsepath ("tfm");
  the_tfm_path= expand (factor (the_tfm_path));
}

void
reset_pk_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string pk= get_setting ("PK");
  the_pk_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/pk") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/pk") |
    "$TEX_PK_PATH" |
    (pk == ""? url_none (): pk);
  if ((get_setting ("MAKEPK") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    if (get_setting ("KPSEWHICH") != "true")
      the_pk_path= the_pk_path | get_kpsepath ("pk");
  the_pk_path= expand (factor (the_pk_path));
}

void
reset_pfb_path () {
  string pfb= get_setting ("PFB");
  the_pfb_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/type1") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/type1") |
    "$TEX_PFB_PATH" |
    (pfb == ""? url_none (): url_system (pfb));
  the_pfb_path= expand (factor (the_pfb_path));
}
