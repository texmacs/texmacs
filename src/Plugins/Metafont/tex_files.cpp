
/******************************************************************************
* MODULE     : tex_files.cpp
* DESCRIPTION: manipulation of TeX font files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tex_files.hpp"
#include "boot.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "path.hpp"
#include "hashmap.hpp"
#include "analyze.hpp"

static url the_tfm_path= url_none ();
static url the_pk_path = url_none ();

/******************************************************************************
* Finding a TeX font
******************************************************************************/

url
resolve_tfm (url name) {
  if (get_setting ("KPSEWHICH") == "true") {
    string which= var_eval_system ("kpsewhich " * as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
  return resolve (the_tfm_path * name);
}

url
resolve_pk (url name) {
  if (get_setting ("KPSEWHICH") == "true") {
    string which= var_eval_system ("kpsewhich " * as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
  return resolve (the_pk_path * name);
}

bool
exists_in_tex (url u) {
  // Weak check for TeXmacs menus
  static hashmap<string,int> tex_file_table (false);
  string s= as_string (u);
  if (get_setting ("KPSEWHICH") != "true") return true;
  if (tex_file_table->contains (s)) return tex_file_table [s];
  tex_file_table(s)= (var_eval_system ("kpsewhich " * s) != "");
  return tex_file_table [s];
}

/******************************************************************************
* Automatically generate missing fonts
******************************************************************************/

void
make_tex_tfm (string name) {
  string s;
  if (get_setting ("MAKETFM") == "MakeTeXTFM") {
    s= "MakeTeXTFM " * name;
    if (DEBUG_AUTO) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
  if (get_setting ("MAKETFM") == "mktextfm") {
    s= "mktextfm " * name;
    if (DEBUG_AUTO) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
  if (get_setting ("MAKETFM") == "maketfm"){
    if (name(N(name) - 4, N(name)) == ".tfm")
      name = name (0, N(name) - 4);
    s = "maketfm --dest-dir \"" * get_env("$TEXMACS_HOME_PATH")
      * "\\fonts\\tfm\" " * name;
    if (DEBUG_AUTO) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
}

void
make_tex_pk (string name, int dpi, int design_dpi, string where) {
  string s;
  if (get_setting ("MAKEPK") == "MakeTeXPK") {
    s="MakeTeXPK " * name * " " *
      as_string (dpi) * " " * as_string (design_dpi) * " " *
      as_string (dpi) * "/" * as_string (design_dpi) * " " * where;
    if (DEBUG_AUTO) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
  if (get_setting ("MAKEPK") == "mktexpk") {
    s="mktexpk " *
      string ("--dpi ") * as_string (dpi) * " " *
      string ("--bdpi ") * as_string (design_dpi) * " " *
      string ("--mag ") * as_string (dpi)*"/"*as_string (design_dpi) * " " *
      (where == ""? string (""): string ("--destdir ") * where) * " " *
      name;
    if (DEBUG_AUTO) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
  if (get_setting ("MAKEPK") == "makepk"){
    s = "makepk --dest-dir \""
      * get_env("$TEXMACS_HOME_PATH") * "\\fonts\\pk\" "
      * name * " " * as_string(dpi) * " " * as_string(design_dpi)
      * " " * as_string(dpi) * "/" * as_string(design_dpi);
    if (DEBUG_AUTO) cout << "TeXmacs] Executing " << s << "\n";
    system (s);
  }
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
    p= expand (complete (dir * url_wildcard (), "dr")) | p;
  }
  return p;
}

void
reset_tfm_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string tfm= get_setting ("TFM");
  the_tfm_path=
    url_here () |
    "$TEX_TFM_PATH:$TEXMACS_HOME_PATH/fonts/tfm:$TEXMACS_PATH/fonts/tfm" |
    (tfm == ""? url_none (): tfm);
  if ((get_setting ("MAKETFM") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    the_tfm_path= the_tfm_path | get_kpsepath ("tfm");
  the_tfm_path= expand (factor (the_tfm_path));
}

void
reset_pk_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string pk= get_setting ("PK");
  the_pk_path=
    url_here () |
    "$TEX_PK_PATH:$TEXMACS_HOME_PATH/fonts/pk:$TEXMACS_PATH/fonts/pk" |
    (pk == ""? url_none (): pk);
  if ((get_setting ("MAKEPK") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    the_pk_path= the_pk_path | get_kpsepath ("pk");
  the_pk_path= expand (factor (the_pk_path));
}

/******************************************************************************
* Load true type substitute for pk font
******************************************************************************/

static hashmap<string,string> ec2cm ("?");

static void
initialize_ec2cm () {
  if (N(ec2cm) > 0) return;
  ec2cm ("ecrm")= "cmr";
  ec2cm ("ecsl")= "cmsl";
  ec2cm ("ecti")= "cmti";
  ec2cm ("ecff")= "cmff";
  ec2cm ("eccc")= "cmcsc";
  ec2cm ("ecdh")= "cmdunh";
  ec2cm ("ecui")= "cmu";
  ec2cm ("ecbx")= "cmbx";
  ec2cm ("ecbl")= "cmbxsl";
  ec2cm ("ecbi")= "cmbxti";
  ec2cm ("ecrb")= "cmbom";
  ec2cm ("ecxc")= "cmbcsc";
  ec2cm ("ectt")= "cmtt";
  ec2cm ("ecst")= "cmsltt";
  ec2cm ("ectc")= "cmtcsc";
  ec2cm ("ecvt")= "cmvtt";
  ec2cm ("ecss")= "cmss";
  ec2cm ("ecsi")= "cmssi";
  ec2cm ("eclq")= "cmssq";
  ec2cm ("ecli")= "cmssqi";
  ec2cm ("ecsx")= "cmssbx";
  ec2cm ("ecssdc")= "cmssdc";
}

void
ec_to_cm (string& name, unsigned char& c) {
  if (!starts (name, "ec")) return;
  if ((c < '\33') || (c == ' ') || (c == '\"') || (c == '<') || (c == '>') ||
      (c == '|') || (c == '^') || (c == '_') || (c > 'z')) return;
  initialize_ec2cm ();
  
  int pos= 0;
  while ((pos<N(name)) && (!is_numeric (name[pos]))) pos++;
  string root  = name (0, pos);
  string suffix= name (pos, N(name));
  if (!ec2cm->contains (root)) return;

  name= ec2cm[root] * suffix;
  if (c<' ') c -= 16;
}

/******************************************************************************
* Load true type substitute for pk font
******************************************************************************/

static bool
pfb_exists (string name) {
  return var_eval_system ("kpsewhich " * name * ".pfb") != "";
}

static string
find_pfb (string name) {
  if (pfb_exists (name)) return name;

  int pos= N(name);
  while ((pos>0) && is_numeric (name[pos-1])) pos--;
  string root= name (0, pos);
  string size= name (pos, N(name));
  if (size == "") return "";
  int sz= as_int (size);

  if (sz>99) return find_pfb (root * as_string (sz/100));
  if (sz>17) return find_pfb (root * "17");
  if (sz>14) return find_pfb (root * "14");
  if (sz>12) return find_pfb (root * "12");
  if (sz>10) return find_pfb (root * "10");
  if (sz< 5) return find_pfb (root * "5");
  if (sz< 6) return find_pfb (root * "6");
  if (sz< 7) return find_pfb (root * "7");
  if (sz< 8) return find_pfb (root * "8");
  if (sz< 9) return find_pfb (root * "9");
  if (sz<10) return find_pfb (root * "10");
  return "";
}

string
pk_to_true_type (string& name) {
  name= find_pfb (name);
  if (name == "") return "";
  string loc= var_eval_system ("kpsewhich " * name * ".pfb");
  return eval_system ("pfbtops " * loc);
}
