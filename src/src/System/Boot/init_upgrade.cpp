
/******************************************************************************
* MODULE     : init_upgrade.cpp
* DESCRIPTION: initializations which are only necessary when
*              you just upgraded your TeXmacs distribution
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

/******************************************************************************
* Old style settings files
******************************************************************************/

static string
line_read (string s, int& i) {
  int start= i, n= N(s);
  for (start=i; i<n; i++)
    if (s[i]=='\n') break;
  string r= s (start, i);
  if (i<n) i++;
  return r;
}

void
get_old_settings (string s) {
  int i= 0, j;
  while (i<N(s)) {
    string l= line_read (s, i);
    for (j=0; j<N(l); j++)
      if (l[j] == '=') {
	string left = l (0, j);
	while ((j<N(l)) && ((l[j]=='=') || (l[j]==' '))) j++;
	string right= l (j, N(l));
	set_setting (left, right);
      }
  }
}

/******************************************************************************
* Check for old Init.scm and Init-buffer.scm files
******************************************************************************/

static void
init_upgrade_scheme () {
#ifndef OS_WIN32
  url u= "$TEXMACS_HOME_PATH/progs";
  string prgs= as_string (u);
  if (exists (u * "Init.scm") && (!exists (u * "my-init-texmacs.scm"))) {
    system ("sed 's/Init.scm/init-texmacs.scm/'", u * "Init.scm",
	    ">", u * "my-init-texmacs.scm");
    remove (u * "Init.scm");
  }
  if (exists (u * "Init-buffer.scm") && (!exists (u * "my-init-buffer.scm"))) {
    system ("sed 's/Init-buffer.scm/init-buffer.scm/'", u * "Init-buffer.scm",
	    ">", u * "my-init-buffer.scm");
    remove (u * "Init-buffer.scm");
  }
#endif
}

/******************************************************************************
* Generate documentation about changes
******************************************************************************/

static void
init_upgrade_doc (string install_version) {
  url from_dir= "$TEXMACS_PATH/doc/about/changes";
  url to= "$TEXMACS_HOME_PATH/doc/about/changes/changes-recent.en.tm";
  copy (from_dir * "changes-pre.en.ptm", to);
  if (version_inf (install_version, "0.3.3.24"))
    append (from_dir * "changes-1.en.ptm", to);
  if (version_inf_eq (install_version, "0.3.4.9"))
    append (from_dir * "changes-2.en.ptm", to);
  if (version_inf_eq (install_version, "1.0.0.4"))
    append (from_dir * "changes-3.en.ptm", to);
  if (version_inf_eq (install_version, "1.0.0.6"))
    append (from_dir * "changes-4.en.ptm", to);
  if (version_inf_eq (install_version, "1.0.0.11"))
    append (from_dir * "changes-5.en.ptm", to);
  if (version_inf_eq (install_version, "1.0.2.0"))
    append (from_dir * "changes-6.en.ptm", to);
  if (version_inf_eq (install_version, "1.0.2.6")) {
    append (from_dir * "changes-7.en.ptm", to);
    append (from_dir * "changes-post.en.ptm", to);
  }
  else remove (to);
}

/******************************************************************************
* Upgrading TeXmacs
******************************************************************************/

void
init_upgrade () {
  string install_version= get_setting ("VERSION");

  cerr << HRULE;
  cerr << "Your disk contains a configuration file for TeXmacs-";
  cerr << install_version << ".\n";
  cerr << "I will now perform the upgrade to version " TEXMACS_VERSION "\n";
  cerr << HRULE;

  url old_settings= "$TEXMACS_HOME_PATH/system" * url_wildcard ("TEX_PATHS*");
  url new_settings= "$TEXMACS_HOME_PATH/system/settings.scm";
  remove (old_settings);
  remove (new_settings);

  init_first ();
  init_upgrade_scheme ();
  init_upgrade_doc (install_version);

  remove ("$TEXMACS_HOME_PATH/system/setup.scm");
  remove ("$TEXMACS_HOME_PATH/system/cache" * url_wildcard ("*"));
}
