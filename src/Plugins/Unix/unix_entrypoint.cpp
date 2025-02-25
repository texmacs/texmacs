
/******************************************************************************
* MODULE     : unix_entrypoint.cpp
* DESCRIPTION: Unix entry point for Unix
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "unix_entrypoint.hpp"
#include "unix_system.hpp"
#include "boot.hpp"
#include "sys_utils.hpp"

#include <QCoreApplication>
#include <QDebug>
#include <QDir>

void setup_texmacs_path () {
  string environment_texmacs_path;
  if (texmacs_getenv ("TEXMACS_PATH", environment_texmacs_path)) return;
  url exedir = texmacs_get_application_directory ();
    if (test_texmacs_path (exedir * "TeXmacs")) {
    return;
  }
  if (test_texmacs_path (exedir * "usr/share/TeXmacs")) return;
  if (test_texmacs_path (exedir * "usr/local/share/TeXmacs")) return;
  if (test_texmacs_path (exedir * "../usr/share/TeXmacs")) return;
  if (test_texmacs_path ("/usr/share/TeXmacs")) return;
  if (test_texmacs_path ("/usr/local/share/TeXmacs")) return;
}

int main (int argc, char** argv) {
  texmacs_init_guile_hooks ();
  setup_texmacs_path ();
  if (get_env ("APPIMAGE") != "") {
    url usr_bin = url (get_env ("APPDIR")) * "usr/bin";
    url usr_local_bin = url (get_env ("APPDIR")) * "usr/local/bin";
    set_env ("PATH", get_env ("PATH") * ":" * as_string (usr_bin) * ":" * as_string (usr_local_bin));
  }
#if !defined (OS_MACOS) && QT_VERSION < 0x060000
  if (get_env ("WAYLAND_DISPLAY") == "") {
    set_env ("QT_QPA_PLATFORM", "xcb"); // todo : remove ?
    set_env ("XDG_SESSION_TYPE", "x11");
  }
#endif
  return texmacs_entrypoint (argc, argv);
}
