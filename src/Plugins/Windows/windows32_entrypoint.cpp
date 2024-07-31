/******************************************************************************
* MODULE     : windows_main.cpp
* DESCRIPTION: Windows entry point for TeXmacs
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "windows32_entrypoint.hpp"
#include "win-utf8-compat.hpp"

#include <windows.h>

void texmacs_attach_console() {  
  // if MSYSTEM is set, we are running in a msys environment
  char *msystem = getenv("MSYSTEM");

  // with native msys terminal, we don't need to attach to the console
  if (msystem != nullptr && !IsDebuggerPresent()) {
    return;
  }

  // attach to an existing windows command prompt (if any)
  if (AttachConsole(ATTACH_PARENT_PROCESS)) {
    // redirect the standard input, output and error to the console
    freopen("CONIN$", "r", stdin);
    freopen("CONOUT$", "w", stdout);
    freopen("CONOUT$", "w", stderr);
  }
}

int main(int argc, char** argv) {
  texmacs_attach_console();
  nowide::args a(argc,argv);
  return texmacs_entrypoint(argc, argv);
}
