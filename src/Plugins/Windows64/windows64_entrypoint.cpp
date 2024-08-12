/******************************************************************************
* MODULE   : windows64_entrypoint.cpp
* DESCRIPTION: Windows entry point for TeXmacs
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// SECURITY_WIN32 is needed to include the security headers and to get 
// the user name using GetUserNameExW
#define SECURITY_WIN32

#include "windows64_entrypoint.hpp"
#include "windows64_system.hpp"
#include "windows64_encoding.hpp"

#include <windows.h>
#include <shellapi.h>
#include <sspi.h>
#include <secext.h>
#include <stdio.h>
#include <fcntl.h>
#include <io.h>

#include <QApplication>
#include <QDebug>
#include <iostream>

#include "Scheme/Guile/guile_tm.hpp"

/*
 * Qt can replace the main with qMain by doing a #define main qMain.
 * This qMain is doing his own conversion of the arguments.
 * Make sure that we are not using the main function 
 * of Qt by emitting an error.
 */
#if defined(QT_NEEDS_QMAIN)
#error "Qt is replacing main with qMain. \
        Please, remove the usage of QMain \
        (you shouldn't have a QT_NEEDS_QMAIN). \
        With --with-qt-find-method=pkg-config, \
        it is the default behavior."
#endif

/*
 * @brief The entry point of the program that will make sure that texmacs 
 * run seemlessly on Windows.
 */
int WINAPI CommonMain() {
  texmacs_attach_console();
  texmacs_initialize_displayname();
  texmacs_init_guile_hooks();

  _setmode(_fileno(stdout), _O_U8TEXT);
  _setmode(_fileno(stderr), _O_U8TEXT);

  int argc = 0;
  LPWSTR* pArgvW = CommandLineToArgvW(GetCommandLineW(), &argc);

  string *texmacs_argv = new string[argc];
  char **char_argv = new char*[argc];

  for (int i = 0; i < argc; i++) {
    texmacs_argv[i] = texmacs_wide_to_utf8(pArgvW[i]);
    char_argv[i] = as_charp(texmacs_argv[i]);
  }

  int result = texmacs_entrypoint(argc, char_argv);

  LocalFree(pArgvW);
  delete[] texmacs_argv;
  delete[] char_argv;

  return result;
}

int WINAPI WinMain(HINSTANCE inst, HINSTANCE prev, LPSTR cmdline, int ncmd) {
  return CommonMain();
}

int WINAPI wWinMain(HINSTANCE inst, HINSTANCE prev, LPWSTR cmdline, int ncmd) {
  return CommonMain();
}

int main(int argc, char** argv) {
  return CommonMain();
}

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

void texmacs_initialize_displayname() {
  if (_wgetenv(L"TEXMACS_DISPLAYNAME") != nullptr) {
    return;
  }
  wchar_t username[256 + 1];
  DWORD username_size = 256 + 1;
  bool result = GetUserNameExW(NameDisplay, username, &username_size);
  if (result) {
    std::wstring full_username = L"TEXMACS_DISPLAYNAME=" 
                                 + std::wstring(username);
    _wputenv(full_username.c_str());
  } else {
    wchar_t error_message[256];
    FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM, nullptr, GetLastError(),
                   0, error_message, 256, nullptr);

    cout << "texmacs_initialize_displayname error " 
         << texmacs_wide_to_utf8(error_message) << "\r\n";

    _wputenv(L"TEXMACS_DISPLAYNAME=Default User");
  }
}