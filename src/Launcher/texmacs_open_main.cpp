/******************************************************************************
* MODULE     : texmacs_open_main.cpp
* DESCRIPTION: texmacs-open launcher entrypoint
* COPYRIGHT  : (C) 2026
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Windows64/windows64_encoding.hpp"
#include "file.hpp"
#include "tm_ostream.hpp"

#if defined(OS_MINGW64)
#include "Windows64/windows64_system.hpp"
#elif defined(OS_MINGW)
#include "Windows/windows32_system.hpp"
#else
#include "Unix/unix_system.hpp"
#endif

#include <cerrno>
#include <cstring>

#ifdef _WIN32
#include <wchar.h>
#endif

#ifdef _WIN32
#include <windows.h>
#include <shellapi.h>
#else
#include <unistd.h>
#endif

int usage_error(string program_name, string message) {
  cout << "[texmacs-open] " << message << LF;
  cout << "Usage: " << program_name << " [path]" << LF;
  return 1;
}

url find_texmacs_binary(url exe_dir) {
  const char* candidates[] = {"texmacs.bin", "texmacs.exe", "texmacs"};

  for (int i = 0; i < 3; ++i) {
    url candidate = exe_dir * candidates[i];
    if (exists(candidate) && !is_directory(candidate)) return candidate;
  }
  return url_none();
}

int launch_texmacs(url exe_path, bool has_open_path, url open_path) {
#ifdef _WIN32
  std::wstring exe_path_wide = texmacs_utf8_to_wide(concretize(exe_path));
  std::wstring open_path_wide = texmacs_utf8_to_wide(concretize(open_path));

  const wchar_t* args[] = {
    exe_path_wide.c_str(),
    open_path_wide.c_str(),
    NULL
  };

  intptr_t result = _wspawnv(_P_NOWAIT, exe_path_wide.c_str(), args);
  return 0;
#else
  string sys_exe = concretize(exe_path);
  c_string sys_exe_c = sys_exe;
  
  if (has_open_path) {
    string sys_open = concretize(open_path);
    c_string sys_open_c = sys_open;
    execl(sys_exe_c, sys_exe_c, (const char*)sys_open_c, NULL);
  } else {
    execl(sys_exe_c, sys_exe_c, NULL);
  }

  cout << "[texmacs-open] failed to launch process: " << strerror(errno) << LF;
  return 1;
#endif
}

int texmacs_open_entrypoint(int argc, char** argv) {
  string program_name = (argc > 0 && argv[0] != nullptr) ? string(argv[0]) : string("texmacs-open.bin");

  if (argc > 2) {
    return usage_error(program_name, "expected 0 or 1 argument");
  }

  bool has_open_path = (argc == 2);
  url open_path;
  if (has_open_path) {
    open_path = url_system(string(argv[1]));
    if (!exists(open_path)) return usage_error(program_name, "invalid path");
  }

  url exe_dir = texmacs_get_application_directory();

  url target = find_texmacs_binary(exe_dir);
  if (is_none(target)) {
    return usage_error(program_name,
      "could not find texmacs.bin, texmacs.exe, or texmacs in " *
      as_system_string(exe_dir));
  }

  return launch_texmacs(target, has_open_path, open_path);
}

#ifdef _WIN32
int WINAPI wWinMain(HINSTANCE, HINSTANCE, LPWSTR, int) {
  int argc = 0;
  LPWSTR* argv_wide = CommandLineToArgvW(GetCommandLineW(), &argc);
  if (argv_wide == nullptr || argc <= 0) {
    return usage_error("texmacs-open.bin", "failed to read command line arguments");
  }

  string* argv_utf8 = new string[argc];
  char** argv_char = new char*[argc];
  for (int i = 0; i < argc; ++i) {
    argv_utf8[i] = texmacs_wide_to_utf8(argv_wide[i]);
    argv_char[i] = as_charp(argv_utf8[i]);
  }

  int result = texmacs_open_entrypoint(argc, argv_char);

  delete[] argv_char;
  delete[] argv_utf8;
  LocalFree(argv_wide);
  return result;
}

int WINAPI WinMain(HINSTANCE inst, HINSTANCE prev, LPSTR cmdline, int ncmd) {
  return wWinMain(inst, prev, GetCommandLineW(), ncmd);
}

int main(int argc, char** argv) {
  return texmacs_open_entrypoint(argc, argv);
}
#else
int main(int argc, char** argv) {
  return texmacs_open_entrypoint(argc, argv);
}
#endif