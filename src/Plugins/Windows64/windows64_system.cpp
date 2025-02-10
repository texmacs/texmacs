/******************************************************************************
* MODULE   : windows64_system.cpp
* DESCRIPTION: Windows system functions with UTF-8 input/output instead of ANSI
* COPYRIGHT  : (C) 2024 Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <io.h>
#include <process.h>
#include <string>
#include <vector>
#include <iostream>

#include "config.h"
#include "windows64_system.hpp"
#include "windows64_encoding.hpp"
#include "windows64_spawn.hpp"

#include "Scheme/Guile/guile_tm.hpp"
#ifdef SCM_HAVE_HOOKS
#include "libguile/system.h"
#endif

#ifdef QTTEXMACS
#include <QGuiApplication>
#include <QStyleHints>
#endif

#include "analyze.hpp"
#include "tm_timer.hpp"

typedef struct texmacs_dir_t {
  HANDLE handle;
  WIN32_FIND_DATAW find_data;
  bool is_find_data_valid;
} texmacs_dir_t;

FILE* texmacs_fopen(string filename, string mode, bool lock) {
  std::wstring wide_filename = texmacs_utf8_to_wide(filename);
  std::wstring wide_mode = texmacs_utf8_to_wide(mode);
  wide_mode += L"b";
  FILE* result = _wfopen(wide_filename.c_str(), wide_mode.c_str());
  return result;
}

ssize_t texmacs_fsize(FILE *stream) {
  int fd = _fileno(stream);
  return _filelengthi64(fd);
}

ssize_t texmacs_fread(char *z, size_t n, FILE *stream) {
  return fread(z, 1, n, stream);
}

ssize_t texmacs_fwrite(const char *str, size_t size, FILE *stream) {
  if (stream != stdout && stream != stderr) {
    size_t ret= fwrite(str, size, 1, stream);
    return ret < 1 ? 0 : size;
  }
  std::wstring wide_str = texmacs_utf8_to_wide(string(str, size));
  if (stream == stdout) {
    std::wcout << wide_str;
  } else {
    std::wcerr << wide_str;
  }
  return size;
}

void texmacs_fclose(FILE *&file, bool unlock) {
  fclose(file);
  file = nullptr;
}

TEXMACS_DIR texmacs_opendir(string dirname) {
  dirname = dirname * "\\*";
  texmacs_dir_t *dir = new texmacs_dir_t;
  dir->handle = FindFirstFileW(
    texmacs_utf8_to_wide(dirname).c_str(), &dir->find_data
  );
  if (dir->handle == INVALID_HANDLE_VALUE) {
    delete dir;
    return nullptr;
  }
  dir->is_find_data_valid = true;
  return dir;
}

void texmacs_closedir(TEXMACS_DIR dir) {
  FindClose(dir->handle);
  delete dir;
}

texmacs_dirent texmacs_readdir(TEXMACS_DIR dirp) {
  texmacs_dirent dirent;
  dirent.is_valid = dirp->is_find_data_valid;
  dirent.d_name = texmacs_wide_to_utf8(dirp->find_data.cFileName);
  dirp->is_find_data_valid = FindNextFileW(dirp->handle, &dirp->find_data);
  return dirent;
}

int texmacs_stat(string filename, struct_stat* buf) {
  return _wstat64(texmacs_utf8_to_wide(filename).c_str(), buf);
}

bool texmacs_getenv(string var_name, string &var_value) {
  std::wstring wide_var_name = texmacs_utf8_to_wide(var_name);
  size_t required_size;
  _wgetenv_s(&required_size, nullptr, 0, wide_var_name.c_str());

  if (required_size == 0) {
    return false;
  }

  std::vector<wchar_t> value(required_size);
  _wgetenv_s(&required_size, value.data(), required_size, wide_var_name.c_str());

  var_value = texmacs_wide_to_utf8(value.data());
  return true;
}

bool texmacs_setenv(string var_name, string new_value) {
  std::wstring wide_var_name = texmacs_utf8_to_wide(var_name);
  std::wstring wide_new_value = texmacs_utf8_to_wide(new_value);
  return _wputenv_s(wide_var_name.c_str(), wide_new_value.c_str()) == 0;
}

bool texmacs_putenv(string variable) {
  std::wstring wide_variable = texmacs_utf8_to_wide(variable);
  return _wputenv(wide_variable.c_str()) == 0;
}

bool texmacs_mkdir(string dirname, int mode) {
  return CreateDirectoryW(
    texmacs_utf8_to_wide(dirname).c_str(), 
    nullptr
  ) != 0;
}

bool texmacs_rmdir(string dirname) {
  return RemoveDirectoryW(texmacs_utf8_to_wide(dirname).c_str()) != 0;
}

bool texmacs_rename(string oldname, string newname) {
  texmacs_remove(newname);
  return MoveFileW(
    texmacs_utf8_to_wide(oldname).c_str(), 
    texmacs_utf8_to_wide(newname).c_str()
  ) != 0;
}

bool texmacs_chmod(string filename, int mode) {
  return _wchmod(texmacs_utf8_to_wide(filename).c_str(), mode) == 0;
}

bool texmacs_remove(string filename) {
  return _wremove(texmacs_utf8_to_wide(filename).c_str()) == 0;
}


#ifdef SCM_HAVE_HOOKS
int texmacs_guile_stat(const char *path, guile_stat_t *buf) {
  std::wstring wide_path = texmacs_utf8_to_wide(path);
  int result = _wstat64(wide_path.c_str(), buf);
  return result;
}

int texmacs_guile_lstat(const char *path, guile_stat_t *buf) {
  std::wstring wide_path = texmacs_utf8_to_wide(path);
  return _wstat64(wide_path.c_str(), buf);
}

int texmacs_guile_open(const char *pathname, int flags, mode_t mode) {
  std::wstring wide_path = texmacs_utf8_to_wide(pathname);
  int result = _wopen(wide_path.c_str(), flags, mode);
  return result;
}

DIR *texmacs_guile_opendir(const char *name) {
  return (DIR*)texmacs_opendir(name);
}

guile_dirent_t *texmacs_guile_readdir(DIR *_dirp) {
  texmacs_dir_t *dirp = (texmacs_dir_t*)_dirp;
  if (dirp->is_find_data_valid == false) {
    return nullptr;
  }
  guile_dirent_t *dirent = (guile_dirent_t*)malloc(sizeof(guile_dirent_t));
  string name = texmacs_wide_to_utf8(dirp->find_data.cFileName);
  c_string c_name = name;
  strncpy(dirent->d_name, c_name, 256);
  dirp->is_find_data_valid = FindNextFileW(dirp->handle, &dirp->find_data);
  return dirent;
}

int texmacs_guile_truncate(const char *path, guile_off_t length) {
  std::wstring wide_path = texmacs_utf8_to_wide(path);
  HANDLE file = CreateFileW(
    wide_path.c_str(), GENERIC_WRITE, 0, nullptr,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr
  );
  if (file == INVALID_HANDLE_VALUE) {
    return -1;
  }
  LARGE_INTEGER li;
  li.QuadPart = length;
  if (!SetFilePointerEx(file, li, nullptr, FILE_BEGIN)) {
    CloseHandle(file);
    return -1;
  }
  if (!SetEndOfFile(file)) {
    CloseHandle(file);
    return -1;
  }
  CloseHandle(file);
  return 0;
}

char *texmacs_guile_getenv(const char *name) {
  string utf8_string;
  bool res = texmacs_getenv(name, utf8_string);
  if (!res) {
    return nullptr;
  }
  const size_t current_size = N(utf8_string) + 1;

  static size_t c_utf8_string_size = 1024;
  static char *c_utf8_string = (char*)malloc(c_utf8_string_size);
  
  if (current_size > c_utf8_string_size) {
    free(c_utf8_string);
    c_utf8_string_size = current_size * 2;
    c_utf8_string = (char*)malloc(c_utf8_string_size);
  }

  memcpy(c_utf8_string, &utf8_string[0], N(utf8_string));
  c_utf8_string[N(utf8_string)] = 0;
  return c_utf8_string;
}

int texmacs_guile_printf(const char *format, ...) {
  // first, use vsnprintf to get the size of the buffer
  va_list args;
  va_start(args, format);
  int size = vsnprintf(nullptr, 0, format, args);
  va_end(args);
  
  if (size == 0) {
    return 0;
  }

  // then, allocate the buffer and print the string
  char *buffer = (char*)malloc(size + 1);
  va_start(args, format);
  vsnprintf(buffer, size + 1, format, args);
  va_end(args);

  // print the string
  std::wcout << texmacs_utf8_to_wide(string(buffer, size)) << std::endl;
  
  // free the buffer
  free(buffer);

  return size;
}

int texmacs_guile_fprintf(FILE *stream, const char *format, ...) {
  if (stream == stdout || stream == stderr) {
    va_list args;
    va_start(args, format);
    int res = texmacs_guile_printf(format, args);
    va_end(args);
    return res;
  }
  va_list args;
  va_start(args, format);
  int res = vfprintf(stream, format, args);
  va_end(args);
  return res;
}
#endif

void texmacs_init_guile_hooks() {
#ifdef SCM_HAVE_HOOKS
  guile_stat = texmacs_guile_stat;
  guile_lstat = texmacs_guile_lstat;
  guile_open = texmacs_guile_open;
  guile_opendir = texmacs_guile_opendir;
  guile_readdir = texmacs_guile_readdir;
  guile_truncate = texmacs_guile_truncate;
  guile_getenv = texmacs_guile_getenv;
  guile_fprintf = texmacs_guile_fprintf;
  guile_printf = texmacs_guile_printf;
#else
  cout << "warning: guile hooks are not available" << LF;
#endif
}

intptr_t texmacs_spawnvp(int mode, string name, array<string> args) {
  // convert the arguments to a wide string
  std::vector<wchar_t*> wide_args;
  for (int i = 0; i < N(args); i++) {
    std::wstring wide_arg = texmacs_utf8_to_wide(args[i]);
    wchar_t *c_wide_arg = (wchar_t*)malloc((wide_arg.size() + 1) * sizeof(wchar_t));
    memcpy(c_wide_arg, &wide_arg[0], wide_arg.size() * sizeof(wchar_t));
    c_wide_arg[wide_arg.size()] = 0;
    wide_args.push_back(c_wide_arg);
  }

  // convert the name to a wide string
  std::wstring wide_name = texmacs_utf8_to_wide(name);

  // spawn the process
  intptr_t res = _wspawnvp(mode, wide_name.c_str(), 
                           (wchar_t* const*)wide_args.data());

  // free the memory
  for (size_t i = 0; i < wide_args.size(); i++) {
    free(wide_args[i]);
  }

  return res;
}

bool IsWindowsDarkMode() {
  HKEY hKey;
  DWORD value;
  DWORD valueSize = sizeof(value);
  LONG result;

  result = RegOpenKeyExW(
              HKEY_CURRENT_USER,
              L"Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
              0, KEY_READ, &hKey
  );

  if (result != ERROR_SUCCESS) {
      return false;
  }

  // Query the value of the AppsUseLightTheme key
  result = RegQueryValueExW(hKey, L"AppsUseLightTheme", nullptr,
                            nullptr, (LPBYTE)&value, &valueSize);
  RegCloseKey(hKey);

  if (result != ERROR_SUCCESS) {
      return false;   // Probably windows 7 or below
  }

  return value == 0;  // If value is 0, dark mode is enabled
}


string get_default_theme() {
#if defined(QTTEXMACS) && QT_VERSION >= 0x060500
  if (QGuiApplication::styleHints()->colorScheme() == Qt::ColorScheme::Dark) {
    return "dark";
  } else {
    return "light";
  }
#else
  if (IsWindowsDarkMode()) {
    return "dark";
  }
  return "light";
#endif
}

string texmacs_get_application_directory_str() {
  std::wstring wide_path(MAX_PATH, 0);
  GetModuleFileNameW(nullptr, &wide_path[0], MAX_PATH);
  size_t pos = wide_path.find_last_of(L"\\");
  pos = std::max(pos, wide_path.find_last_of(L"/"));
  if (pos == std::wstring::npos) {
    return "";
  }
  wide_path = wide_path.substr(0, pos);
  return texmacs_wide_to_utf8(wide_path);
}

/* ScopedHandle is a class to automatically close a handle in windows_system function,
 * simplifying the code and avoiding resource leaks.
 */
class ScopedHandle {

public:
  ~ScopedHandle() {
    if (h) {
      CloseHandle(h);
    }
  }

  HANDLE h = 0;
};

int windows_system(string cmd, string *cmdout, string *cmderr) {

  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle = TRUE;
  sa.lpSecurityDescriptor = NULL;

  ScopedHandle hOutRead, hOutWrite;
  ScopedHandle hErrRead, hErrWrite;

  bool res = CreatePipe(&hOutRead.h, &hOutWrite.h, &sa, 0);
  if (!res) {
    std_warning << "failed to create pipe" << LF;
    return 1;
  }

  res = CreatePipe(&hErrRead.h, &hErrWrite.h, &sa, 0);
  if (!res) {
    std_warning << "failed to create pipe"  << LF;
    return 1;
  }

  res = SetHandleInformation(hOutRead.h, HANDLE_FLAG_INHERIT, 0);
  if (!res) {
    std_warning << "failed to set handle information"  << LF;
    return 1;
  }

  res = SetHandleInformation(hErrRead.h, HANDLE_FLAG_INHERIT, 0);
  if (!res) {
    std_warning << "failed to set handle information"  << LF;
    return 1;
  }

  PROCESS_INFORMATION pi;
  STARTUPINFOW si;
  ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
  ZeroMemory(&si, sizeof(STARTUPINFOW));
  si.cb = sizeof(STARTUPINFOW);
  si.hStdOutput = hOutWrite.h;
  si.hStdError = hErrWrite.h;
  si.dwFlags |= STARTF_USESTDHANDLES;

  std::wstring wide_cmd = texmacs_utf8_to_wide(cmd);

  // CreateProcessW will work only on executable files.
  // It will not work to open PDF, links, etc.
  res = CreateProcessW(NULL, (LPWSTR)wide_cmd.c_str(), NULL, NULL, 
                       TRUE, CREATE_NO_WINDOW, NULL, NULL, &si, &pi);
  if (!res) {
    // If we are here, it means that windows_system is trying to
    // open a file, like a PDF, or a link.
    res = (uintptr_t)ShellExecuteW(NULL, NULL, wide_cmd.c_str(), 
                                   NULL, NULL, SW_SHOW) >= 32;

    if (!res) {
      std_warning << "failed to launch command '" << cmd << "'" << LF;
      return 1;
    }

    // in the case of PDF or link, we don't want (and have)
    // to read stdout and stderr, so we immediately.
    return 0;
  }

  if (cmdout == nullptr && cmderr == nullptr) {
    // If we are here, we launched a command, and we don't want to read
    // the output, so we immediately return.
    return 0;
  }

  // We wait for the application to finish before reading the output
  // todo : this should be done asynchronously, 
  // because it can freeze the application
  WaitForSingleObject(pi.hProcess, 30000);
  
  // Close the write pipe handle so the child process stops reading
  // and we can read the output
  CloseHandle(hOutWrite.h);
  CloseHandle(hErrWrite.h);

  DWORD bytesRead;
  std::wstring wide_cmdout, wide_cmderr;
  WCHAR buffer[4096];

  while (ReadFile(hOutRead.h, buffer, sizeof(buffer), &bytesRead, NULL) && bytesRead > 0) {
    wide_cmdout += std::wstring(buffer, bytesRead);
  }

  while (ReadFile(hErrRead.h, buffer, sizeof(buffer), &bytesRead, NULL) && bytesRead > 0) {
    wide_cmderr += std::wstring(buffer, bytesRead);
  }

  if (cmdout != nullptr) {
    *cmdout = texmacs_wide_to_utf8(wide_cmdout);
  }
  if (cmderr != nullptr) {
    *cmderr = texmacs_wide_to_utf8(wide_cmderr);
  }

  DWORD exitCode;
  GetExitCodeProcess(pi.hProcess, &exitCode);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);

  return exitCode;
}

int windows_system(string cmd, string &cmdout, string &cmderr) {
  return windows_system(cmd, &cmdout, &cmderr);
}

int windows_system(string cmd, string &cmdout) {
  return windows_system(cmd, &cmdout, nullptr);
}

int windows_system(string cmd) {
  std::wstring wide_cmd = texmacs_utf8_to_wide(cmd);
  HINSTANCE res = ShellExecuteW(NULL, NULL, wide_cmd.c_str(), NULL, NULL, SW_SHOW );
  return (uintptr_t)res >= 32 ? 0 : -1;
}

static void
mingw_system_warn (pid_t pid, ::string which, ::string msg) {
  debug_io << "unix_system, pid " << pid << ", warning: " << msg << "\n";
}

int
mingw_system (::array< ::string> arg,
	    ::array<int> fd_in, ::array< ::string> str_in,
      ::array<int> fd_out, ::array< ::string*> str_out) {
	// Run command arg[0] with arguments arg[i], i >= 1.
  // str_in[i] is sent to the file descriptor fd_in[i].
  // str_out[i] is filled from the file descriptor fd_out[i].
  // If str_in[i] is -1 then $$i automatically replaced by a valid
  // file descriptor in arg.
  if (N(arg) == 0) return 0;
  ::string which= recompose (arg, " ");
  int n_in= N (fd_in), n_out= N (fd_out);
  ASSERT(N(str_in)  == n_in, "size mismatch");
  ASSERT(N(str_out) == n_out, "size mismatch");
  ::array<Channel> ch (n_in + n_out);

  for (int i= 0; i < n_in; i++) {
    int fd= fd_in[i];
    if (fd >= 0) ch[i].Init (fd,Channel::CHIN); 
    else ch[i].Init(Channel::CHIN);
  }
  for (int i= 0; i < n_out; i++) {
    int fd= fd_out[i];
    if (fd >= 0) ch[i + n_in].Init (fd,Channel::CHOUT); 
    else ch[i + n_in].Init(Channel::CHOUT);
  }

  ::array< ::string> arg_= arg;
  for (int j= 0; j < N(arg); j++)
    for (int i= 0; i < n_in; i++)
      if (fd_in[i] < 0) {
        arg_[j]= replace (arg_[j], "$%" * as_string (i),
          as_string (_get_osfhandle (ch[i].getPipe ())));
        arg_[j]= replace (arg_[j], "$$" * as_string (i),
          as_string (ch[i].getPipe ()));
      }
  debug_io << "unix_system, launching: " << arg_ << "\n"; 

  spawn_system process (ch, arg_[0], arg_);
  if (!process.isRunning ()) {
    debug_io << "unix_system, failed" << "\n";
    return -1;
  }
  debug_io << "unix_system, succeeded to create pid " << \
      process.getpid() <<  LF;

  // receive data from spawn process
  // class string is not thread safe, use std::string instead
  ::array<std::string> str(n_out);
  for (int i= 0; i < n_out; i++) ch[i + n_in].read(&str[i]);

  // send data to spawn process
  ::array<int> pos_in (n_in);
  for (int i= 0; i < n_in; i++) pos_in[i]= 0;
  time_t last_wait_time= texmacs_time ();

  bool busy;
  do {
    busy= false;
    if (texmacs_time () - last_wait_time > 5000) { // FIXME?
      last_wait_time= texmacs_time ();
      mingw_system_warn (process.getpid(), which, "waiting spawn process");
    }
    for (int i= 0; i < n_in; i++) {
      if (N(str_in[i]) > pos_in[i]) {
        int m= min (ch[i].sz, N(str_in[i]) - pos_in[i]); //do not fill the pipe
        int o= ch[i].write (&(str_in[i][pos_in[i]]), m);
        if (o >= 0) { 
          pos_in[i] += o;
          if (N(str_in[i]) == pos_in[i]) ch[i].close (); else busy= true;
        } 
      }
    }
  } while (busy);

  // wait for process
  int wret= process.wait();
  debug_io << "unix_system, pid " << process.getpid ()
           << " terminated with code" << wret << "\n"; 
  for (int i= 0; i < n_out; ++i)
    (*(str_out[i])) << ::string(str[i].data (), str[i].length ()); 
  return (wret);
}
