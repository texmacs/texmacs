/******************************************************************************
* MODULE     : spawn.cpp
* DESCRIPTION: external command handling
* COPYRIGHT  : (C) 2015 Denis RAUX
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#include <errno.h>
#include <windows.h>
#undef FAILED
#include <vector>
#include <string>
#include "array.hpp"
#include "windows64_spawn.hpp"
#include "windows64_system.hpp"

Channel::Channel (int s): sz (s) {
  origin = -1; saved = -1; fd = -1; toBeClosed = -1; str = NULL; tid = 0; 
}

void
Channel::Init (Direction d) {
  Init (-1, d);
}

void
Channel::Init (int _fd, Direction d) {
  int pp[2];
  origin = _fd;

  if (_pipe (pp, sz, _O_BINARY) == 0) {
    fd = pp[d];
    toBeClosed = pp[otherDirection (d)];

    // Parent pipe handle must NOT be inherited
    HANDLE hParent = (HANDLE) _get_osfhandle (fd);
    if (hParent != INVALID_HANDLE_VALUE) {
      SetHandleInformation (hParent, HANDLE_FLAG_INHERIT, 0);
    }
    // Child pipe handle MUST be inherited
    HANDLE hChild = (HANDLE) _get_osfhandle (toBeClosed);
    if (hChild != INVALID_HANDLE_VALUE) {
      SetHandleInformation (hChild, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    }
  } else {
    origin = -1;
  }
}

void
Channel::redirect () {
  if (origin < 0 || toBeClosed < 0) return;
  saved = _dup (origin); 
  _dup2 (toBeClosed, origin);
  HANDLE h = (HANDLE) _get_osfhandle (origin);
  if (h != INVALID_HANDLE_VALUE) {
    SetHandleInformation (h, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
  }
  if (origin == 0) SetStdHandle (STD_INPUT_HANDLE, h);
  else if (origin == 1) SetStdHandle (STD_OUTPUT_HANDLE, h);
  else if (origin == 2) SetStdHandle (STD_ERROR_HANDLE, h);
}

void
Channel::read (std::string *_str) {
  str = _str;
  tid = _beginthreadex (NULL, 0, bkgread, this, 0, NULL);
}

void 
Channel::close () { 
  if (fd >= 0) { _close (fd); fd = -1; }
}

void 
Channel::closeUnused () {
  if (toBeClosed >= 0) { _close (toBeClosed); toBeClosed = -1; } 
  restore (); 
} 

void
Channel::wait () {
  if (tid) {
    WaitForSingleObject ((HANDLE) tid, 5000);
    CloseHandle ((HANDLE) tid);
    tid = 0;
  }
}
  
void
Channel::restore () {
  if (saved >= 0) {
    _dup2 (saved, origin);
    HANDLE h = (HANDLE) _get_osfhandle (origin);
    if (origin == 0) SetStdHandle (STD_INPUT_HANDLE, h);
    else if (origin == 1) SetStdHandle (STD_OUTPUT_HANDLE, h);
    else if (origin == 2) SetStdHandle (STD_ERROR_HANDLE, h);
    _close (saved);
    saved = -1;
  }
}

Channel::~Channel () { 
  closeUnused (); 
  close (); 
  if (tid) {
    TerminateThread ((HANDLE) tid, -99); 
    CloseHandle ((HANDLE) tid); 
    tid = 0;
  }
}

unsigned
bkgread (void *thatv) {
  char buf[4096];
  int cnt;
  Channel *that = (Channel *) thatv;
  do {
    cnt = _read (that->fd, buf, sizeof (buf));
    if (cnt > 0) {
      *(that->str) += std::basic_string<char> (buf, cnt);
    } else if (cnt == 0) {
      that->close ();
    }
  } while (cnt > 0);

  return (cnt);
}

static std::wstring quote_win32_arg (const std::wstring& arg) {
  if (arg.empty ()) return L"\"\"";
  if (arg.find_first_of (L" \t\n\v\"") == std::wstring::npos) {
    return arg;
  }
  std::wstring quoted = L"\"";
  for (size_t i = 0; i < arg.size (); ++i) {
    size_t num_backslashes = 0;
    while (i < arg.size () && arg[i] == L'\\') {
      ++num_backslashes;
      ++i;
    }
    if (i == arg.size ()) {
      quoted.append (num_backslashes * 2, L'\\');
      break;
    } else if (arg[i] == L'"') {
      quoted.append (num_backslashes * 2 + 1, L'\\');
      quoted.push_back (L'"');
    } else {
      quoted.append (num_backslashes, L'\\');
      quoted.push_back (arg[i]);
    }
  }
  quoted += L"\"";
  return quoted;
}

static bool is_shell_builtin (string cmd) {
  string s;
  for (int i = 0; i < N (cmd); ++i) {
    char c = cmd[i];
    if (c >= 'A' && c <= 'Z') c += ('a' - 'A');
    s << c;
  }
  return s == "dir" || s == "cd" || s == "echo" || s == "copy" ||
         s == "del" || s == "erase" || s == "ren" || s == "rename" ||
         s == "type" || s == "md" || s == "mkdir" || s == "rd" ||
         s == "rmdir" || s == "cls" || s == "ver" || s == "vol";
}

spawn_system::spawn_system (::array<Channel> &ch, string name, ::array< ::string> args): process_id (0), process_handle (NULL), channel (ch) {
  STARTUPINFOW si;
  ZeroMemory (&si, sizeof (si));
  si.cb = sizeof (si);
  si.dwFlags = STARTF_USESTDHANDLES;

  HANDLE hIn = INVALID_HANDLE_VALUE;
  HANDLE hOut = INVALID_HANDLE_VALUE;
  HANDLE hErr = INVALID_HANDLE_VALUE;

  for (int i = 0; i < N (channel); ++i) {
    int orig = channel[i].getOrigin ();
    int p = channel[i].getPipe ();
    if (p >= 0) {
      HANDLE h = (HANDLE) _get_osfhandle (p);
      if (orig == 0) hIn = h;
      else if (orig == 1) hOut = h;
      else if (orig == 2) hErr = h;
    }
  }

  // Stdin: if not redirected, open NUL so child process never blocks on stdin
  HANDLE hNulIn = INVALID_HANDLE_VALUE;
  if (hIn == INVALID_HANDLE_VALUE) {
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof (sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;
    hNulIn = CreateFileW (L"NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
                          &sa, OPEN_EXISTING, 0, NULL);
    si.hStdInput = (hNulIn != INVALID_HANDLE_VALUE) ? hNulIn : GetStdHandle (STD_INPUT_HANDLE);
  } else {
    si.hStdInput = hIn;
  }

  // Stdout: if not redirected, open NUL or fallback to GetStdHandle
  HANDLE hNulOut = INVALID_HANDLE_VALUE;
  if (hOut == INVALID_HANDLE_VALUE) {
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof (sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;
    hNulOut = CreateFileW (L"NUL", GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
                           &sa, OPEN_EXISTING, 0, NULL);
    si.hStdOutput = (hNulOut != INVALID_HANDLE_VALUE) ? hNulOut : GetStdHandle (STD_OUTPUT_HANDLE);
  } else {
    si.hStdOutput = hOut;
  }

  // Stderr: if not redirected, open NUL or fallback to GetStdHandle
  HANDLE hNulErr = INVALID_HANDLE_VALUE;
  if (hErr == INVALID_HANDLE_VALUE) {
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof (sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;
    hNulErr = CreateFileW (L"NUL", GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
                           &sa, OPEN_EXISTING, 0, NULL);
    si.hStdError = (hNulErr != INVALID_HANDLE_VALUE) ? hNulErr : GetStdHandle (STD_ERROR_HANDLE);
  } else {
    si.hStdError = hErr;
  }

  // Build command line
  std::wstring cmdline;
  bool need_cmd = (N (args) > 0 && is_shell_builtin (args[0]));

  if (need_cmd) {
    cmdline = L"cmd.exe /c";
    for (int i = 0; i < N (args); ++i) {
      cmdline += L" ";
      cmdline += quote_win32_arg (texmacs_utf8_to_wide (args[i]));
    }
  } else {
    for (int i = 0; i < N (args); ++i) {
      if (i > 0) cmdline += L" ";
      cmdline += quote_win32_arg (texmacs_utf8_to_wide (args[i]));
    }
  }

  std::vector<wchar_t> cmd_buf (cmdline.begin (), cmdline.end ());
  cmd_buf.push_back (0);

  PROCESS_INFORMATION pi;
  ZeroMemory (&pi, sizeof (pi));

  BOOL ok = CreateProcessW (NULL, cmd_buf.data (), NULL, NULL, TRUE,
                            0, NULL, NULL, &si, &pi);

  if (!ok && !need_cmd && (GetLastError () == ERROR_FILE_NOT_FOUND || GetLastError () == ERROR_PATH_NOT_FOUND)) {
    std::wstring fb_cmd = L"cmd.exe /c " + cmdline;
    std::vector<wchar_t> fb_buf (fb_cmd.begin (), fb_cmd.end ());
    fb_buf.push_back (0);
    ok = CreateProcessW (NULL, fb_buf.data (), NULL, NULL, TRUE,
                         0, NULL, NULL, &si, &pi);
  }

  if (ok) {
    CloseHandle (pi.hThread);
    process_id = pi.dwProcessId;
    process_handle = pi.hProcess;
  } else {
    process_id = 0;
    process_handle = NULL;
  }

  // Close child pipe handles in parent so reader thread receives EOF when child finishes
  for (int i = 0; i < N (channel); ++i) {
    channel[i].closeUnused ();
  }

  if (hNulIn != INVALID_HANDLE_VALUE) CloseHandle (hNulIn);
  if (hNulOut != INVALID_HANDLE_VALUE) CloseHandle (hNulOut);
  if (hNulErr != INVALID_HANDLE_VALUE) CloseHandle (hNulErr);
}

int
spawn_system::wait () {
  int ret = 0;
  if (process_handle != NULL) {
    WaitForSingleObject (process_handle, INFINITE);
    DWORD exit_code = 0;
    if (GetExitCodeProcess (process_handle, &exit_code)) {
      ret = (int) exit_code;
    } else {
      ret = (int) GetLastError ();
    }
    CloseHandle (process_handle);
    process_handle = NULL;
  } else {
    ret = EINVAL;
  }
  for (int i = 0; i < N (channel); ++i) {
    channel[i].wait ();
  }
  return (ret);
}

spawn_system::~spawn_system () {
  if (process_handle != NULL) {
    CloseHandle (process_handle);
    process_handle = NULL;
  }
}
