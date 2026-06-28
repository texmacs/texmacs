
/******************************************************************************
* MODULE     : sys_utils.hpp
* DESCRIPTION: system utilities
* COPYRIGHT  : (C) 1999-2016  Joris van der Hoeven, Denis Raux
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SYS_UTILS_H
#define SYS_UTILS_H
#include "string.hpp"
#include "url.hpp"
#include "array.hpp"

#if defined (OS_MINGW64)
#ifdef QTTEXMACS
#include "Qt/qt_sys_utils.hpp"
#endif
#include "Windows64/windows64_system.hpp"
#elif defined (OS_MINGW)
#ifdef QTTEXMACS
#include "Qt/qt_sys_utils.hpp"
#endif
#include "Windows/mingw_sys_utils.hpp"
#include "Windows/windows32_system.hpp"
#elif defined (OS_ANDROID)
#include "Android/android_system.hpp"
#include "Qt/qt_sys_utils.hpp"
#else
#include "Unix/unix_sys_utils.hpp"
#include "Unix/unix_system.hpp"
#endif

extern int script_status; // 0: never accept, 1: prompt, 2: always accept

int    system (string s);
int    system (string s, string &r);
int    system (string s, string &r, string& e);
string eval_system (string s);
string var_eval_system (string s);
string get_env (string var);
void   set_env (string var, string with);
int    os_version ();
string get_stacktrace (unsigned int max_frames= 127);

url get_texmacs_path ();
url get_texmacs_home_path ();

array<string> evaluate_system (array<string> arg,
			       array<int> fd_in, array<string> in,
			       array<int> fd_out);

class object;
bool async_eval_system (string cmd, object call_back);
void async_eval_pending ();

string get_printing_default ();
bool has_printing_cmd (void);
string get_printing_cmd (void);
void set_printing_cmd (string cmd);

/******************************************************************************
* Cross-platform poll
* Uses poll() on POSIX, select() on Windows.
* Returns number of ready fds, 0 on timeout, -1 on error.
******************************************************************************/

#define TM_POLL_READ   0x01
#define TM_POLL_WRITE  0x02
#define TM_POLL_ERROR  0x04

struct tm_pollfd {
  int fd;
  int events;   /* requested: TM_POLL_READ, TM_POLL_WRITE */
  int revents;  /* returned:  TM_POLL_READ, TM_POLL_WRITE, TM_POLL_ERROR */
};

int tm_poll (struct tm_pollfd* fds, int nfds, int timeout_ms);

#endif // defined SYS_UTILS_H
