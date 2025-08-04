
/******************************************************************************
* MODULE     : server_log.cpp
* DESCRIPTION: server log facilities
* COPYRIGHT  : (C) 2022  Gregoire lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "server_log.hpp"
#include "language.hpp"
#include "locale.hpp"
#include "client_server.hpp"
#include <unistd.h>

namespace wlog {
#include <Lmcons.h>
#include "windows.h"
}

static wlog::HANDLE tmlog= NULL;

void
server_log_start () {
  if (tmlog == NULL)
    tmlog= wlog::RegisterEventSourceA (NULL, "TeXmacs server");
  if (tmlog == NULL)
    io_warning << "cannot register to server log" << LF;
}

void
server_log_stop () {
  if (!wlog::DeregisterEventSource (tmlog))
    io_warning << "cannot deregister from server log" << LF;
  else
    tmlog= NULL;
}

void
server_log_write (int level, string m) {
  static const string pid= as_string ((int) getpid ());
  static char buf[(unsigned) (UNLEN + (wlog::DWORD) 1)];
  wlog::DWORD buf_size= UNLEN + (wlog::DWORD) 1;
  string uid;
  if (wlog::GetUserNameA (buf, &buf_size))
	uid= string (buf);
  else
	uid= "";
  static const string prefix= "pid=" * pid * ", uid=" * uid * ", ";
  string msg= prefix * m;
  if (!is_server ())
    return;
  string date= get_date (get_locale_language (), "yyyy-MM-dd, HH:mm:ss");
  static bool warned= false;
  if (tmlog == NULL) {
    if (!warned) {
      io_warning << "server log is not active" << LF;
      warned= true;
    }
    io_warning << "server, " << date << ", " << msg << "\n";
  }
  else {
    const c_string _msg (msg);
    const char* _vmsg= _msg;
    wlog::WORD type;
    if (level <= 3) type= EVENTLOG_ERROR_TYPE;
    else if (level == 4) type= EVENTLOG_WARNING_TYPE;
    else if (level <= 6) type= EVENTLOG_INFORMATION_TYPE;
    else type= EVENTLOG_SUCCESS;
    if (!wlog::ReportEventA (tmlog, type, 0, 0,
			     NULL, 1, 0, &_vmsg, NULL))
      io_warning << "server log failed for message, "
                 << date << ", " << msg << "\n";
  }
  debug_io << "server, " << date << ", " << msg << "\n";
}
