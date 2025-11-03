
/******************************************************************************
* MODULE     : server_log.hpp
* DESCRIPTION: TeXmacs logs for the server
* COPYRIGHT  : (C) 2022  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SERVER_LOG_H
#define SERVER_LOG_H
#include "string.hpp"

#define SERRNO_FMT(msg) (string ("error: ") * msg * ": " * strerror (errno))

#define SLOG_LVL(lvl, msg) server_log_write (log_ ## lvl, msg)

#define SLOG(msg)  do { if (DEBUG_IO) SLOG_LVL(debug, msg); } while (0)
#define SLOGI(msg) SLOG_LVL(info, msg)
#define SLOGW(msg) SLOG_LVL(warning, msg)
#define SLOGE(msg) SLOG_LVL(error, msg)

#define SERRNO_LOG(msg)  SLOG  (SERRNO_FMT (msg))
#define SERRNO_LOGI(msg) SLOGI (SERRNO_FMT (msg))
#define SERRNO_LOGW(msg) SLOGW (SERRNO_FMT (msg))
#define SERRNO_LOGE(msg) SLOGE (SERRNO_FMT (msg))

const int log_emergency= 0;
const int log_alert= 1;
const int log_critical= 2;
const int log_error= 3;
const int log_warning= 4;
const int log_notice= 5;
const int log_info= 6;
const int log_debug= 7;

inline tm_ostream& server_get_stream (int level) {
  switch (level) {
    case log_emergency:
      return io_emergency;
    case log_alert:
      return io_alert;
    case log_critical:
      return io_critical;
    case log_error:
      return io_error;
    case log_warning:
      return io_warning;
    case log_notice:
      return io_notice;
    case log_info:
      return io_info;
    case log_debug:
      return debug_io;
  }
}
void server_log_start ();
void server_log_stop ();
void server_log_write (int level, string msg);

#endif // defined SERVER_LOG_H
