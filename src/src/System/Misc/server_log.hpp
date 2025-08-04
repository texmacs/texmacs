
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

const int log_emergency= 0;
const int log_alert= 1;
const int log_critical= 2;
const int log_error= 3;
const int log_warning= 4;
const int log_notice= 5;
const int log_info= 6;
const int log_debug= 7;


// temporary definition
bool is_server_started();

bool is_server() { return is_server_started(); }

void server_log_start ();
void server_log_stop ();
void server_log_write (int level, string msg);

#endif // defined SERVER_LOG_H
