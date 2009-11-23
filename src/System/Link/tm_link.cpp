
/******************************************************************************
* MODULE     : tm_link.cpp
* DESCRIPTION: Links between TeXmacs and extern programs
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_link.hpp"
#include "../Plugins/Openssl/openssl.hpp"
#include "timer.hpp"

/******************************************************************************
* Sending data by packets
******************************************************************************/

static bool
message_complete (string s) {
  int start= 0;
  int i, n= N(s);
  if (n>0 && s[0] == '!') start= 1;
  for (i=start; i<n; i++)
    if (s[i] == '\n') break;
  if (i == n) return false;
  return (n - (i+1)) >= as_int (s (start, i));
}

static string
message_receive (string& s) {
  int start= 0;
  int i, n= N(s);
  if (n>0 && s[0] == '!') start= 1;
  for (i=start; i<n; i++)
    if (s[i] == '\n') break;
  if (i == n) return "";
  int l= as_int (s (start, i++));
  string r= s (i, i+l);
  s= s (i+l, n);
  return r;
}

void
tm_link_rep::write_packet (string s, int channel) {
  if (secret != "") s= secret_encode (s, secret);
  write ((as_string (N (s)) * "\n") * s, channel);
}

bool
tm_link_rep::complete_packet (int channel) {
  return message_complete (watch (channel));
}

string
tm_link_rep::read_packet (int channel, int timeout, bool& success) {
  success= false;
  string& r= watch (channel);
  time_t start= texmacs_time ();
  while (!message_complete (r)) {
    int n= N(r);
    if (timeout > 0) listen (timeout);
    if (N(r) == n && (texmacs_time () - start >= timeout)) return "";
  }
  if (channel == LINK_OUT && N(r) > 0 && r[0] == '!') {
    secure_server (message_receive (r));
    return "";
  }
  else {
    string back= message_receive (r);
    if (secret != "") back= secret_decode (back, secret);
    success= true;
    return back;
  }
}

/******************************************************************************
* Data encryption
******************************************************************************/

void
tm_link_rep::secure_server (string client_public) {
  if (secret != "") return;
  string k= secret_generate ();
  string s= rsa_encode (k, client_public);
  write_packet (s, LINK_IN);
  secret= k;
}

void
tm_link_rep::secure_client () {
  if (secret != "") return;
  write ("!", LINK_IN);
  write_packet (rsa_my_public_key (), LINK_IN);
  bool success;
  string r= read_packet (LINK_OUT, 10000, success);
  if (!success) { stop (); return; }
  secret= rsa_decode (r, rsa_my_private_key ());
}
