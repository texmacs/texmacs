
/******************************************************************************
* MODULE     : openssl.hpp
* DESCRIPTION: Functions for cryptography
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_OPENSSL_H
#define TM_OPENSSL_H
#include "string.hpp"

string rsa_my_private_key ();
string rsa_my_public_key ();
string rsa_encode (string msg, string key);
string rsa_decode (string msg, string key);

string secret_generate (int len= 32);
string secret_encode (string msg, string key);
string secret_decode (string msg, string key);
string secret_hash (string msg);

#endif // TM_OPENSSL_H
