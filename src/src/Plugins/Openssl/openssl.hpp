
/******************************************************************************
* MODULE     : openssl.hpp
* DESCRIPTION: Functions for cryptography
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
