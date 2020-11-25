
/******************************************************************************
* MODULE     : openssl.cpp
* DESCRIPTION: Functions for cryptography
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "openssl.hpp"
#include "file.hpp"

/******************************************************************************
* OpenSSL configuration
******************************************************************************/

static string openssl_cmd;

string
openssl (string args) {
  if (openssl_cmd == "") openssl_cmd= get_env ("TM_OPENSSL");
  if (openssl_cmd == "") openssl_cmd= "openssl";
  //cout << "TeXmacs] " << (openssl_cmd * " " * args) << LF;
  return eval_system (openssl_cmd * " " * args);
}

string
openssl_rsa (string args) {
  //return openssl ("rsautl " * args);
  return openssl ("pkeyutl " * args);
}

string
openssl_enc (string args) {
  //return openssl ("aes-256-cbc " * args);
  return openssl ("enc -aes-256-cbc -md sha512 -pbkdf2 -iter 100000 " * args);
}

/******************************************************************************
* RSA encryption and decryption
******************************************************************************/

void
rsa_initialize () {
  url dir = url ("$TEXMACS_HOME_PATH") * "system/crypto";
  url priv= dir * "texmacs.private";
  url pub = dir * "texmacs.public";
  if (!exists (dir)) mkdir (dir);
  if (!exists (priv))
    openssl ("genrsa -out " * as_string (priv) * " 2048 2> /dev/null");
  if (!exists (pub))
    openssl ("rsa -in " * as_string (priv) *
             " -pubout -out " * as_string (pub) * " 2> /dev/null");
}

string
rsa_my_private_key () {
  rsa_initialize ();
  url dir = url ("$TEXMACS_HOME_PATH") * "system/crypto";
  url priv= dir * "texmacs.private";
  string private_key;
  load_string (priv, private_key, true);
  return private_key;
}

string
rsa_my_public_key () {
  rsa_initialize ();
  url dir = url ("$TEXMACS_HOME_PATH") * "system/crypto";
  url priv= dir * "texmacs.public";
  string public_key;
  load_string (priv, public_key, true);
  return public_key;
}

string
rsa_encode (string msg, string key) {
  url _msg= url_temp ();
  save_string (_msg, msg);
  url _key= url_temp ();
  save_string (_key, key);
  string r= openssl_rsa ("-in " * as_string (_msg) *
			 " -pubin -inkey " * as_string (_key) *
			 " -encrypt");
  remove (_msg);
  remove (_key);
  return r;
}

string
rsa_decode (string msg, string key) {
  url _msg= url_temp ();
  save_string (_msg, msg);
  url _key= url_temp ();
  save_string (_key, key);
  string r= openssl_rsa ("-in " * as_string (_msg) *
                         " -inkey " * as_string (_key) *
                         " -decrypt");
  remove (_msg);
  remove (_key);
  return r;
}

/******************************************************************************
* AES encryption and decryption
******************************************************************************/

string
secret_generate (int len) {
  //return openssl ("rand -base64 " * as_string (len));
  return openssl ("rand " * as_string (len));
}

string
secret_encode (string msg, string key) {
  url _msg= url_temp ();
  save_string (_msg, msg);
  url _key= url_temp ();
  save_string (_key, key);
  string r= openssl_enc ("-nosalt -in " * as_string (_msg) *
                         " -pass file:" * as_string (_key));
  remove (_msg);
  remove (_key);
  return r;
}

string
secret_decode (string msg, string key) {
  url _msg= url_temp ();
  save_string (_msg, msg);
  url _key= url_temp ();
  save_string (_key, key);
  string r= openssl_enc ("-nosalt -d -in " * as_string (_msg) *
                         " -pass file:" * as_string (_key));
  remove (_msg);
  remove (_key);
  return r;
}

string
secret_hash (string msg) {
  return secret_encode ("TeXmacs worgelt BlauwBilGorgels", msg);
}
