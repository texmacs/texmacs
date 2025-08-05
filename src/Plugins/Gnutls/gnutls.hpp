
/******************************************************************************
* MODULE     : gnutls.hpp
* DESCRIPTION: interface with GnuTLS
* COPYRIGHT  : (C) 2022  Gregoire Lecerf
*                  2025  Robin Wils
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GNUTLS_HPP
#define GNUTLS_HPP
#include "tm_contact.hpp"

#define GNUTLS_ERROR(ret) \
  "GnuTLS ERROR (" * ret * "): " \
  * __func__ * ":" * __LINE__ * " " \
  * gnutls_strerror (ret)

#define GNUTLS_ERROR_CALL(ret, call) \
  "GnuTLS ERROR (" * as_string (ret) * "): " \
  * __func__ * ":" * __LINE__ * " " \
  * #call * " " * gnutls_strerror (ret)

#define GNUTLS_LOG(msg) \
    if (is_server()) { \
      server_log_write (log_error, msg); \
    } else if (DEBUG_IO) { \
      debug_io << msg << LF; \
    }

#define GNUTLS_LOG_CALL(ret, call) \
  do { \
    string msg = GNUTLS_ERROR_CALL(ret, call); \
    GNUTLS_LOG(msg); \
  } while (0)

#define CHECK_GNUTLS_INIT(ret, call) \
  do { \
    ret = (call); \
    if (ret <= 0 && ret != GNUTLS_E_SUCCESS) { \
      GNUTLS_LOG_CALL(ret, call); \
      if (DEBUG_IO) { \
        debug_io << "GnuTLS initialization failed" << LF; \
      } \
      return; \
    } \
  } while (0)

// see Password Storage/PBKDF2 in https://cheatsheetseries.owasp.org
#define MAC_SHA256_ITER_COUNT 600000
#define MAC_SHA256_BYTE_SIZE 32

/******************************************************************************
* Test if GnuTLS is present
******************************************************************************/

bool gnutls_present ();

/******************************************************************************
* TLS contacts
******************************************************************************/

tm_contact make_tls_server_contact (array<array<string> > args);
tm_contact make_tls_client_contact (array<array<string> > args);

/******************************************************************************
* PBKDF2 password hash
******************************************************************************/

string hash_password_pbkdf2 (string passwd, string salt);

#endif // GNUTLS_HPP
