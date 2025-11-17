
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
#include "hashmap.hpp"
#include "tm_contact.hpp"

#include <gnutls/gnutls.h>

#define GNUTLS_ERROR_FMT(ret, opname) \
  ("GnuTLS ERROR (" * as_string (ret) * ") during " *  opname * ": " \
  * gnutls_strerror (ret))

#define GNUTLS_ERROR(ret, op) GNUTLS_ERROR_FMT (ret, op)
#define GNUTLS_ERROR_CALL(ret, op) GNUTLS_ERROR_FMT (ret, #op)

#define GNUTLS_LOG  SLOG
#define GNUTLS_LOGI SLOGI
#define GNUTLS_LOGW SLOGW
#define GNUTLS_LOGE SLOGE

#define GNUTLS_ERR_LOG(ret, op)  SLOG  (GNUTLS_ERROR (ret, op))
#define GNUTLS_ERR_LOGI(ret, op) SLOGI (GNUTLS_ERROR (ret, op))
#define GNUTLS_ERR_LOGW(ret, op) SLOGW (GNUTLS_ERROR (ret, op))
#define GNUTLS_ERR_LOGE(ret, op) SLOGE (GNUTLS_ERROR (ret, op))

#define CHECK_GNUTLS_INIT(ret, call) \
  do { \
    ret = (call); \
    if (ret <= 0 && ret != GNUTLS_E_SUCCESS) { \
      GNUTLS_LOGE (GNUTLS_ERROR_CALL (ret, call)); \
      if (DEBUG_IO) { \
        debug_io << "GnuTLS initialization failed" << LF; \
      } \
      return; \
    } \
  } while (0)

#define CHECK_GNUTLS_BRETURN(ret, call) \
  do { \
    ret = (call); \
    if (ret <= 0 && ret != GNUTLS_E_SUCCESS) { \
      GNUTLS_LOGE (GNUTLS_ERROR_CALL (ret, call)); \
      return false; \
    } \
  } while (0)

// see Password Storage/PBKDF2 in https://cheatsheetseries.owasp.org
#define MAC_SHA256_ITER_COUNT 600000
#define MAC_SHA256_BYTE_SIZE 32

#define PRIVKEY_ALGO GNUTLS_PK_EDDSA_ED25519
#define PRIVKEY_SEC_PARAM GNUTLS_SEC_PARAM_HIGH
#define PRIVKEY_OUTPUT_BUFSIZE 65536

#define CRT_SERIAL_SIZE 20

#define GNUTLS_X509_COMMON_NAME_SIZE 256
#define GNUTLS_X509_COUNTRY_NAME_SIZE 3
#define GNUTLS_X509_ORGANIZATION_NAME_SIZE 256
#define GNUTLS_X509_ORGANIZATIONAL_UNIT_NAME_SIZE 256
#define GNUTLS_X509_LOCALITY_NAME_SIZE 256
#define GNUTLS_X509_STATE_OR_PROVINCE_NAME_SIZE 256

/******************************************************************************
* Test if GnuTLS is present
******************************************************************************/

bool gnutls_present ();

/******************************************************************************
 * X.509 authentication
 ******************************************************************************/

typedef hashmap<string, string> crt_cfg;

string certificate_path ();
bool   certificate_present ();

bool generate_self_signed (tree cfg, url cert_path, url key_path);
bool trust_certificate (const string& crt_pem);

void disable_certificate_time_checks ();

/******************************************************************************
* TLS contacts
******************************************************************************/

tm_contact make_tls_server_contact (array<array<string> > args);
tm_contact make_tls_client_contact (string host, array<array<string> > args);

/******************************************************************************
* PBKDF2 password hash
******************************************************************************/

string hash_password_pbkdf2 (string passwd, string salt);

#endif // GNUTLS_HPP
