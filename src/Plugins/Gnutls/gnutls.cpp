
/******************************************************************************
 * MODULE     : gnutls.cpp
 * DESCRIPTION: interface with GnuTLS
 * COPYRIGHT  : (C) 2022  Grégoire Lecerf
 *                  2025  Robin Wils
 ******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "Gnutls/gnutls.hpp"
#include "analyze.hpp"
#include "list.hpp"
#include "string.hpp"

#ifdef USE_GNUTLS
#include "scheme.hpp"
#include "file.hpp"
#include "server_log.hpp"
#if defined(_WIN32) && defined (TEXMACS_FIX_1_GNUTLS)
#define GNUTLS_INTERNAL_BUILD
#endif
#include <gnutls/x509.h>
#include <gnutls/abstract.h>
#include <gnutls/crypto.h>
#include "client_server.hpp"


/******************************************************************************
 * Functionality provided by the plug-in
 ******************************************************************************/

bool
gnutls_present () {
  return true;
}

/******************************************************************************
 * TLS log facilities
 ******************************************************************************/

static int tm_gnutls_log_level= 0;

void tm_gnutls_log_callback (int level, const char* msg) {
  (void) level;
  GNUTLS_LOGI (msg);
}

/******************************************************************************
 * Global TLS settings
 ******************************************************************************/

static bool tm_gnutls_initialized= false;
static gnutls_anon_server_credentials_t tm_anon_server_credentials;
static gnutls_anon_client_credentials_t tm_anon_client_credentials;
static gnutls_certificate_credentials_t tm_x509_client_credentials;
static gnutls_certificate_credentials_t tm_x509_server_credentials;
static const int tm_dh_bitsize= 2048;
static gnutls_dh_params_t tm_dh_parameters;
static const string tm_x509_cert_path= "$TEXMACS_HOME_PATH/server/cert.pem";
static const string tm_x509_key_path= "$TEXMACS_HOME_PATH/server/key.pem";
static const string tm_x509_trusted_cas_path=
  "$TEXMACS_HOME_PATH/system/certificates/trusted-certificates.crt";
static gnutls_x509_crt_t server_certificate = NULL;
static unsigned int cert_verify_flags = 0;

static void
tm_initialize_tls () {
  static bool first_call= true;
  if (!first_call) return;
  first_call= false;
  if (tm_gnutls_initialized) return;

  int ret = 0;

  /* for backwards compatibility with gnutls < 3.3.0 */
  CHECK_GNUTLS_INIT (ret, gnutls_global_init ());
  if (DEBUG_GNUTLS) tm_gnutls_log_level= 4;
  gnutls_global_set_log_level (tm_gnutls_log_level);
  gnutls_global_set_log_function (tm_gnutls_log_callback);

  CHECK_GNUTLS_INIT (ret,
    gnutls_anon_allocate_server_credentials (&tm_anon_server_credentials));
  CHECK_GNUTLS_INIT (ret,
    gnutls_anon_allocate_client_credentials (&tm_anon_client_credentials));

  // X509 server
  url cert_path (tm_x509_cert_path);
  url key_path (tm_x509_key_path);

  GNUTLS_LOG ("x509 Certificate Path: " * as_string (cert_path));
  GNUTLS_LOG ("x509 Key Path: " * as_string (key_path));

  CHECK_GNUTLS_INIT (ret,
    gnutls_certificate_allocate_credentials (&tm_x509_server_credentials));
  if (exists (cert_path) && exists (key_path)) {
    c_string _cert_path (as_string (cert_path));
    c_string _key_path (as_string (key_path));
    CHECK_GNUTLS_INIT (ret,
        gnutls_certificate_set_x509_key_file
	  (tm_x509_server_credentials, _cert_path,
	   _key_path,GNUTLS_X509_FMT_PEM));
  } else if (!exists (cert_path)) {
    GNUTLS_LOG ("Certificate file not found: " * as_string (cert_path));
  } else {
    GNUTLS_LOG ("Key file not found: " * as_string (key_path));
  }

  // X509 client
  CHECK_GNUTLS_INIT (ret,
    gnutls_certificate_allocate_credentials (&tm_x509_client_credentials));
  CHECK_GNUTLS_INIT (ret,
    gnutls_certificate_set_x509_system_trust (tm_x509_client_credentials));

  url trusted_path (tm_x509_trusted_cas_path);
  if (exists (trusted_path)) {
    c_string _trusted_path (as_string (trusted_path));
    CHECK_GNUTLS_INIT (ret,
      gnutls_certificate_set_x509_trust_file (tm_x509_client_credentials,
        _trusted_path, GNUTLS_X509_FMT_PEM));
  }

  CHECK_GNUTLS_INIT (ret, gnutls_dh_params_init (&tm_dh_parameters));
  CHECK_GNUTLS_INIT (ret,
    gnutls_dh_params_generate2 (tm_dh_parameters, tm_dh_bitsize));

#if GNUTLS_VERSION_NUMBER >= 0x030506
  /* only available since GnuTLS 3.5.6, on previous versions see
   * gnutls_certificate_set_dh_params(). */
  gnutls_certificate_set_known_dh_params (tm_x509_server_credentials,
      GNUTLS_SEC_PARAM_MEDIUM);
#endif

  gnutls_anon_set_server_dh_params (tm_anon_server_credentials,
    tm_dh_parameters);

  tm_gnutls_initialized= true;
  GNUTLS_LOG ("GnuTLS initialization succeeded");
}

static void
tm_deinitialize_tls () {
  if (!tm_gnutls_initialized) return;
  gnutls_anon_free_server_credentials (tm_anon_server_credentials);
  gnutls_anon_free_client_credentials (tm_anon_client_credentials);
  gnutls_certificate_free_credentials (tm_x509_client_credentials);
  gnutls_certificate_free_credentials (tm_x509_server_credentials);
  gnutls_global_deinit ();
  tm_gnutls_initialized= false;
  GNUTLS_LOG ("GnuTLS deinitialization succeeded");
}

// Lazy initialization

struct tls_global_session {
  tls_global_session () {};
  ~tls_global_session () { tm_deinitialize_tls (); }
};

static void
tls_ensure_initialization () {
  // Deinitialization is done at exit
  static tls_global_session _tls_global_session;
  if (!tm_gnutls_initialized) tm_initialize_tls ();
}

/******************************************************************************
 * X.509 authentication
 ******************************************************************************/

static string
as_string_gnutls_datum (gnutls_datum_t data, bool full = false) {
  string str (reinterpret_cast<char*> (data.data));
  if (!full) {
    return str;
  }
  return "data: '" * str * "', size = " * as_string (data.size);
}

static string
as_string_gnutls_crt (const gnutls_x509_crt_t crt) {
  string info;

  {
    uint8_t serial[128] = {0};
    size_t serial_size = sizeof(serial);
    int err;

    err =
      gnutls_x509_crt_get_serial(crt, serial, &serial_size);
    if (err < 0)
      info << "Serial error: " << gnutls_strerror (err) << "\n";
    else {
      info << "Serial: ";
      for (size_t i = 0; i < serial_size; i++) {
        info << as_hexadecimal (serial[i]);
      }
      info << "\n";
    }
  }

  {
    size_t len;
    int err;

#define GNUTLS_X509_COMMON_NAME_SIZE 256
#define GNUTLS_X509_COUNTRY_NAME_SIZE 3
#define GNUTLS_X509_ORGANIZATION_NAME_SIZE 256
#define GNUTLS_X509_ORGANIZATIONAL_UNIT_NAME_SIZE 256
#define GNUTLS_X509_LOCALITY_NAME_SIZE 256
#define GNUTLS_X509_STATE_OR_PROVINCE_NAME_SIZE 256
#define _GET_DN_OID(name, print) \
    do { \
      char name[GNUTLS_X509_ ## name ## _SIZE] = {0}; \
      len = GNUTLS_X509_ ## name ## _SIZE; \
      err = gnutls_x509_crt_get_dn_by_oid (crt, \
          GNUTLS_OID_X520_ ## name, 0, 0, name, &len); \
      if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE) { \
        info << print ":\n"; \
      } else if (err < 0) { \
        info << print " error: " << gnutls_strerror (err) << "\n"; \
      } else { \
        info << print ": " << name << "\n"; \
      } \
    } while (0)

    _GET_DN_OID (COMMON_NAME, "Common Name");
    _GET_DN_OID (ORGANIZATION_NAME, "Organization");
    _GET_DN_OID (ORGANIZATIONAL_UNIT_NAME, "Unit");
    _GET_DN_OID (COUNTRY_NAME, "Country");
    _GET_DN_OID (STATE_OR_PROVINCE_NAME, "State");
    _GET_DN_OID (LOCALITY_NAME, "City");
  }

  {
    time_t tim;

    tim = gnutls_x509_crt_get_activation_time(crt);
    if (tim != -1) {
      char s[42];
      size_t max = sizeof(s);
      struct tm t;

      if (gmtime_r(&tim, &t) == NULL)
        info << "Not Before error: gmtime_r (" << (unsigned long) tim
          << ")\n";
      else if (strftime (s, max, "%a %b %d %H:%M:%S UTC %Y", &t) == 0)
        info << "Not Before error: strftime (" << (unsigned long) tim
          << ")\n";
      else
        info << "Not Before: " << s << "\n";
    } else {
      info << "Not Before: unknown\n";
    }

    tim = gnutls_x509_crt_get_expiration_time(crt);
    if (tim != -1) {
      char s[42];
      size_t max = sizeof(s);
      struct tm t;

      if (gmtime_r(&tim, &t) == NULL)
        info << "Not After error: gmtime_r (" << (unsigned long) tim
          << ")\n";
      else if (strftime (s, max, "%a %b %d %H:%M:%S UTC %Y", &t) == 0)
        info << "Not After error: strftime (" << (unsigned long) tim
          << ")\n";
      else
        info << "Not After: " << s << "\n";
    } else {
      info << "Not After: unknown\n";
    }
  }

  {
    gnutls_datum_t dn;
    int err;

    err = gnutls_x509_crt_get_issuer_dn3(crt, &dn, 0);
    if (err == GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE) {
      info << "Issuer:\n";
    } else if (err < 0) {
      info << "Issuer error: " << gnutls_strerror (err) << "\n";
    } else {
      info << "Issuer: " << as_string (dn.data) << "\n";
      gnutls_free(dn.data);
    }
  }

  return info;
}


/**
 * @brief allocate certificate from gnutls_datum
 *
 * @param cert_data certificate data
 * @return returns a certificate structure to be used with gnutls, must be
 * deinit by the user
 */
static gnutls_x509_crt_t
tm_get_cert (const gnutls_datum_t* cert_data, gnutls_x509_crt_fmt_t fmt) {
  gnutls_x509_crt_t cert = NULL;

  if (gnutls_x509_crt_init (&cert) == 0 &&
      gnutls_x509_crt_import (cert, cert_data, fmt) == 0) {
    return cert;
  }
  return NULL;
}

static gnutls_x509_crt_t
tm_get_cert (const string cert_str, gnutls_x509_crt_fmt_t fmt) {
	c_string _cert_str (cert_str);

  gnutls_datum_t cert_data {
    .data= reinterpret_cast<unsigned char*> ((char *) _cert_str),
    .size= (unsigned int) N (cert_str),
  };

  return tm_get_cert (&cert_data, fmt);
}

static string
tm_cert_as_pem_string (gnutls_x509_crt_t cert) {
  gnutls_datum_t out= {NULL, 0};
  string pem;
  int ret;

  ret = gnutls_x509_crt_export2 (cert, GNUTLS_X509_FMT_PEM, &out);
  if (ret < 0) {
    return "";
  }

  pem = as_string_gnutls_datum (out);
  gnutls_free (out.data);

  return pem;
}

static int
trust_certificate (gnutls_x509_crt_t crt, string crt_pem) {
  bool deinit_cert= false;

  if (!crt) {
    if (N (crt_pem) == 0) {
      return GNUTLS_E_CERTIFICATE_ERROR;
    }

    crt = tm_get_cert (crt_pem, GNUTLS_X509_FMT_PEM);
    deinit_cert = true;
  }

  if (!crt) {
    return GNUTLS_E_CERTIFICATE_ERROR;
  }

  int ret = gnutls_certificate_set_x509_trust (tm_x509_client_credentials,
      &crt, 1);
  if (ret < 0) {
    return ret;
  }

  // write to our custom trust store
  if (N (crt_pem) == 0) {
    gnutls_datum_t out;
    ret = gnutls_x509_crt_export2 (crt, GNUTLS_X509_FMT_PEM, &out);
    if (ret < 0) {
      return ret;
    }

    crt_pem = as_string_gnutls_datum (out);
    gnutls_free (out.data);
  }

  if (!append_string (tm_x509_trusted_cas_path, crt_pem)) {
    return ret;
  }

  if (deinit_cert) gnutls_x509_crt_deinit (crt);

  return 0;
}

bool
trust_certificate (const string& crt_pem) {
  return trust_certificate (NULL, crt_pem) == 0;
}

void
disable_certificate_time_checks () {
  cert_verify_flags |= GNUTLS_VERIFY_DISABLE_TRUSTED_TIME_CHECKS |
      GNUTLS_VERIFY_DISABLE_TIME_CHECKS;
}

static int cert_out_callback (gnutls_x509_crt_t cert,
    gnutls_x509_crt_t issuer,
    gnutls_x509_crl_t crl,
    unsigned int verification_output)
{
  (void) crl;
  gnutls_datum_t tmp= {NULL,0}, txt= {NULL,0};
  int ret;

  GNUTLS_LOG ("Printing full certificate path validation to trust root.");

  GNUTLS_LOG ("\tCertificate:\n" * as_string_gnutls_crt (cert));

  if (issuer != NULL) {
    GNUTLS_LOG ("\tIssuer Certificate:\n" * as_string_gnutls_crt (issuer));
  }

  if (verification_output) {
    GNUTLS_LOG ("\tNot Verified");
  } else {
    GNUTLS_LOG ("\tVerified");
  }

  ret = gnutls_certificate_verification_status_print (verification_output,
      GNUTLS_CRT_X509, &txt, 0);
  if (ret < 0) {
    GNUTLS_ERR_LOGE (ret, "gnutls_certificate_verification_status_print");
    return 1;
  }

  GNUTLS_LOG ("verification status (" * as_string (verification_output) * "): "
      * as_string_gnutls_datum (txt));

  ret = gnutls_x509_crt_export2 (cert, GNUTLS_X509_FMT_DER, &tmp);
  if (ret < 0) {
    gnutls_free (txt.data);
    GNUTLS_ERR_LOGW (ret, "gnutls cert copy (export der)");
    return 1;
  }

  ret = gnutls_x509_crt_init (&server_certificate);
  if (ret < 0) {
    gnutls_free (txt.data);
    GNUTLS_ERR_LOGW (ret, "gnutls cert copy (init)");
    return 1;
  }

  ret = gnutls_x509_crt_import (server_certificate, &tmp ,GNUTLS_X509_FMT_DER);
  if (ret < 0) {
    GNUTLS_ERR_LOGW (ret, "gnutls cert copy (import der)");
  }

  gnutls_free (txt.data);
  return 0;
}

static inline bool
handle_cert_error_interactive (unsigned int status) {
  return status & GNUTLS_CERT_SIGNER_NOT_FOUND ||
    status & GNUTLS_CERT_NOT_ACTIVATED ||
    status & GNUTLS_CERT_EXPIRED;
}

static inline string
as_unquoted_string (tree t) { return scm_unquote (as_string (t)); }

static crt_cfg
crt_cfg_from_tree (tree cfg) {
  std::string s;
  crt_cfg h ("");

  for (int i=0; i<N (cfg); i++) {
    if (is_tuple (cfg[i])) {
      h (as_unquoted_string (cfg[i][0])) = as_unquoted_string (cfg[i][1]);
    }
  }

  return h;
}

string certificate_path ()    { return concretize (tm_x509_cert_path); }
bool   certificate_present () { return exists (tm_x509_cert_path); }

bool
generate_self_signed (tree cfg_tree, url cert_path, url key_path)
{
  gnutls_privkey_t pkey;
  gnutls_x509_privkey_t x509_pkey;
  gnutls_pubkey_t pubkey;
  gnutls_digest_algorithm_t dig;
  gnutls_x509_crt_t crt;
  gnutls_datum_t crt_out;
  char key_out[PRIVKEY_OUTPUT_BUFSIZE];
  size_t key_out_size = PRIVKEY_OUTPUT_BUFSIZE;
  int ret;
  unsigned char serial[CRT_SERIAL_SIZE];
  time_t secs;

  if (!is_tuple (cfg_tree)) {
    GNUTLS_LOGE ("wrong certificate config given");
    return false;
  }

  crt_cfg cfg = crt_cfg_from_tree (cfg_tree);

  CHECK_GNUTLS_BRETURN (ret, gnutls_privkey_init (&pkey));
  CHECK_GNUTLS_BRETURN (ret, gnutls_privkey_generate (pkey, PRIVKEY_ALGO,
        gnutls_sec_param_to_pk_bits (PRIVKEY_ALGO, PRIVKEY_SEC_PARAM),
        0));
  CHECK_GNUTLS_BRETURN (ret, gnutls_privkey_verify_params (pkey));

  CHECK_GNUTLS_BRETURN (ret, gnutls_pubkey_init (&pubkey));
  CHECK_GNUTLS_BRETURN (ret, gnutls_pubkey_import_privkey (pubkey, pkey, 0, 0));
  CHECK_GNUTLS_BRETURN (ret,
      gnutls_pubkey_get_preferred_hash_algorithm (pubkey, &dig, NULL));

  GNUTLS_LOG ("Generating a self signed certificate...");

  CHECK_GNUTLS_BRETURN (ret,
      gnutls_rnd (GNUTLS_RND_NONCE, serial, CRT_SERIAL_SIZE));

  // serial must be positive and max 20 octets
  // so we must zero the most significant bit (with MSB set, the DER encoding
  // would be 21 octets long). See RFC 5280, section 4.1.2.2.
  serial[0] &= 0x7F;

  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_init (&crt));

  if (cfg->contains ("country")) {
    c_string _country (cfg["country"]);
    CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_dn_by_oid (crt,
          GNUTLS_OID_X520_COUNTRY_NAME, 0, _country, N (cfg["country"])));
  }

  if (cfg->contains ("locality")) {
    c_string _loc (cfg["locality"]);
    CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_dn_by_oid (crt,
          GNUTLS_OID_X520_LOCALITY_NAME, 0, _loc, N (cfg["locality"])));
  }

  if (cfg->contains ("state")) {
    c_string _st (cfg["state"]);
    CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_dn_by_oid (crt,
          GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME, 0, _st, N (cfg["state"])));
  }

  if (cfg->contains ("organization")) {
    c_string _org (cfg["organization"]);
    CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_dn_by_oid (crt,
          GNUTLS_OID_X520_ORGANIZATION_NAME, 0, _org, N (cfg["organization"])));
  }

  if (cfg->contains ("unit")) {
    c_string _unit (cfg["unit"]);
    CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_dn_by_oid (crt,
          GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME, 0, _unit, N (cfg["unit"])));
  }

  if (cfg->contains ("cn")) {
    c_string _cn (cfg["cn"]);
    CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_dn_by_oid (crt,
          GNUTLS_OID_X520_COMMON_NAME, 0, _cn, N (cfg["cn"])));
  }

  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_pubkey (crt, pubkey));
  gnutls_pubkey_deinit (pubkey);

  CHECK_GNUTLS_BRETURN (ret,
      gnutls_x509_crt_set_serial (crt, serial, CRT_SERIAL_SIZE));

  secs = time (NULL);
  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_activation_time (crt, secs));

  secs += 730 * 24 * 60 * 60;
  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_expiration_time (crt, secs));

  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_basic_constraints (crt, 0, -1));

  if (cfg->contains ("uri")) {
    c_string _uri (cfg["uri"]);
    CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_subject_alt_name (crt,
          GNUTLS_SAN_RFC822NAME, _uri, N (cfg["uri"]), GNUTLS_FSAN_APPEND));
  }

  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_key_purpose_oid (crt,
        GNUTLS_KP_TLS_WWW_SERVER, 0));
  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_key_usage (crt,
        GNUTLS_KEY_DIGITAL_SIGNATURE));

  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_crt_set_version (crt, 3));

  CHECK_GNUTLS_BRETURN (ret,
      gnutls_x509_crt_privkey_sign (crt, crt, pkey, dig, 0));

  CHECK_GNUTLS_BRETURN (ret, gnutls_privkey_export_x509 (pkey, &x509_pkey));
  CHECK_GNUTLS_BRETURN (ret, gnutls_x509_privkey_export_pkcs8 (x509_pkey,
        GNUTLS_X509_FMT_PEM, NULL, GNUTLS_PKCS_NULL_PASSWORD,
        key_out, &key_out_size));

  string _key_out (key_out, key_out_size);
  save_string (key_path, _key_out);

  CHECK_GNUTLS_BRETURN (ret,
      gnutls_x509_crt_export2 (crt, GNUTLS_X509_FMT_PEM, &crt_out));

  save_string (cert_path, as_string_gnutls_datum (crt_out));
  gnutls_free (crt_out.data);

  gnutls_x509_crt_deinit (crt);
  gnutls_x509_privkey_deinit (x509_pkey);
  gnutls_privkey_deinit (pkey);

  return true;
}

/******************************************************************************
 * Error messages for sockets
 ******************************************************************************/

static string
as_string_gnutls_error (int e, int cert_verification) {
  if (e == GNUTLS_E_SUCCESS)
    return string ("");
  if (e == TM_NET_SESSION_INACTIVE)
    return string ("unexpected dead GnuTLS session");
  if (e == GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR &&
      handle_cert_error_interactive (cert_verification))
    return string ("certificate verify interactive");
  if (e == GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR) {
    gnutls_datum_t txt= {NULL,0};
    int ret = gnutls_certificate_verification_status_print (cert_verification,
        GNUTLS_CRT_X509, &txt, 0);
    if (ret < 0) {
      GNUTLS_ERR_LOGE (ret, "gnutls_certificate_verification_status_print");
      return string("cannot get certificate verification status text");
    }
    return as_string_gnutls_datum (txt);
  }
  const char* msg= gnutls_strerror (e);
  if (msg == NULL)
    return string ("unknown GnuTLS error");
  return string (msg);
}

/******************************************************************************
 * TLS server sessions
 ******************************************************************************/

static string
tm_session_info (gnutls_session_t session) {
  string info ("- Session:");
  gnutls_credentials_type_t cred;
  gnutls_kx_algorithm_t kx;
  int dhe, ecdh;
  gnutls_group_t group;
  char *desc;

  /* get a description of the session connection, protocol,
   * cipher/key exchange */
  desc = gnutls_session_get_desc (session);
  if (desc != NULL) {
    info << desc << "\n";
  } else {
    info << "(Unknown)\n";
  }

  dhe = ecdh = 0;

  kx = gnutls_kx_get (session);

  /* Check the authentication type used and switch
   * to the appropriate.
   */
  cred = gnutls_auth_get_type (session);
  switch (cred) {
    case GNUTLS_CRD_SRP:
      {
        const char* srp_username = gnutls_srp_server_get_username (session);
        info << "- SRP session";
        if (srp_username) {
          info << " with user name " << srp_username;
        }
        break;
      }

    case GNUTLS_CRD_PSK:
      if (gnutls_psk_client_get_hint (session) != NULL)
        info << "- PSK authentication. PSK hint '"
          << gnutls_psk_client_get_hint (session) << "'\n";

      if (gnutls_psk_server_get_username (session) != NULL)
        info << "- PSK authentication. Connected as '"
          << gnutls_psk_server_get_username (session) << "'\n";

      if (kx == GNUTLS_KX_ECDHE_PSK)
        ecdh = 1;
      else if (kx == GNUTLS_KX_DHE_PSK)
        dhe = 1;
      break;

    case GNUTLS_CRD_ANON: /* anonymous authentication */

      info << "- Anonymous authentication.\n";
      if (kx == GNUTLS_KX_ANON_ECDH)
        ecdh = 1;
      else if (kx == GNUTLS_KX_ANON_DH)
        dhe = 1;
      break;

    case GNUTLS_CRD_CERTIFICATE: /* certificate authentication */

      /* Check if we have been using ephemeral Diffie-Hellman.
      */
      if (kx == GNUTLS_KX_DHE_RSA || kx == GNUTLS_KX_DHE_DSS)
        dhe = 1;
      else if (kx == GNUTLS_KX_ECDHE_RSA ||
          kx == GNUTLS_KX_ECDHE_ECDSA)
        ecdh = 1;
      info << "- X.509 Auth\n";

      /* if the certificate list is available, then
       * print some information about it.
       * print_ 509_certificate_info(session);
       */
      break;
    case GNUTLS_CRD_IA:
      info << "- IA credential\n";
      break;
    default:
      info << "- Wrong credential type " << as_string (cred) << "\n";
      break;
  } /* switch */

  /* read the negotiated group - if any */
  group = gnutls_group_get (session);
  if (group != 0) {
    info << "- Negotiated group " << gnutls_group_get_name (group);
  } else {
    if (ecdh != 0)
      info << "- Ephemeral ECDH using curve "
        << gnutls_ecc_curve_get_name (gnutls_ecc_curve_get (session));
    else if (dhe != 0)
      info << "- Ephemeral DH using prime of " <<
        gnutls_dh_get_prime_bits (session) << " bitsn";
  }

  return info;
}

struct tls_server_contact_rep: tm_contact_rep {
  int io;
  pointer ptr;
  int error_number;
  bool handshake_in_progress;

  tls_server_contact_rep (array<array<string> > auths=
      array<array<string> > ());
  ~tls_server_contact_rep ();
  void start (int io);
  void stop ();
  int send (const void* buffer, size_t length);
  int receive (void* buffer, size_t length);
  bool alive ();
  bool active ();
  string last_error ();

private:
  void reset () { io=-1; ptr=(pointer) NULL; handshake_in_progress= false; }
};

tls_server_contact_rep::tls_server_contact_rep (array<array<string> > auths):
  tm_contact_rep (auths), io (-1), ptr (NULL),
  error_number (GNUTLS_E_SUCCESS), handshake_in_progress (false)
{
  tls_ensure_initialization ();
  type= SOCKET_SERVER;
}

tls_server_contact_rep::~tls_server_contact_rep () {
  stop ();
}

void
tls_server_contact_rep::start (int io2) {
  ASSERT (!active (), "contact already active");
  int e;
  gnutls_session_t s;
  if (!handshake_in_progress) {
    error_number= GNUTLS_E_SUCCESS;
    if (N (args) == 0) {
      GNUTLS_LOGW ("Missing credentials for starting TLS session for client "
          * as_string (io2));
      io= -1;
      return;
    }
    io= io2;
    GNUTLS_LOGI ("GnuTLS session starting for client " * as_string (io));

    e= gnutls_init (&s, GNUTLS_SERVER|GNUTLS_NONBLOCK);
    if (e != GNUTLS_E_SUCCESS && e < 0) {
      GNUTLS_ERR_LOGE (e, "server init");
      reset ();
      return;
    }

    ptr= (pointer) s;

    string priority= "NORMAL";
    for (int i= 0; i < N (args); i++) {
      if (N (args[i]) == 0) continue;
      else if (args[i][0] == string ("anonymous")) {
        gnutls_credentials_set (s, GNUTLS_CRD_ANON,
            tm_anon_server_credentials);
        priority = priority * ":+ANON-DH";
      }
      else
        GNUTLS_LOGW ("Unknown GnuTLS credential type " * as_string (args[i]));
    }
    // do not request any certificate from the client.
    gnutls_certificate_server_set_request (s, GNUTLS_CERT_IGNORE);
    gnutls_credentials_set (s, GNUTLS_CRD_CERTIFICATE,
        tm_x509_server_credentials);
    c_string _priority (priority);
    e= gnutls_priority_set_direct (s, _priority, NULL);
    if (e != GNUTLS_E_SUCCESS && e < 0) {
      GNUTLS_ERR_LOGE (e, priority * " priority set ");
      gnutls_deinit (s);
      reset ();
      return;
    }
    gnutls_dh_set_prime_bits (s, tm_dh_bitsize);
    gnutls_transport_set_int2 (s, io, io);
    string _timeout= get_preference ("server contact timeout");
    int timeout= is_int (_timeout) ? as_int (_timeout) : 0;
    if (timeout == 0)
      GNUTLS_LOGW ("server connection timeout is disabled");
    gnutls_handshake_set_timeout (s, timeout);
  } else {
    s= (gnutls_session_t) ptr;
  }

  e= gnutls_handshake (s);
  if (e == GNUTLS_E_SUCCESS) {
    GNUTLS_LOGI ("GnuTLS session started for client " * as_string (io));
    GNUTLS_LOGI (tm_session_info (s));
    handshake_in_progress= false;
  } else if (e == GNUTLS_E_AGAIN || e == GNUTLS_E_INTERRUPTED) {
    GNUTLS_LOG ("GnuTLS handshake in progress for client " * as_string (io));
    handshake_in_progress= true;
  } else if (e < 0) {
    GNUTLS_ERR_LOGE (e, "session handshake for client " * as_string (io));
    GNUTLS_LOGE (tm_session_info (s));
    gnutls_deinit (s);
    reset ();
    return;
  }

  gnutls_credentials_type_t cred_type= gnutls_auth_server_get_type (s);
  if (cred_type == GNUTLS_CRD_ANON)
    GNUTLS_LOGI ("client " * as_string (io) * " authenticated anonymously");
}

void
tls_server_contact_rep::stop () {
  if (!alive ()) return;
  gnutls_credentials_type_t cred_type=
    gnutls_auth_server_get_type ((gnutls_session_t) ptr);
  if (cred_type != GNUTLS_CRD_ANON && cred_type != GNUTLS_CRD_CERTIFICATE)
    GNUTLS_LOGW ("unknown credential type for client " * as_string (io));

  gnutls_bye ((gnutls_session_t) ptr, GNUTLS_SHUT_WR);
  gnutls_deinit ((gnutls_session_t) ptr);
  GNUTLS_LOGI ("GnuTLS closed session for client " * as_string (io));
  reset ();
}

int
tls_server_contact_rep::send (const void* buffer, size_t length) {
  if (!active ()) {
    GNUTLS_LOGE ("unexpected inactive GnuTLS session while sending to client "
        * as_string (io));
    return TM_NET_SESSION_INACTIVE;
  }
  int r= gnutls_record_send ((gnutls_session_t) ptr, buffer, length);
  error_number= r < 0 ? r : GNUTLS_E_SUCCESS;
  if (r < 0 &&
      r != GNUTLS_E_SUCCESS &&
      r != GNUTLS_E_AGAIN &&
      r != GNUTLS_E_INTERRUPTED) stop ();
  return r;
}

int
tls_server_contact_rep::receive (void* buffer, size_t length) {
  if (!active ()) {
    GNUTLS_LOGE ("unexpected inactive GnuTLS session while receiving "
        "from client " * as_string (io));
    return TM_NET_SESSION_INACTIVE;
  }
  int r= gnutls_record_recv ((gnutls_session_t) ptr, buffer, length);
  error_number= r < 0 ? r : GNUTLS_E_SUCCESS;
  if (r < 0 &&
      r != GNUTLS_E_SUCCESS &&
      r != GNUTLS_E_AGAIN &&
      r != GNUTLS_E_INTERRUPTED) stop ();
  return r;
}

string
tls_server_contact_rep::last_error () {
  return as_string_gnutls_error (error_number, 0);
}

bool
tls_server_contact_rep::alive () {
  // Cannot be alive if GnuTLS is not present
  return ptr != (pointer) NULL;
}

bool
tls_server_contact_rep::active () {
  // Cannot be active if GnuTLS is not present
  return alive () && !handshake_in_progress;
}

tm_contact 
make_tls_server_contact (array<array<string> > authentications) {
  return (tm_contact_rep*) tm_new<tls_server_contact_rep> (authentications);
}

/******************************************************************************
 * TLS client contacts
 ******************************************************************************/

struct tls_client_contact_rep: tm_contact_rep {
  int io;
  pointer ptr;
  int error_number;
  unsigned int cert_verif_status;
  bool handshake_in_progress;
  tls_client_contact_rep (array<array<string> > args=
      array<array<string> > ());
  ~tls_client_contact_rep ();
  void start (int io);
  void stop ();
  int send (const void* buffer, size_t length);
  int receive (void* buffer, size_t length);
  bool alive ();
  bool active ();
  string last_error ();

private:
  /**
   * @brief initiates TLS handshake with GnuTLS
   *
   * @param s GnuTLS session
   * @return 0 on success, 1 on error, 2 on retry
   */
  int handshake (gnutls_session_t s);
  int verify ();
  void reset () { io=-1; ptr=(pointer) NULL; handshake_in_progress= false; }
};

tls_client_contact_rep::tls_client_contact_rep (array<array<string> > creds):
  tm_contact_rep (creds), io (-1), ptr (NULL),
  error_number (GNUTLS_E_SUCCESS), handshake_in_progress (false)
{
  tls_ensure_initialization ();
  type= SOCKET_CLIENT;
}

tls_client_contact_rep::~tls_client_contact_rep () {
  stop ();
}

int
tls_client_contact_rep::handshake (gnutls_session_t s) {
  int ret= gnutls_handshake (s);
  if (ret == GNUTLS_E_SUCCESS) {
    GNUTLS_LOGI ("GnuTLS session started for client " * as_string (io));
    GNUTLS_LOG (tm_session_info (s));
    handshake_in_progress= false;
    return ret;
  } else if (ret == GNUTLS_E_AGAIN || ret == GNUTLS_E_INTERRUPTED) {
    GNUTLS_LOG ("GnuTLS handshake in progress for " * as_string (io));
    handshake_in_progress = true;
    error_number= ret;
    return ret;
  } else if (ret < 0) {
    error_number= ret;
    GNUTLS_ERR_LOGE (ret, "handshake for client " * as_string (io));
    reset ();
  }

  if (ret == GNUTLS_E_CERTIFICATE_VERIFICATION_ERROR &&
      server_certificate != NULL) {
    cert_verif_status= gnutls_session_get_verify_cert_status(s);

    // ask user for server cert confirmation if it isn't a trusted cert
    // or expired/not active
    if (!handle_cert_error_interactive (cert_verif_status))
      return ret;

    string msg;

    string crt_str = as_string_gnutls_crt (server_certificate);
    if (crt_str == "") {
      char serial[128] = {0};
      size_t serial_size = sizeof (serial);
      int ret =
        gnutls_x509_crt_get_serial (server_certificate, serial, &serial_size);
      if (ret < 0) {
        GNUTLS_ERR_LOGE (ret, "cert get serial");
        gnutls_deinit (s);
        reset ();
        return ret;
      }
      msg = string (serial);
    } else {
      msg = crt_str;
    }

    GNUTLS_LOGW (msg);

    if (cert_verif_status & GNUTLS_CERT_SIGNER_NOT_FOUND) {
      call ("trust-certificate-interactive",
          object (msg),
          object (tm_cert_as_pem_string (server_certificate)));
    } else {
      call ("disable-certificate-time-checks-interactive", object (msg));
    }

    gnutls_x509_crt_deinit (server_certificate);
    server_certificate = NULL;
  }
  return ret;
}

void
tls_client_contact_rep::start (int io2) {
  ASSERT (!active (), "contact already active");

  int ret;
  gnutls_session_t s;

  ret= error_number= GNUTLS_E_SUCCESS;
  if (!handshake_in_progress) {
    if (!tm_gnutls_initialized) return;
    if (N (args) == 0) {
      GNUTLS_LOGE ("Missing credentials for starting TLS session for client "
          * as_string (io));
      reset ();
      return;
    }

    io= io2;
    GNUTLS_LOG ("GnuTLS session starting for client " * as_string (io) * "...");

    ret = gnutls_init (&s, GNUTLS_CLIENT|GNUTLS_NONBLOCK);
    ptr= (pointer) s;
    string priority = "NORMAL";
    for (int i= 0; i < N (args); i++) {
      array<string> cred= args[i];
      if (N (cred) == 0) continue;
      if (N (cred) == 1 && cred[0] == "anonymous") {
        gnutls_credentials_set (s, GNUTLS_CRD_ANON,
            tm_anon_client_credentials);
        priority << ":+ANON-DH";
      }
      else {
        GNUTLS_LOGW ("Unknown credentials " * print_to_string (cred)
            * " for GnuTLS session of client " * as_string (io));
      }
    }

    gnutls_session_set_verify_cert (s, NULL, cert_verify_flags);
    gnutls_session_set_verify_output_function (s, cert_out_callback);

    ret = gnutls_credentials_set (s, GNUTLS_CRD_CERTIFICATE,
        tm_x509_client_credentials);
    if (ret != GNUTLS_E_SUCCESS) {
      GNUTLS_ERR_LOGE (ret, "x509 credentials set");
      gnutls_deinit (s);
      reset ();
      return;
    }

    c_string _priority (priority);
    ret = gnutls_priority_set_direct (s, _priority, NULL);
    if (ret != GNUTLS_E_SUCCESS) {
      GNUTLS_ERR_LOGE (ret, "setting priorities " * priority
          * " for client " * as_string (io));
      gnutls_deinit (s);
      reset ();
      return;
    }
    gnutls_transport_set_int2 (s, io, io);
    string _timeout= get_preference ("client contact timeout");
    int timeout= is_int (_timeout) ? as_int (_timeout) : 0;
    if (timeout == 0)
      GNUTLS_LOGW ("client connection timeout is disabled");
    gnutls_handshake_set_timeout (s, timeout);
  } else {
    s= (gnutls_session_t) ptr;
  }
  ret = handshake (s);
  if (ret < 0 && ret != GNUTLS_E_AGAIN && ret != GNUTLS_E_INTERRUPTED) {
    GNUTLS_LOGE ("GnuTLS session aborted for client " * as_string (io));
    gnutls_deinit (s);
    reset ();
  }
}

void
tls_client_contact_rep::stop () {
  if (alive ()) {
    gnutls_bye ((gnutls_session_t) ptr, GNUTLS_SHUT_RDWR);
    gnutls_deinit ((gnutls_session_t) ptr);
    ptr= (pointer) NULL;
    handshake_in_progress= false;
  }
  if (io >= 0) {
    GNUTLS_LOG ("GnuTLS closed session for client " * as_string (io));
    reset ();
  }
}

int
tls_client_contact_rep::send (const void* buffer, size_t length) {
  if (!active ()) {
    GNUTLS_LOGE ("Unexpected inactive GnuTLS session while sending to client "
      * as_string (io));
    return TM_NET_SESSION_INACTIVE;
  }
  int r= gnutls_record_send ((gnutls_session_t) ptr, buffer, length);
  error_number= r < 0 ? r : GNUTLS_E_SUCCESS;
  if (r < 0 &&
      r != GNUTLS_E_SUCCESS &&
      r != GNUTLS_E_AGAIN &&
      r != GNUTLS_E_INTERRUPTED) stop ();
  return r;
}

int
tls_client_contact_rep::receive (void* buffer, size_t length) {
  if (!active ()) {
    GNUTLS_LOGE ("Unexpected dead GnuTLS session while receiving "
        "from client " * as_string (io));
    return TM_NET_SESSION_INACTIVE;
  }
  int r= gnutls_record_recv ((gnutls_session_t) ptr, buffer, length);
  error_number= r < 0 ? r : GNUTLS_E_SUCCESS;
  if (r < 0 &&
      r != GNUTLS_E_SUCCESS &&
      r != GNUTLS_E_AGAIN &&
      r != GNUTLS_E_INTERRUPTED) stop ();
  return r;
}

string
tls_client_contact_rep::last_error () {
  string errstr= as_string_gnutls_error (error_number, cert_verif_status);

  return error_number == GNUTLS_E_PREMATURE_TERMINATION ?
    errstr * " Is TeXmacs server up ?" : errstr;
}

bool
tls_client_contact_rep::alive () {
  // Cannot be alive if GnuTLS is not present
  return ptr != (pointer) NULL;
}

bool
tls_client_contact_rep::active () {
  // Cannot be active if GnuTLS is not present
  return alive () && !handshake_in_progress;
}

tm_contact 
make_tls_client_contact (array<array<string> > args) {
  return (tm_contact_rep*) tm_new<tls_client_contact_rep> (args);
}

/******************************************************************************
* PBKDF2 password hash
******************************************************************************/

string hash_password_pbkdf2 (string passwd, string salt) {
  c_string _pwd(passwd);
  c_string _salt(salt);

  gnutls_datum_t key_data = {
    .data = (unsigned char *)(char *)_pwd,
    .size = static_cast<unsigned int> (N (passwd)),
  };
  gnutls_datum_t salt_data = {
    .data = (unsigned char *)(char *)_salt,
    .size = static_cast<unsigned int> (N (salt)),
  };
  uint8_t output_raw[4096] = {0};
  gnutls_datum_t out_data, out_b64, salt_b64;

  int ret = gnutls_pbkdf2 (GNUTLS_MAC_SHA256 , &key_data, &salt_data,
      MAC_SHA256_ITER_COUNT, output_raw, MAC_SHA256_BYTE_SIZE);
  if (ret < 0) {
    return "";
  }

  out_data.data = output_raw;
  out_data.size = MAC_SHA256_BYTE_SIZE;

  if (GNUTLS_E_SUCCESS != gnutls_base64_encode2 (&out_data, &out_b64)) {
    return "";
  }

  if (GNUTLS_E_SUCCESS != gnutls_base64_encode2 (&salt_data, &salt_b64)) {
    return "";
  }

  return string ("$7$") * as_string (MAC_SHA256_ITER_COUNT)
    * "$" * as_string (salt_b64.data)
    * "$" * as_string (out_b64.data);
}

#else // USE_GNUTLS

/******************************************************************************
 * If Gnutls is not present...
 ******************************************************************************/

bool gnutls_present () {
  return false;
}

tm_contact
make_tls_server_contact (array<array<string> > args) {
  (void) args;
  return tm_contact ();
}

tm_contact
make_tls_client_contact (array<array<string> > args) {
  (void) args;
  return tm_contact ();
}

list<string>
srp_verifier_and_salt (string pseudo, string passwd) {
  (void) pseudo; (void) passwd;
  return list<string> ();
}

string hash_password_pbkdf2 (string passwd, string salt) {
  (void) passwd; (void) salt;
  return string("");
}

string
certificate_path () {
  return "";
}

bool
certificate_present () {
  return true;
}

bool
trust_certificate (const string& crt_pem) {
  return true;
}

bool
generate_self_signed (tree cfg_tree, url cert_path, url key_path) {
  return true;
}

#endif // USE_GNUTLS
