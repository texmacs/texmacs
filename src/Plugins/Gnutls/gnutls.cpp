
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
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>
#include <gnutls/crypto.h>
#include "client_server.hpp"

#ifdef QTTEXMACS
#include <QMessageBox>
#endif

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
  server_log_write (log_info, msg);
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

static void
tm_initialize_tls () {
  static bool first_call= true;
  if (!first_call) return;
  first_call= false;
  if (tm_gnutls_initialized) return;

  int ret = 0;

  /* for backwards compatibility with gnutls < 3.3.0 */
  CHECK_GNUTLS_INIT(ret, gnutls_global_init ());
  if (DEBUG_GNUTLS) tm_gnutls_log_level= 4;
  gnutls_global_set_log_level (tm_gnutls_log_level);
  gnutls_global_set_log_function (tm_gnutls_log_callback);

  CHECK_GNUTLS_INIT(ret,
    gnutls_anon_allocate_server_credentials (&tm_anon_server_credentials));
  CHECK_GNUTLS_INIT(ret,
    gnutls_anon_allocate_client_credentials (&tm_anon_client_credentials));

  // X509 server
  url cert_path (tm_x509_cert_path);
  url key_path (tm_x509_key_path);

  GNUTLS_LOG("x509 Certificate Path: " * as_string (cert_path));
  GNUTLS_LOG("x509 Key Path: " * as_string (key_path));

  CHECK_GNUTLS_INIT(ret,
    gnutls_certificate_allocate_credentials (&tm_x509_server_credentials));
  if (exists (cert_path) && exists (key_path)) {
    c_string _cert_path (as_string (cert_path));
    c_string _key_path (as_string (cert_path));
    CHECK_GNUTLS_INIT(ret,
        gnutls_certificate_set_x509_key_file (tm_x509_server_credentials,
          _cert_path,
          _key_path,
          GNUTLS_X509_FMT_PEM));
  } else if (!exists (cert_path)) {
    GNUTLS_LOG("Certificate file not found: " * as_string (cert_path));
  } else {
    GNUTLS_LOG("Key file not found: " * as_string (key_path));
  }

  // X509 client
  CHECK_GNUTLS_INIT(ret,
    gnutls_certificate_allocate_credentials (&tm_x509_client_credentials));
  CHECK_GNUTLS_INIT(ret,
    gnutls_certificate_set_x509_system_trust (tm_x509_client_credentials));

  url trusted_path (tm_x509_trusted_cas_path);
  if (exists (trusted_path)) {
    c_string _trusted_path (as_string (trusted_path));
    CHECK_GNUTLS_INIT(ret,
      gnutls_certificate_set_x509_trust_file (tm_x509_client_credentials,
        _trusted_path, GNUTLS_X509_FMT_PEM));
  }

  CHECK_GNUTLS_INIT(ret, gnutls_dh_params_init (&tm_dh_parameters));
  CHECK_GNUTLS_INIT(ret,
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
  GNUTLS_LOG("GnuTLS initialization succeeded");
}

static void
tm_deinitialize_tls () {
  if (!tm_gnutls_initialized) return;
  gnutls_anon_free_server_credentials (tm_anon_server_credentials);
  gnutls_anon_free_client_credentials (tm_anon_client_credentials);
  gnutls_certificate_free_credentials(tm_x509_client_credentials);
  gnutls_certificate_free_credentials(tm_x509_server_credentials);
  gnutls_global_deinit ();
  tm_gnutls_initialized= false;
  GNUTLS_LOG("GnuTLS deinitialization succeeded");
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
  string str(reinterpret_cast<char*>(data.data));
  if (!full) {
    return str;
  }
  return "data: '" * str * "', size = " * as_string (data.size);
}

static string
as_string_gnutls_crt (const gnutls_x509_crt_t crt) {
  gnutls_datum_t info;
  int ret = 0;

  ret = gnutls_x509_crt_print (crt,
      GNUTLS_CRT_PRINT_FULL,
      &info);
  if (ret < 0) {
    return "";
  }

  string crt_info = as_string_gnutls_datum(info);

  string line;
  int i = 0;
  string CN, OU, O, ST, C;
  string start_date, end_date;
  while (read_line (crt_info, i, line)) {
    int start;
    for (start=0;
        start<N (line) && (is_space (line[start]) || line[start] == '\t');
        start++) ;
    line = line (start, N (line));

    if (starts (line, "Issuer")) {
      int pos = tm_search_forwards (" ", 0, line) + 1;
      for (int next_field = 0;
          next_field >= 0 ;
          pos = next_field + 1) {
        next_field = tm_search_forwards(",", pos, line);
        int value_delim = tm_search_forwards ("=", pos, line);
        string field = line (pos, value_delim);
        GNUTLS_LOG(as_string (value_delim) * ", " * next_field * " field '" * field * "'");
        string value;
        if (next_field == -1) {
          value = line (value_delim+1, N (line));
        } else {
          value = line (value_delim+1, next_field);
          pos = next_field + 1;
        }
        GNUTLS_LOG("value '" * value * "'");
        if (field == "CN") CN = value;
        else if (field == "OU") OU = value;
        else if (field == "O") O = value;
        else if (field == "ST") ST = value;
        else if (field == "C") C = value;
      }
    } else if (starts (line, "Not Before")) {
      start_date = line (tm_search_forwards (":", 0, line) + 1, N (line));
    } else if (starts (line, "Not After")) {
      end_date = line (tm_search_forwards (":", 0, line) + 1, N (line));
    }
  }

  gnutls_free(info.data);

  return "Common Name: " * CN * "\n"
    * "Organization Unit: " * OU * "\n"
    * "Organization: " * O * "\n"
    * "State: " * ST * "\n"
    * "Country Name: " * C * "\n"
    * "Start Date: " * start_date * "\n"
    * "End Date: " * end_date * "\n";
}


/**
 * @brief allocate certificate from gnutls_datum
 *
 * @param cert_data certificate data
 * @return returns a certificate structure to be used with gnutls, must be
 * deinit by the user
 */
static gnutls_x509_crt_t
tm_get_cert (const gnutls_datum_t* cert_data) {
	string cert_info;
  gnutls_x509_crt_t cert = NULL;

  if (gnutls_x509_crt_init(&cert) == 0 &&
      gnutls_x509_crt_import(cert, cert_data,
        GNUTLS_X509_FMT_DER) ==
      0) {
    return cert;
  }
  return NULL;
}

__attribute__((unused)) static int
tm_trust_cert () {
  if (!server_certificate) {
    return GNUTLS_E_CERTIFICATE_ERROR;
  }

  int ret = gnutls_certificate_set_x509_trust (tm_x509_client_credentials,
      &server_certificate, 1);
  if (ret < 0) {
    return ret;
  }

  // write to our custome trust store
  gnutls_datum_t out;
  ret = gnutls_x509_crt_export2(server_certificate, GNUTLS_X509_FMT_PEM, &out);
  if (ret < 0) {
    return ret;
  }

  if (!append_string(tm_x509_trusted_cas_path, as_string_gnutls_datum (out))) {
    return ret;
  }

  gnutls_free(out.data);

  // cert is trusted, we can safely remove local reference
  gnutls_x509_crt_deinit(server_certificate);
  server_certificate = NULL;
}

static bool
tm_trust_cert (const string& cert_serial) {
  char serial[128] = {0};
  size_t serial_size = sizeof (serial);

  int ret = gnutls_x509_crt_get_serial (server_certificate, serial, &serial_size);
  if (ret < 0) {
    io_warning << "cannot get server cert " << gnutls_strerror (ret);
    return false;
  }

  string server_cert_serial (serial);
  GNUTLS_LOG("trusting cert " * cert_serial * " and last server cert is"
      * server_cert_serial);

  if (server_cert_serial != cert_serial) {
    io_warning << "cannot trust cert " << cert_serial <<
      ", server cert remembered was " << server_cert_serial;
    return false;
  }

  ret = tm_trust_cert();
  if (ret < 0) {
    io_warning << "cannot trust cert " << cert_serial <<
      gnutls_strerror (ret);
    return false;
  }

  return true;
}

static int
certificate_client_verification_callback (gnutls_session_t session) {
  unsigned int status;
  int ret;
  unsigned int cert_list_size = 0;

  if (gnutls_certificate_type_get (session) != GNUTLS_CRT_X509) {
    return GNUTLS_E_CERTIFICATE_ERROR;
  }

  /* This verification function uses the trusted CAs in the credentials
   * structure. So you must have installed one or more CA certificates.
   * 
   * TODO: use gnutls_certificate_verify_peers3 to check hostname, but
   * we need access to it here, through user pointer for example:
   * gnutls_session_get_ptr(session), set with gnutls_transport_set_ptr
   * ret = gnutls_certificate_verify_peers3 (session, s->hostname, &status);
   */
  ret = gnutls_certificate_verify_peers2 (session, &status);
  if (ret < 0) {
      GNUTLS_LOG("Could not verify peer certificate due to an error");
    return GNUTLS_E_CERTIFICATE_ERROR;
  }

  const gnutls_datum_t* cert_list =
    gnutls_certificate_get_peers (session, &cert_list_size);
  if (cert_list_size < 1) {
    GNUTLS_LOG("Could not get peer certificate info");
    return GNUTLS_E_CERTIFICATE_ERROR;
  }

  gnutls_x509_crt_t crt = tm_get_cert (&cert_list[0]);
  gnutls_datum_t info;

  ret = gnutls_x509_crt_print (crt,
      GNUTLS_CRT_PRINT_ONELINE,
      &info);
  GNUTLS_LOG("Server certificate: " * as_string_gnutls_datum (info));
  gnutls_free(info.data);

  if (status) {
    gnutls_datum_t txt;
    ret = gnutls_certificate_verification_status_print (status,
        GNUTLS_CRT_X509, &txt, 0);
    if (ret >= 0) {
      GNUTLS_LOG("verification error (" * as_string (status) * "): " 
        * as_string_gnutls_datum (txt, true));

      server_certificate = crt;

      gnutls_free (txt.data);
    }
    return GNUTLS_E_CERTIFICATE_ERROR;
  }

  GNUTLS_LOG("Peer passed certificate verification");

  /* notify gnutls to continue handshake normally */
  return 0;
}

/******************************************************************************
 * Error messages for sockets
 ******************************************************************************/

static string
as_string_gnutls_error (int e) {
  if (e == GNUTLS_E_SUCCESS)
    return string ("");
  if (e == -1024)
    return string ("unexpected inactive GnuTLS session");
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
  string info("- Session:");
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
	case GNUTLS_CRD_SRP: {
    const char* srp_username = gnutls_srp_server_get_username (session);
		info << "- SRP session";
    if (srp_username) {
      info << " with user name " << srp_username;
    } 
		break;
  }

	case GNUTLS_CRD_PSK:
		/* This returns NULL in server side.
		 */
		if (gnutls_psk_client_get_hint (session) != NULL)
			info << "- PSK authentication. PSK hint '"
        << gnutls_psk_client_get_hint (session) << "'\n";
		/* This returns NULL in client side.
		 */
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
	default:
    info << "- Wrong credential type" << as_string (cred) << "\n";
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
  public:
  tls_server_contact_rep (array<array<string> > auths=
      array<array<string> > ());
  ~tls_server_contact_rep ();
  void start (int io);
  void stop ();
  int send (const void* buffer, size_t length);
  int receive (void* buffer, size_t length);
  bool active ();
  string last_error ();
};

tls_server_contact_rep::tls_server_contact_rep (array<array<string> > auths):
  tm_contact_rep (auths), io (-1), ptr (NULL),
  error_number (GNUTLS_E_SUCCESS)
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
  error_number= GNUTLS_E_SUCCESS;
  if (N(args) == 0) {
    server_log_write (log_warning,
        string ("Missing credentials for starting TLS session ")
        * string ("for client ") * as_string (io2));
    io= -1;
    return;
  }
  io= io2;
  server_log_write (log_info,
      string ("GnuTLS session starting for client ")
      * as_string (io));

  gnutls_session_t s;
  int e= gnutls_init (&s, GNUTLS_SERVER);
  if (e != GNUTLS_E_SUCCESS && e < 0) {
    server_log_write (log_error, "cannot initialize GnuTLS server: "
        * as_string (gnutls_strerror (e)));
    io= -1;
    return;
  }

  ptr= (pointer) s;

  string priority= "NORMAL";
  for (int i= 0; i < N(args); i++) {
    if (N(args[i]) == 0) continue;
    else if (args[i][0] == string ("anonymous")) {
      gnutls_credentials_set (s, GNUTLS_CRD_ANON,
          tm_anon_server_credentials);
      priority = priority * ":+ANON-DH";
    }
    else
      server_log_write (log_warning,
          "Unknown GnuTLS credential type " *
          as_string (args[i]));
  }
  // do not request any certificate from the client.
  gnutls_certificate_server_set_request (s, GNUTLS_CERT_IGNORE);
  gnutls_credentials_set (s, GNUTLS_CRD_CERTIFICATE,
        tm_x509_server_credentials);
  c_string _priority (priority);
  e= gnutls_priority_set_direct (s, _priority, NULL);
  if (e != GNUTLS_E_SUCCESS && e < 0) {
    server_log_write (log_error, "cannot set priority '" * priority * "': "
        * as_string (gnutls_strerror (e)));
    gnutls_deinit (s);
    io= -1;
    ptr= (pointer) NULL;
    return;
  }
  gnutls_dh_set_prime_bits (s, tm_dh_bitsize);
  gnutls_transport_set_int2 (s, io, io);
  string _timeout= get_preference ("server contact timeout");
  int timeout= is_int (_timeout) ? as_int (_timeout) : 0;
  if (timeout == 0)
    io_warning << "server connection timeout is disabled" << LF; 
  gnutls_handshake_set_timeout (s, timeout);

  e= gnutls_handshake (s);
  server_log_write (log_info, "GnuTLS handshake with client " *
      as_string (io) * string (" returned (")
      * as_string (e) * string (") '") *
      gnutls_strerror (e) * "'");
  if (e != GNUTLS_E_SUCCESS && e < 0) {
    server_log_write (log_info, "GnuTLS session failed for client " *
        as_string (io));
    server_log_write (log_error, "\n" * tm_session_info (s));
    gnutls_deinit (s);
    io= -1;
    ptr= (pointer) NULL;
    return;
  }
  server_log_write (log_info, "GnuTLS session started for client " *
      as_string (io));
  server_log_write (log_info, "\n" * tm_session_info (s));

  gnutls_credentials_type_t cred_type= gnutls_auth_server_get_type (s);
  if (cred_type == GNUTLS_CRD_ANON)
    server_log_write (log_info, "client " * as_string (io) *
        " authenticated anonymously");
}

void
tls_server_contact_rep::stop () {
  if (!active ()) return;
  gnutls_credentials_type_t cred_type=
    gnutls_auth_server_get_type ((gnutls_session_t) ptr);
  if (cred_type != GNUTLS_CRD_ANON)
    server_log_write (log_warning, "unknown credential type for client "
        * as_string (io));

  gnutls_bye ((gnutls_session_t) ptr, GNUTLS_SHUT_WR);
  gnutls_deinit ((gnutls_session_t) ptr);
  io= -1;
  ptr= (pointer) NULL;
  server_log_write (log_info, "GnuTLS closed session for client " *
      as_string (io));
}

int
tls_server_contact_rep::send (const void* buffer, size_t length) {
  if (!active ()) {
    server_log_write (log_error,
        "unexpected inactive GnuTLS session while sending"
        * string (" to client ") * as_string (io));
    return -1024;
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
    server_log_write (log_error,
        "unexpected inactive GnuTLS session while receiving"
        * string (" from client ") * as_string (io));
    return -1024;
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
  return as_string_gnutls_error (error_number);
}

bool
tls_server_contact_rep::active () {
  // Cannot be active if GnuTLS is not present
  return ptr != (pointer) NULL;
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
  tls_client_contact_rep (array<array<string> > args=
      array<array<string> > ());
  ~tls_client_contact_rep ();
  void start (int io);
  void stop ();
  int send (const void* buffer, size_t length);
  int receive (void* buffer, size_t length);
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
};

tls_client_contact_rep::tls_client_contact_rep (array<array<string> > creds):
  tm_contact_rep (creds), io (-1), ptr (NULL),
  error_number (GNUTLS_E_SUCCESS)
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
  GNUTLS_LOG("GnuTLS handshake for client " * as_string (io)
      * " returned '" * gnutls_strerror (ret));
  if (ret == GNUTLS_E_SUCCESS) {
    GNUTLS_LOG("GnuTLS session started for client " * as_string  (io));
    GNUTLS_LOG(tm_session_info (s));
    return 0;
  }
  else if (ret == GNUTLS_E_CERTIFICATE_ERROR && server_certificate != NULL) {
    // ask user for server cert confirmation if it isn't a trusted cert
    gnutls_datum_t info_short;
    string msg;

    string crt_str = as_string_gnutls_crt (server_certificate);
    if (crt_str == "") {
      char serial[128] = {0};
      size_t serial_size = sizeof (serial);

      int ret = gnutls_x509_crt_get_serial (server_certificate, serial, &serial_size);
      if (ret < 0) {
        io_warning << "cannot get server cert " << gnutls_strerror (ret);
        gnutls_deinit (s);
        io= -1;
        ptr= (pointer) NULL;
        return 1;
      }
      msg = string (serial);
    } else {
      msg = crt_str;
    }
    GNUTLS_LOG("Untrusted certificate:" * as_string (io));
    GNUTLS_LOG(msg);

#ifdef QTTEXMACS
    QMessageBox msgBox;
    c_string _msg("The server certificate issuer is unknown\n\n" * msg);
    QString q_msg(_msg);
    gnutls_datum_t info_full;
    int ret_full = gnutls_x509_crt_print(server_certificate,
        GNUTLS_CRT_PRINT_FULL,
        &info_full);

    msgBox.setText(q_msg);
    if (ret_full == 0) {
      c_string detailed(as_string_gnutls_datum (info_full));
      msgBox.setDetailedText((char *)detailed);
    }
    msgBox.setInformativeText("Do you still want to trust it ?");
    msgBox.setStandardButtons(QMessageBox::Yes | QMessageBox::No
        | QMessageBox::Cancel);
    msgBox.setDefaultButton(QMessageBox::Save);
    int ret = msgBox.exec();
    switch (ret) {
      case QMessageBox::Yes:
        ret = tm_trust_cert();
        if (ret < 0) {
          io_warning << "cannot get server cert " << gnutls_strerror (ret);
          break;
        }
        // retry handshake
        return 2;
      default:
        break;
    }
#else
    eval ("(trust-certificate-interactive \"" *
        scm_quote (as_string_gnutls_datum (info)) * "\")");
#endif
    if (ret_full) {
      gnutls_free(info_full.data);
    }
    gnutls_free(info_short.data);
  }
  return 1;
}

void
tls_client_contact_rep::start (int io2) {
  ASSERT (!active (), "contact already active");
  GNUTLS_LOG("tls_client_contact_rep::start");
  error_number= GNUTLS_E_SUCCESS;
  if (!tm_gnutls_initialized) return;
  if (N(args) == 0) {
    GNUTLS_LOG("Missing credentials for starting TLS session for client "
        * as_string (io));
    io= -1;
    return;
  }
  io= io2;
  GNUTLS_LOG("GnuTLS session starting for client " * as_string (io) * "...");
  gnutls_session_t s;
  gnutls_init (&s, GNUTLS_CLIENT);
  ptr= (pointer) s;
  string priority = "NORMAL";
  for (int i= 0; i < N(args); i++) {
    array<string> cred= args[i];
    if (N(cred) == 0) continue;
    if (N(cred) == 1 && cred[0] == "anonymous") {
      gnutls_credentials_set (s, GNUTLS_CRD_ANON,
          tm_anon_client_credentials);
      priority << ":+ANON-DH";
    }
    else
      io_warning << "Unknown credentials " << cred
        << " for GnuTLS session of client " << io << "\n";
  }

  gnutls_session_set_verify_function(s,
      certificate_client_verification_callback);
  gnutls_credentials_set (s, GNUTLS_CRD_CERTIFICATE,
      tm_x509_client_credentials);

  c_string _priority (priority);
  if (gnutls_priority_set_direct (s, _priority, NULL) != GNUTLS_E_SUCCESS) {
    io_warning << "GnuTLS failed setting priorities " << priority
      << " for client " << io << "\n";
    gnutls_deinit (s);
    io= -1;
    ptr= (pointer) NULL;
    return;
  }
  gnutls_transport_set_int2 (s, io, io);
  string _timeout= get_preference ("client contact timeout");
  int timeout= is_int (_timeout) ? as_int (_timeout) : 0;
  if (timeout == 0)
    io_warning << "client connection timeout is disabled" << LF;
  gnutls_handshake_set_timeout (s, timeout);

  int ret = 0;
  do {
    if (ret == 2) {
      GNUTLS_LOG("Retrying GnuTLS handshake...");
    }
    ret = handshake(s);
  } while (ret == 2);

  if (ret == 1) {
    GNUTLS_LOG("GnuTLS session aborted for client " * as_string (io));
    gnutls_deinit (s);
    io= -1;
    ptr= (pointer) NULL;
  }
}

void
tls_client_contact_rep::stop () {
  if (active ()) {
    gnutls_bye ((gnutls_session_t) ptr, GNUTLS_SHUT_RDWR);
    gnutls_deinit ((gnutls_session_t) ptr);
    ptr= (pointer) NULL;
  }
  if (io >= 0) {
    GNUTLS_LOG("GnuTLS closed session for client " * as_string (io));
    io= -1;
  }
}

int
tls_client_contact_rep::send (const void* buffer, size_t length) {
  if (!active ()) {
    io_error << "Unexpected inactive GnuTLS session while sending"
      << " to client " << io << "\n";
    return -1024;
  }
  int r= gnutls_record_send ((gnutls_session_t) ptr, buffer, length);
  error_number= r < 0 ? r : GNUTLS_E_SUCCESS;
  if (r < 0 &&
      r != GNUTLS_E_SUCCESS &&
      r != GNUTLS_E_AGAIN &&
      r != GNUTLS_E_INTERRUPTED ) stop ();
  return r;
}

int
tls_client_contact_rep::receive (void* buffer, size_t length) {
  if (!active ()) {
    io_error << "Unexpected inactive GnuTLS session while receiving"
      << " from client " << io << "\n";
    return -1024;
  }
  int r= gnutls_record_recv ((gnutls_session_t) ptr, buffer, length);
  error_number= r < 0 ? r : GNUTLS_E_SUCCESS;
  if (r < 0 &&
      r != GNUTLS_E_SUCCESS &&
      r != GNUTLS_E_AGAIN &&
      r != GNUTLS_E_INTERRUPTED ) stop ();
  return r;
}

string
tls_client_contact_rep::last_error () {
  return as_string_gnutls_error (error_number);
}

bool
tls_client_contact_rep::active () {
  // Cannot be active if GnuTLS is not present
  return ptr != (pointer) NULL;
}

tm_contact 
make_tls_client_contact (array<array<string> > args) {
  return (tm_contact_rep*) tm_new<tls_client_contact_rep> (args);
}

/******************************************************************************
* PBKDF2 password hash
******************************************************************************/

string hash_password_pbkdf2 (string passwd, string salt) {
	c_string _passwd(passwd), _salt(salt);
	gnutls_datum_t key_data = {
      .data = (unsigned char*)((char *)_passwd),
      .size = static_cast<unsigned int>(N (passwd)),
    };
    gnutls_datum_t salt_data = {
      .data = (unsigned char *)((char *)_salt),
      .size = static_cast<unsigned int>(N (salt)),
    };
    uint8_t output_raw[4096] = {0};
    gnutls_datum_t out_data, out_b64, salt_b64;

    int ret = gnutls_pbkdf2(GNUTLS_MAC_SHA256 , &key_data, &salt_data,
        MAC_SHA256_ITER_COUNT, output_raw, MAC_SHA256_BYTE_SIZE);
    if (ret < 0) {
      return "";
    }

    out_data.data = output_raw;
    out_data.size = MAC_SHA256_BYTE_SIZE;

    if (GNUTLS_E_SUCCESS != gnutls_base64_encode2(&out_data, &out_b64)) {
      return "";
    }

    if (GNUTLS_E_SUCCESS != gnutls_base64_encode2(&salt_data, &salt_b64)) {
      return "";
    }

    return string("$7$") * as_string (MAC_SHA256_ITER_COUNT)
      * "$" * as_string (reinterpret_cast<const char *>(salt_b64.data))
      * "$" * as_string (reinterpret_cast<const char *>(out_b64.data));
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

#endif // USE_GNUTLS
