(use-modules (server server-base)
             (security password))

;; generate x509 certificates for server

(define cert-url (string->url "$TEXMACS_SERVER_CERT_DIR/cert.pem"))
(define key-url  (string->url "$TEXMACS_SERVER_CERT_DIR/key.pem"))

(when (or (not (url-exists? cert-url)) (not (url-exists? key-url)))
  (generate-self-signed-certificate `(("cn" "localhost")) cert-url key-url))

