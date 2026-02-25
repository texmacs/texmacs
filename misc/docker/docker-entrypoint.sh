#! /bin/bash -x
env

# if first arg looks like a flag, assume we want to run texmacs server
if [ "${1:0:1}" = '-' ]; then
  set -- texmacs "$@"
fi

TM_HOME=/srv/texmacs-server/.TeXmacs

if [[ "$1" == texmacs* ]] && [ "$(id -u)" = "0" ]; then
  mkdir -p "$TM_HOME"
  chown -R texmacs-server:texmacs-server "$TM_HOME"

  # The server refuses to start without a certificate, and the image ships none
  # (it would be the same one for every installation). Generate a self-signed
  # pair on first run. This has to happen after the chown above, otherwise
  # TeXmacs cannot even create its own $TEXMACS_HOME_PATH skeleton.
  if [ ! -s "$TM_HOME/server/cert.pem" ] || [ ! -s "$TM_HOME/server/key.pem" ]; then
    echo "Generating self-signed certificate"
    gosu texmacs-server texmacs --headless \
      -x '(generate-self-signed-certificate (list (list "cn" "localhost"))
            (string->url "$TEXMACS_SERVER_CERT_DIR/cert.pem")
            (string->url "$TEXMACS_SERVER_CERT_DIR/key.pem"))' \
      -q
    chmod 600 "$TM_HOME/server/key.pem"
  fi

  echo "Switching to dedicated user 'texmacs-server'"
  exec gosu texmacs-server "$BASH_SOURCE" "$@"
fi

# if launching texmacs, force server mode
if [ "$1" = 'texmacs' ]; then
  shift # "texmacs"
  set -- texmacs --server --headless "$@"
fi

exec "$@"
