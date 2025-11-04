#! /bin/bash -x
env

# if first arg looks like a flag, assume we want to run texmacs server
if [ "${1:0:1}" = '-' ]; then
  set -- texmacs "$@"
fi

if [[ "$1" == texmacs* ]] && [ "$(id -u)" = "0" ]; then
  echo "Switching to dedicated user 'texmacs-server'"
  exec gosu texmacs-server "$BASH_SOURCE" "$@"
fi

# if launching texmacs, force server mode
if [ "$1" = 'texmacs' ]; then
  shift # "texmacs"
  set -- texmacs --server --headless "$@"
fi

exec "$@"
