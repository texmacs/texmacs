#!/bin/bash

SCRIPTNAME="$0"
SCRIPTDIR=$(dirname $(readlink -f $SCRIPTNAME))

# Function to display usage information
usage() {
    echo "Usage: $0 [options]"
    echo
    echo "       This script will build check and upload TeXmacs artifacts to a remote with rsync."
    echo "       Artifacts should be the result of a docker build, or a CI build."
    echo
    echo "Options:"
    echo "  -d, --dir DIR           Specify the directory to use (default: current directory)"
    echo "  -a, --artifacts-dir DIR Specify the artifacts directory to use (default: current directory)"
    echo "  -r, --revision REV      Specify the revision to use (default: git/svn revision)"
    echo "  -b, --use-docker        Use docker to produce artifacts (default false)"
    echo "  --no-build              Skip building, will use artifacts directory to upload (default: false)"
    echo "  --no-upload             Skip upload (default: false)"
    echo "  -h, --help              Display this help message"
    exit 1
}

VERBOSE=1
QUIET=0

### Color Codes
RESET=$(tput sgr0)
RED=$(tput setaf 1)
YELLOW=$(tput setaf 3)
CYAN=$(tput setaf 6)
GRAY=$(tput setaf 7)

# RED="\033[0;31m"
# YELLOW="\033[0;33m"
# CYAN="\033[0;36m"
# GRAY="\033[2;37m"

### Logging Functions
log() {
    local LEVEL="$1"; shift
    local STRING="$(echo -e "$*" | sed -e "s/[[:space:]]\+/ /g")"
    local ANSI

    case "$LEVEL" in
        DEBUG) ANSI="$GRAY";;
        INFO)  ANSI="$CYAN";;
        WARN)  ANSI="$YELLOW";;
        ERROR) ANSI="$RED";;
        FATAL) ANSI="$RED";;
        *)     ANSI="";;
    esac

    TS="[$(date +"%Y-%m-%d %H:%M")]"

    printf "%-7s %s %s\n" "[${ANSI}${LEVEL}${RESET}]" "$TS" "$STRING" >&2
}

debug() { [[ "$QUIET" != "1" && "$VERBOSE" == "1" ]] && log DEBUG "$*"; }
info() { [[ "$QUIET" == "1" && "$VERBOSE" != "1"  ]] || log INFO "$*"; }
warn() { log WARN "$*"; }
error() { log ERROR "$*"; }

fatal() {
    local STATUS="$?" MSG="$*"
    log FATAL "$MSG"
    exit "$STATUS"
}

TOPDIR=$(pwd)
TEXMACS_PATH="$TOPDIR/TeXmacs"
GUILE_SRC="$TOPDIR/../guile-texmacs"
BUILD=true
UPLOAD=true
USE_DOCKER=false
ARTIFACTS_DIR=$TOPDIR
REMOTE_USER="texmacs-server"
REMOTE_HOST="cloud.texmacs.org"

PARSED_OPTIONS=$(getopt -o a:d:br:h --long dir:,artifacts-dir:,guile-src:,use-docker,revision:,remote-user:,remote-host:,no-build,no-upload,help -- "$@")
if [ $? -ne 0 ]; then
    usage
fi

eval set -- "$PARSED_OPTIONS"

while true; do
    case "$1" in
        -d|--dir)
            TOPDIR="$2"
            shift 2
            ;;
        -a|--artifacts-dir)
            ARTIFACTS_DIR="$2"
            shift 2
            ;;
        -b|--use-docker)
            USE_DOCKER=true
            shift
            ;;
        --no-build)
            BUILD=false
            shift
            ;;
        --no-upload)
            UPLOAD=false
            shift
            ;;
        --remote-user)
            REMOTE_USER="$2"
            shift 2
            ;;
        --remote-host)
            REMOTE_HOST="$2"
            shift 2
            ;;
        --guile-src)
            GUILE_SRC="$2"
            shift 2
            ;;
        -r|--revision)
            REVISION="$2"
            shift 2
            ;;
        -h|--help)
            usage
            ;;
        --)
            shift
            break
            ;;
        *)
            usage
            ;;
    esac
done

[ -d $GUILE_SRC ] || fatal "guile sources not found in $GUILE_SRC"

info "Using directory: $TOPDIR"
info "Using Guile sources in: $GUILE_SRC"
if [ "$USE_DOCKER" = "true" ]; then
  info "Using docker to build artifacts"
  [ ! -L "$GUILE_SRC" ] || fatal "When using docker, guile source directory should not be a symbolic link"
fi
info "Storing artifacts in: $ARTIFACTS_DIR"


ICE9_PATH="$GUILE_SRC/ice-9"
# EXCLUDE_FILE="exclude.txt"

get_local_revision() {
    if [ -n "$REVISION" ]; then
      echo -n "$REVISION"
      return
    fi

    local revision=$(git rev-parse --short=8 HEAD 2>/dev/null)
    if [ $? -eq 0 ]; then
        echo -n "$revision"
        return
    fi

    revision=$(git svn info | grep 'Revision' | awk '{print $2}' 2>/dev/null)
    if [ $? -eq 0 ]; then
        echo -n "$revision"
        return
    fi

    revision=$(svn info | grep 'Revision' | awk '{print $2}' 2>/dev/null)
    if [ $? -eq 0 ]; then
        echo -n "$revision"
        return
    fi

    fatal "No version control system detected."
}

check_dir_or_fail() {
  [ -d "$1" ] || fatal "Local directory $1 does not exist."
}

check_file_or_fail() {
  [ -f "$1" ] || fatal "Local file $1 does not exist."
}

check_build_output() {
  OUTDIR="$1"
  check_dir_or_fail "$OUTDIR"
  check_dir_or_fail "$OUTDIR/bin"
  check_file_or_fail "$OUTDIR/bin/texmacs.bin"
  check_dir_or_fail "$OUTDIR/progs"
  check_dir_or_fail "$OUTDIR/progs/ice-9"
  check_dir_or_fail "$OUTDIR/fonts"
  check_dir_or_fail "$OUTDIR/plugins"
  check_dir_or_fail "$OUTDIR/styles"
  check_dir_or_fail "$OUTDIR/texts"
  check_dir_or_fail "$OUTDIR/langs"
  check_dir_or_fail "$OUTDIR/packages"
  check_file_or_fail "$OUTDIR/LICENSE"
}

REMOTE_DIR="releases/TeXmacs-upload-$(get_local_revision)"

RSYNC_OPTIONS="-azp --delete --partial --info=progress2 --mkpath"

if [ -f "$EXCLUDE_FILE" ]; then
    RSYNC_OPTIONS+=" --exclude-from='$EXCLUDE_FILE'"
fi

if [[ "$BUILD" == "true" ]]; then
  BUILDTS=$(date +"%Y%m%d%H%M")
  BUILDOUT=$ARTIFACTS_DIR/build-$BUILDTS
  info "Creating artifacts to upload in $BUILDOUT"

  if [ "$USE_DOCKER" = "true" ]; then
    docker build --build-context guile-src=$GUILE_SRC \
      --target export \
      -t texmacs-build-$BUILDTS \
      --output="$BUILDOUT" .
  else
    mkdir -p $BUILDOUT
    rsync $RSYNC_OPTIONS \
      $TEXMACS_PATH/progs \
      $TEXMACS_PATH/fonts \
      $TEXMACS_PATH/plugins \
      $TEXMACS_PATH/styles \
      $TEXMACS_PATH/texts \
      $TEXMACS_PATH/langs \
      $TEXMACS_PATH/packages \
      $TEXMACS_PATH/bin \
      $TOPDIR/LICENSE \
      $BUILDOUT
    rsync $RSYNC_OPTIONS $ICE9_PATH $BUILDOUT/progs/
  fi
else
  info "Skipping build, directly using $ARTIFACTS_DIR to upload"
  BUILDOUT=$ARTIFACTS_DIR
fi


if [[ "$UPLOAD" == "true" ]]; then
  check_build_output $BUILDOUT
  info "Starting upload to $REMOTE_USER@$REMOTE_HOST"

  rsync $RSYNC_OPTIONS $BUILDOUT/ "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"
    build_success=true
  # else
  #   info "Using TeXmacs path at $TEXMACS_PATH"
  #   rsync $RSYNC_OPTIONS \
  #     $TEXMACS_PATH/progs \
  #     $TEXMACS_PATH/fonts \
  #     $TEXMACS_PATH/plugins \
  #     $TEXMACS_PATH/styles \
  #     $TEXMACS_PATH/texts \
  #     $TEXMACS_PATH/langs \
  #     $TEXMACS_PATH/packages \
  #     $TEXMACS_PATH/bin \
  #     $TOPDIR/LICENSE \
  #     "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR/"
  #   first_ret=$?
  #
  #   rsync $RSYNC_OPTIONS \
  #     $ICE9_PATH \
  #     "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR/progs/"
  #   if [[ $first_ret == 0 && $? == 0 ]]; then
  #     build_success=true
  #   fi
  # fi

  if [[ $? == 0 ]]; then
    info "Upload completed successfully."
  else
    fatal "Upload failed."
  fi
else
  info "Skipping upload."
fi
