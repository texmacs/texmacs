#!/bin/bash -O extglob -O nocasematch -O nocaseglob -O nullglob
#
# denis RAUX  CNRS/LIX 2015
#
# Copies frameworks and lib into the application bundle and rewrites the loading
# information in the .dylib files.
#
# Qt plugins list are copied as specified within configure --enable-QtPlugins
# This script relies on the variables environment variables  QT_PLUGINS_PATH 
# and QT_PLUGINS_LIST
#
# NOTE that all paths are relative to BASEDIR=TeXmacs.app/Contents/MacOS
# Some assumptions to keep in mind :
# - this script does not manage space in pathnames
# - Framework is identified by name.framework in the pathname
# - Qt expects plugins in Whatever.app/Contents/plugins
# - id setting is silently ignored for non dll file
#

function set_rpath
{ # $1 = returned value
  local args rpath value cmdout file=$1  retval=$2
  cmdout="$(otool -lX $file)" || return 42
  while read rpath value
  do
    [[ $rpath == path ]] && args+=" -delete_rpath $value"
  done <<< "$cmdout"
  eval $retval+=$args
}
  
function bundle_all_libs
{
# $1   executable  or library path (relative to Contents directory)
  local libdest="Resources/lib"

  echo "Bundling all libraries for [$1]"
  bundle_all_libs_sub "$1"
}
  
function bundle_all_libs_sub
{
# $file is library to process with path relative to Contents directory

  local lib change cmdout libname file="$1" rpath="$2" d
  [[ $(otool -DX "$file") == @executable_path/../$file ]] && return 0
  echo "Process $file"
    chmod +w "$file"  # Needed e.g. with homebrew (libraries are 622)
  install_name_tool -id "@executable_path/../$file" "$file" || return 31
  cmdout="$(otool -LX "$file")" || return 41
  # Add local Libs and Force bundling of (system) libltdl (changed in OSX 10.8)
  while read -r lib version
  do
    case $lib in
    @executable_path/../$file) ;;
    *:) ;;
    /System*) ;;
    /+(opt/local|sw|Users|usr/local)/*/lib*.dylib|/usr/lib/libltdl.*.dylib)
    local blib="$(basename $lib)"
    [ -f "$libdest/$blib" ] || cp "$lib" "$libdest" && chmod u+w "$libdest/$blib" || return 11
    bundle_all_libs_sub "$libdest/$blib" || return $?
    change="$change -change $lib  @executable_path/../Resources/lib/$blib"
    ;; 
    *.framework/*)
    local fwloc="${lib%%.framework/*}.framework"; 
    if [[ ! "$fwloc" =~ ^/.* ]]; then 
      if [[ -f /Library/Frameworks/$lib ]]
      then fwloc="/Library/Frameworks/$fwloc"
      else  
        if [[ -f "$QT_FRAMEWORKS_PATH/$lib" ]]
        then fwloc="$QT_FRAMEWORKS_PATH/$fwloc"
        else return 32
        fi
      fi
    fi

    local fwname="${fwloc##*/}"
    local blib=$(basename $lib)
    local fwbase="Frameworks/$fwname"
    [ -d "$fwbase" ] || mkdir "$fwbase" || return 12
    for d in Resources Contents
    do [ -d "$fwloc/$d" -a ! -d "$fwbase/$d" ] && { cp -RL "$fwloc/$d" "$fwbase" || return 13; }
    done
    [ -f "$fwbase/$blib" ] || cp "$fwloc/${lib#*.framework}" "$fwbase" || return 14
    bundle_all_libs_sub "$fwbase/$blib" || return $?
    change="$change -change $lib  @executable_path/../$fwbase/$blib"
    ;;
    esac
  done <<< "$cmdout"
  set_rpath "$file" change || return $?
  [ "$rpath" ] && change+=" -add_rpath $rpath"
  [ -z "$change" ] && return 0
  install_name_tool $change "$file" || return 33
  return 0
}


function bundle_qt_plugins
{ 
# $2 Qt plugin path $1 subdir list
# Plugins is the directory where we store them
  [ -z $1 ] && return 0
  local oplug dplug
  if [[ $1 =~ , ]]
  then 
    oplug="$(eval echo $2/{$1})"
    dplug="$(eval echo Plugins/{$1}/*dylib)"
  else
    oplug="$2/$1"
    dplug="Plugins/$1/*dylib"
  fi
  for d in $oplug
  do test -d $d && mkdir Plugins/$(basename $d) && \
    test -n "$(echo $d/*dylib)" && cp $d/*dylib Plugins/$(basename $d)/
  done
  for p in $dplug
  do bundle_all_libs $p || return $?
  done
}

###############################################################################

cd "$(dirname $1)/.." || exit 10
bundle_all_libs "MacOS/$(basename $1)"  || exit $?
[ -z "$QT_PLUGINS_LIST" ] && exit 0
bundle_qt_plugins "$QT_PLUGINS_LIST" "$QT_PLUGINS_PATH"
