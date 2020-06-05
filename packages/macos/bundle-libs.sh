#!/bin/bash -O extglob -O nocasematch -O nocaseglob -O nullglob
#
# denis RAUX  CNRS/LIX 2015-2021
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
# - Framework is identified by name.framework in the pathname
# - Qt expects plugins in Whatever.app/Contents/plugins
# - id setting is silently ignored for non dll file
#

typeset absLibPath #if there is only relative path

function bundle_all_libs {
# $1   executable  or library path (relative to Contents directory)
  local libdest="Resources/lib"

  echo "Bundling all libraries for [$1]"
  bundle_lib "$1"
}
  


function bundle_qt_plugins { 
# $2 Qt plugin path $1 subdir list
# Plugins is the directory where we store them
  [ -z $1 ] && return 0
  local oplug dplug
  if [[ $1 =~ , ]]
  then oplug="$(eval echo $2/{$1})"
  else oplug="$2/$1"
  fi
  for d in $oplug
  do test -d $d && mkdir Plugins/$(basename $d) && \
    test -n "$(echo $d/*dylib)" && cp $d/*dylib Plugins/$(basename $d)/
  done
  
  for p in $(eval echo Plugins/*/*dylib)
  do bundle_all_libs $p || return $?
  done
}

function bundle_lib {
# $1 lib to pack
# for rpath location : 
#  return 11 if framework not found
#  return 12 if lib not found

  local file=$1
  local -i state=0 step=0 setrpath=0
  local -a tlibs trpath
  local lib change

  while read -r cmd arg
  do
    arg=${arg% (offset*)}
    case $cmd in
    Load) set $arg
          test $1 == command -a $2 -ne $step && exit 11
          step+=1
          state=1;;
    cmd)  test $state -ne 1 && continue
          case  $arg in
          LC_LOAD_DYLIB) state=2;;
          LC_RPATH) state=3;;
          *) state=0
          esac;;
    name) test $state -ne 2 && continue
          tlibs+=("$arg")
          state=0;;
    path) test $state -ne 3 && continue
          trpath+=("$arg")
          [[ $arg =~ ^/ ]] && absLibPath="$arg"
          state=0;;
    esac 
  done <<< "$(otool -lX $1)"

  test ${#tlibs[*]} -eq 0 && exit 12
  
  for lib in "${tlibs[@]}"
  do 
    case $lib in
    /System*) ;;
    /Library*) ;;
    /+(opt/local|sw|Users|usr/local)/*/lib*.dylib|/usr/lib/libltdl.*.dylib)
    local blib="$(basename $lib)"
    if ! test -f "$libdest/$blib"
    then 
      cp "$lib" "$libdest" && chmod u+w "$libdest/$blib" || return 11
      bundle_lib "$libdest/$blib" || return $?
      change="$change -change $lib  @executable_path/../Resources/lib/$blib"
    fi
    ;; 
    @rpath/*.framework/*)
    # we don't scan the framework lib they might be well built
    # be carrefull with space in file names
    for p in "${trpath[@]}" $absLibPath
    do fullname=${lib/@rpath/$p}
      if test -f "$fullname"
      then 
        local frwkroot="${fullname%.framework/*}.framework" frwkname="${fullname##*/}"
        if test ! -d "Frameworks/$frwkname"
        then rsync -az "$frwkroot" Frameworks/
             bundle_lib "Frameworks/$frwkname${fullname#*$frwkname}" || return $?
             setrpath=$(($setrpath|1))
        fi
        continue 2
      fi
    done
    echo Framework $lib not found >&2
    return 11
    ;;
    @rpath/*) #some extra libs
    for p in "${rpath[@]}"
    do fullname=${lib/@rpath\//$p}
      if test -f "$fullname"
      then 
        local blib=$(basename $lib)
        if test ! -f "$libdest/$blib"
        then 
          cp "$fullname" "$libdest" 
          bundle_lib "$libdest/$blib" || return $?
          setrpath=$(($setrpath|2))
        fi
        continue 2
      fi
    done
    echo Library $lib not found >&2
    return 12
    ;;
    esac
  done
  #fix the rpath
  for r in "${trpath[@]}"
  do case "$r" in 
      @executable_path/../Resources/lib) setrpath=$((($setrpath|2)^2));;
      @executable_path/../Frameworks) setrpath=$((($setrpath|1)^1));;
      @loader_path/*) ;;
      *) # install_name_tool doesn't support duplicate commands
         # may be some duplicates in the files
         install_name_tool -delete_rpath $r "$file"
      
    esac
  done
  # adjust rpath in the library
  test $(($setrpath&1)) -ne 0 && 
    change+=" -add_rpath @executable_path/../Frameworks"
  test $(($setrpath&2)) -ne 0 && 
    change+=" -add_rpath @executable_path/../Resources/lib"
  [ -z "$change" ] && return 0
  eval install_name_tool $change "$file" || return 33
  return 0
}

###############################################################################


test -f "$1" || exit 1
cd "$(dirname $1)/.." || exit 10
bundle_all_libs "MacOS/$(basename $1)"  || exit $?
[ -z "$QT_PLUGINS_LIST" ] && exit 0
bundle_qt_plugins "$QT_PLUGINS_LIST" "$QT_PLUGINS_PATH"
