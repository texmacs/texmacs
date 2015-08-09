#!/bin/bash -O extglob -O nocasematch -O nocaseglob
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
{ # $1 file name $2 new rpath
	local args rpath value cmdout
	cmdout="$(otool -lX $1)" || return 42
	while read rpath value
	do
		[[ $rpath == path ]] && args="$args -delete_rpath $value"
	done <<< "$cmdout"
	[ -n "$2" ] &&  args="$args -add_rpath $2"
	[ -z "$args" ] && return 0
	install_name_tool $args $1 || return 43
}
	
function bundle_all_libs
{
#	$1   executable  or library path (relative to Contents directory)
  local rpath="@executable_path/../Resources/lib"
  local libdest="Resources/lib"

  echo "Bundling all libraries for [$1]"
  bundle_all_libs_sub "$1" || return $?
  set_rpath "$1" $rpath || return $?
}
  
function bundle_all_libs_sub
{
# $1 is library to process with path relative to Contents directory

	local lib change cmdout libname
	[[ $(otool -DX "$1") == @executable_path/../$1 ]] && return 0
	echo "Process $1"
  install_name_tool -id "@executable_path/../$1" "$1" || return 31
	cmdout="$(otool -LX "$1")" || return 41
	# Add local Libs and Force bundling of (system) libltdl (changed in OSX 10.8)
  while read -r lib version
  do
  	case $lib in
  	@executable_path/../$1) ;;
  	/System*) ;;
  	/+(opt/local|sw|Users|usr/local)/*/lib*.dylib|/usr/lib/libltdl.*.dylib)
    local blib="$(basename $lib)"
   	[ -f "$libdest/$blib" ] || cp "$lib" "$libdest" && chmod u+w "$libdest/$blib" || return 11
    bundle_all_libs_sub "$libdest/$blib" || return $?
    change="$change -change $lib  @rpath/$blib"
    ;; 
    /*/*.framework/*)
    local fwname="${lib%%.framework/*}.framework"; fwname="${fwname##*/}"
    local blib=$(basename $lib)
    local fwbase="Frameworks/$fwname"
    [ -d "$fwbase" ] || mkdir "$fwbase" || return 12
    [ -f "$fwbase/$blib" ] || cp "$lib" "$fwbase" || return 12
    bundle_all_libs_sub "$fwbase/$blib" || return $?
    change="$change -change $lib  @executable_path/../$fwbase/$blib"
		;;
		esac
	done <<< "$cmdout"
  set_rpath "$1" || return $?
  [ -z "$change" ] && return 0
  install_name_tool $change "$1" || return 33
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
  cp -R $oplug Plugins/. || return 51
  for p in $dplug
  do bundle_all_libs $p || return $?
  done
}

###############################################################################

cd "$(dirname $1)/.." || exit 10
bundle_all_libs "MacOS/$(basename $1)"  || exit $?
[ -z "$QT_PLUGINS_LIST" ] && exit 0
bundle_qt_plugins "$QT_PLUGINS_LIST" "$QT_PLUGINS_PATH"
