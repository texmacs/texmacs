#!/bin/bash
###############################################################################
# bundle-libs.sh
#
# Copies frameworks into the application bundle and rewrites the loading
# in the .dylib files. (Qt and Sparkle as of Aug. 2013)
#
# Some Qt plugins are copied as well, but we need some way to narrow down the
# copy to the strictly necessary (TODO)
#
# This script relies on the variables QT_FRAMEWORKS_PATH, QT_PLUGINS_PATH and,
# eventually, SPARKLE_FRAMEWORK_PATH. (See Makefile.in), but we try some
# default values too.
#
# NOTE that all paths are relative to BASEDIR=TeXmacs.app/Contents/MacOS
###############################################################################

BASEDIR=$(dirname $1)
EXECUTABLE=$(basename $1)
BUNDLE_RESOURCES=../Resources
BUNDLE_FRAMEWORKS=../Frameworks
BUNDLE_PLUGINS=../plugins # Qt expects plugins in Whatever.app/Contents/plugins

###############################################################################

function bundle_install_lib
{
  local target=$1
  local libpath=$2
  local libdest="$BUNDLE_RESOURCES/lib/$(basename $2)"
  local newidname="@executable_path/$libdest"

  echo "Bundling [$libpath] in [$libdest] for [$target]"

  [ ! -f "$libdest" ] && cp "$libpath" "$BUNDLE_RESOURCES/lib";

  install_name_tool -id "$newidname" "$libdest"
  install_name_tool -change "$libpath" "$newidname" "$target"
  bundle_all_libs "$libdest"
}

function bundle_all_libs
{
  local target=$1
  echo "Bundling all libraries for [$target]"
  for lib in $( otool -L "$target"  | \
                grep -o '/\(opt\|sw\|Users\|usr/local\)/.*/lib[^/]*dylib' )
  do 
    bundle_install_lib "$target" "$lib"
  done

  # Force bundling of (system) libltdl (changed in OSX 10.8)
  for lib in $( otool -L $target | grep -o '/usr/lib/libltdl[^/]*dylib' ); do 
    bundle_install_lib "$target" "$lib"
  done
}

function bundle_install_plugin
{
  local target=$1
  local pluginpath=$2
  local pluginname=$(basename $2)
  local newidname="@executable_path/$pluginpath"

  echo "Bundling plugin [$pluginname] for [$target]"

  install_name_tool -id "$newidname" "$pluginpath"
  install_name_tool -change "$pluginpath" "$newidname" "$target"

  bundle_all_libs "$pluginpath"
  bundle_qt_frameworks "$pluginpath"
}

function bundle_qt_plugins
{
  local target=$1
  local subs=$2

  for group in $subs; do
    echo "Bundling Qt plugins in group [$group] for [$target]"
    mkdir -p "$BUNDLE_PLUGINS/$group"
    if [ -r "$QT_PLUGINS_PATH/$group" ]; then
      find "$QT_PLUGINS_PATH/$group/" -type f -not -iname '*_debug.dylib' \
           -exec cp \{\} "$BUNDLE_PLUGINS/$group" \;
      for lib in $(find "$BUNDLE_PLUGINS/$group" -name \*.dylib -print); do
        bundle_install_plugin "$1" "$lib"
      done
    fi
  done
}

function bundle_framework
{
  local target=$1
  local fpath=$2
  local flinked=$3

  local ffullname=${fpath##*/}
  local fbasename=${ffullname%.framework}
  local tmp=${flinked#*Versions/}
  local fversion=${tmp%/*}
  local dest="$ffullname/Versions/$fversion/$fbasename"
  local newid="@executable_path/../Frameworks/$dest"

  if [ ! "$fbasename" == "${target##*/}" ]; then
    echo "Bundling framework [$fpath] to [$BUNDLE_FRAMEWORKS/$ffullname] for [$target]"
    if [ ! -r "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion" ]; then
      mkdir -p "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion"
      ln -s "$fversion" "$BUNDLE_FRAMEWORKS/$ffullname/Versions/Current"
      #lipo -thin $ARCH $fpath/Versions/$fversion/$fbasename 
      #     -output $BUNDLE_FRAMEWORKS/$fname/Versions/$fversion/$fbasename
      cp "$fpath/Versions/$fversion/$fbasename" "$BUNDLE_FRAMEWORKS/$dest"
      if [ -r "$fpath/Versions/$fversion/Resources" ]; then
        cp -R "$fpath/Versions/$fversion/Resources" \
              "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion/Resources"
        ln -s "Versions/$fversion/Resources" \
              "$BUNDLE_FRAMEWORKS/$ffullname/Resources"
      fi
      bundle_qt_frameworks "$BUNDLE_FRAMEWORKS/$dest"
    fi
    install_name_tool -id "$newid" "$BUNDLE_FRAMEWORKS/$dest"
    install_name_tool -change "$flinked" "$newid" "$target"
  fi
}

function bundle_qt_frameworks
{
  local target=$1

  echo "Bundling Qt frameworks for [$target]"
  for f in $(otool -L "$1"  | grep -o '\(.*Qt.*\.framework/Versions/[^: ]*\)')
  do
    local fpath=${f%%/Versions/*}
    if [ ! -r "$fpath" ] && [ -r "$QT_FRAMEWORKS_PATH/$fpath" ]; then
      local fpath="$QT_FRAMEWORKS_PATH/$fpath"
    fi
    if [ -r "$fpath" ]; then
      bundle_framework "$target" "$fpath" "$f"
    fi
  done
}

function bundle_other_frameworks
{
  local target=$1
  local fbasename=$2

  for f in $(otool -L $target  | \
             grep -o ".*${fbasename}.framework/Versions/[^: ]* ") ; do
    local fpath=${f%%/Versions/*}
    local replace=@loader_path/../Frameworks
    local fpath=${fpath/$replace/$SPARKLE_FRAMEWORK_PATH}
    if [ -r "$fpath" ]; then
      bundle_framework "$target" "$fpath" "$f"
    fi
  done
}

###############################################################################

echo -n "Qt Frameworks path: "
for d in "$QT_FRAMEWORKS_PATH \
          /Library/Frameworks \
          /Developer/QtSDK/Desktop/Qt/4.8.1/gcc"; do
  if [ -r "$d" ]; then QT_FRAMEWORKS_PATH="$d"; break; fi
done

if [ ! -r "$QT_FRAMEWORKS_PATH" ]; then
  echo " WARNING: QT_FRAMEWORKS_PATH not readable. Bundling might be wrong."
else
  echo "[$QT_FRAMEWORKS_PATH]"
fi

echo -n "Qt Plugins path: "
for d in "$QT_PLUGINS_PATH \
          $QT_FRAMEWORKS_PATH/plugins \
          /Developer/Applications/Qt/plugins \
          /Developer/QtSDK/Desktop/Qt/4.8.1/gcc/plugins"; do
 if [ -r "$d" ]; then QT_PLUGINS_PATH="$d"; break; fi
done

if [ ! -r "$QT_PLUGINS_PATH" ]; then
  echo "WARNING: QT_PLUGINS_PATH not readable. Bundling might be wrong."
else
  echo "[$QT_PLUGINS_PATH]"
fi

cd "$BASEDIR"
mkdir -p "$BUNDLE_FRAMEWORKS"

bundle_all_libs "$EXECUTABLE"
bundle_qt_frameworks "$EXECUTABLE"
bundle_qt_plugins "$EXECUTABLE" "imageformats accessible"
bundle_other_frameworks "$EXECUTABLE" "Sparkle"

