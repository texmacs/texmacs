#!/bin/bash 

BASEDIR=$(dirname $1)
EXECUTABLE=$(basename $1)
BUNDLE_RESOURCES=../Resources
BUNDLE_FRAMEWORKS=../Frameworks
BUNDLE_PLUGINS=../Plugins

if [ ! "$QT_FRAMEWORKS_PATH" ]; then
  QT_FRAMEWORKS_PATH=/Library/Frameworks
fi

if [ ! "$QT_PLUGINS_PATH" ]; then
  if [ -e "$QT_FRAMEWORKS_PATH/plugins" ]; then 
    QT_PLUGINS_PATH=$QT_FRAMEWORKS_PATH/plugins
  else 
    QT_PLUGINS_PATH=/Developer/Applications/Qt/plugins
  fi
fi

echo "Qt Frameworks path: [$QT_FRAMEWORKS_PATH]"
echo "Qt Plugins path: [$QT_PLUGINS_PATH]"

function bundle_install_lib {
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

function bundle_all_libs {
  local target=$1
  echo "Bundling all libraries for [$target]"
  for lib in $( otool -L "$target"  | grep -o '/\(opt\|sw\|Users\|usr/local\)/.*/lib[^/]*dylib' ) ; do 
    bundle_install_lib "$target" "$lib"
  done

  # Force bundling of (system) libltdl (changed in OSX 10.8)
  for lib in $( otool -L $target | grep -o '/usr/lib/libltdl[^/]*dylib' ) ; do 
    bundle_install_lib "$target" "$lib"
  done
}

function bundle_install_plugin {
  local target=$1
  local pluginpath=$2
  local pluginname=$(basename $2)
  local newidname=@executable_path/../Plugins/${pluginpath#${BUNDLE_PLUGINS}}

  echo "Bundling plugin [$pluginpath] for [$target] with relative path [$relpath]"

  install_name_tool -id "$newidname" "$pluginpath"
  install_name_tool -change "$pluginpath" "$newidname" "$target"

  bundle_all_libs "$pluginpath"
  bundle_qt_frameworks "$pluginpath"
}

function bundle_qt_plugins {
  local target=$1
  echo "Bundling Qt plugins for [$target]"
  if [ -e "$QT_PLUGINS_PATH" ]; then
    cp -R "$QT_PLUGINS_PATH" "$BUNDLE_PLUGINS"
    for lib in $( find "$BUNDLE_PLUGINS" -name \*.dylib -print ) ; do 
      bundle_install_plugin "$1" "$lib"
    done
  fi
}

function bundle_framework {
  local target=$1
  local fpath=$2
  local flinked=$3

  local ffullname=${fpath##*/}
  local fbasename=${ffullname%.framework}
  local tmp=${flinked#*Versions/}
  local fversion=${tmp%/*}
  local dest="$ffullname/Versions/$fversion/$fbasename"
  local newid=@executable_path/../Frameworks/$dest

  if [ ! "$fbasename" == "${target##*/}" ]; then
    echo "Bundling Framework [$fpath] to [$BUNDLE_FRAMEWORKS/$ffullname] for [$target]"
    if [ ! -e "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion" ]; then
      mkdir -p "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion"
      ln -s "$fversion" "$BUNDLE_FRAMEWORKS/$ffullname/Versions/Current"
      #lipo -thin $ARCH $fpath/Versions/$fversion/$fbasename 
      #     -output $BUNDLE_FRAMEWORKS/$fname/Versions/$fversion/$fbasename
      cp "$fpath/Versions/$fversion/$fbasename" "$BUNDLE_FRAMEWORKS/$dest"
      if [ -e "$fpath/Versions/$fversion/Resources" ]; then
        cp -R "$fpath/Versions/$fversion/Resources" \
              "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion/Resources"
        ln -s "Versions/$fversion/Resources" "$BUNDLE_FRAMEWORKS/$ffullname/Resources"
      fi
      bundle_qt_frameworks "$BUNDLE_FRAMEWORKS/$dest"
    fi
    install_name_tool -id "$newid" "$BUNDLE_FRAMEWORKS/$dest"
    install_name_tool -change "$flinked" "$newid" "$target"
  fi
}

function bundle_qt_frameworks {
  local target=$1
  mkdir -p "$BUNDLE_FRAMEWORKS"
  for f in $( otool -L ${1}  | grep -o '\(.*Qt.*\.framework/Versions/[^: ]*\) ' ) ; do
    local fpath=${f%%/Versions/*}
    if [ ! -e "$fpath" ] && [ -e "$QT_FRAMEWORKS_PATH/$fpath" ]; then
      local fpath="$QT_FRAMEWORKS_PATH/$fpath"
    fi
    if [ -e "$fpath" ]; then
      bundle_framework "$target" "$fpath" "$f"
    fi
  done
}

function bundle_other_frameworks {
  local target=$1
  local fbasename=$2

  mkdir -p ${BUNDLE_FRAMEWORKS}
  for f in $( otool -L $target  | grep -o ".*${fbasename}.framework/Versions/[^: ]* " ) ; do 
    local fpath=${f%%/Versions/*}
    local replace=@loader_path/../Frameworks
    local fpath=${fpath/$replace/$SPARKLE_FRAMEWORK_PATH}
    if [ -e "$fpath" ]; then
      bundle_framework "$target" "$fpath" "$f"
    fi
  done
}

cd "$BASEDIR"
bundle_all_libs "$EXECUTABLE"
bundle_qt_frameworks "$EXECUTABLE"
bundle_qt_plugins "$EXECUTABLE"
bundle_other_frameworks "$EXECUTABLE" "Sparkle"

