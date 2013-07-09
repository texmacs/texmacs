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

echo Qt Frameworks path: [$QT_FRAMEWORKS_PATH]

function bundle_install_lib {
  target=$1
  libpath=$2
  libdest=${BUNDLE_RESOURCES}/lib/$(basename $2)
  newidname="@executable_path/../Resources/lib/$3"

  echo Bundling [$libpath] in [$libdest] for [$target]
  
  if [ ! -f $libdest ]; then
    cp ${2} ${BUNDLE_RESOURCES}/lib
  fi

  install_name_tool -id $newidname $libdest
  bundle_all_libs $libdest
  install_name_tool -change $libpath $newidname $target
}

function bundle_all_libs {
  target=$1
  echo Bundling all libraries for $target
  for lib in $( otool -L $target  | grep -o '/\(opt\|sw\|Users\|usr/local\)/.*/lib[^/]*dylib' ) ; do 
    bundle_install_lib "$target" "$lib"
  done

  # Force bundling of (system) libltdl (changed in OSX 10.8)
  for lib in $( otool -L $target | grep -o '/usr/lib/libltdl[^/]*dylib' ) ; do 
    bundle_install_lib "$target" "$lib"
  done
}

function bundle_install_plugin {
  target=$1
  pluginpath=$2
  pluginname=$(basename $2)
  newidname=@executable_path/../Plugins/${pluginpath#${BUNDLE_PLUGINS}}

  echo Bundling plugin [$pluginpath] for [$target] with relative path [$relpath]

  install_name_tool -id $relpath $pluginpath
  install_name_tool -change $pluginname $relpath $target
  bundle_all_libs $pluginpath
  bundle_qt_frameworks $pluginpath
}

function bundle_qt_plugins {
  target=$1
  echo Bundling Qt plugins for $target
  if [ -e "$QT_PLUGINS_PATH" ]; then
    cp -R "$QT_PLUGINS_PATH" "$BUNDLE_PLUGINS"
    for lib in $( find "$BUNDLE_PLUGINS" -name \*.dylib -print ) ; do 
      bundle_install_plugin "$1" "$lib"
    done
  fi
}

function bundle_framework {
  target=$1
  fpath=$2
  flinked=$3

  ffullname=${fpath##*/}
  fbasename=${ffullname%.framework}
  tmp=${flinked#*Versions/}
  fversion=${tmp%/*}

  if [ ! "$fbasename" == "${target##*/}" ]; then
    echo Bundling Framework [$fpath] to [$BUNDLE_FRAMEWORKS/$ffullname] for [$target]
    if [ ! -e "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion$" ]; then
      mkdir -p "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion"
      ln -s "$fversion" "$BUNDLE_FRAMEWORKS/$ffullname/Versions/Current"
      #lipo -thin $ARCH $fpath/Versions/$fversion/$fbasename 
      #     -output $BUNDLE_FRAMEWORKS/$fname/Versions/$fversion/$fbasename
      cp "$fpath/Versions/$fversion/$fbasename" \
         "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion/$fbasename"
      if [ -e "$fpath/Versions/$fversion/Resources" ]; then
        cp -R "$fpath/Versions/$fversion/Resources" \
              "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion/Resources"
        ln -s "Versions/$fversion/Resources" "$BUNDLE_FRAMEWORKS/$ffullname/Resources"
      fi
      bundle_qt_frameworks "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion/$fbasename"
    fi
    newid=@executable_path/../Frameworks/$ffullname/Versions/$fversion/$fbasename
    install_name_tool -id "$newid" "$BUNDLE_FRAMEWORKS/$ffullname/Versions/$fversion/$fbasename"
    install_name_tool -change "$flinked" "$newid" "$target"
  fi
}

function bundle_qt_frameworks {
  target=$1
  mkdir -p "$BUNDLE_FRAMEWORKS"
  for f in $( otool -L ${1}  | grep -o '\(.*Qt.*\.framework/Versions/[^: ]*\) ' ) ; do
    fpath=${f%%/Versions/*}
    if [ ! -e "$fpath" ]; then
      if [ -e ${QT_FRAMEWORKS_PATH}/${fpath} ]; then
        fpath=${QT_FRAMEWORKS_PATH}/${fpath}
      fi
    fi 
    if [ -e "$fpath" ]; then
      bundle_framework $target $fpath $f
    fi
  done
}

function bundle_other_frameworks {
  target=$1
  fbasename=$2

  mkdir -p ${BUNDLE_FRAMEWORKS}
  for f in $( otool -L $target  | grep -o ".*${fbasename}.framework/Versions/[^: ]* " ) ; do 
    fpath=${f%%/Versions/*}
    replace=@loader_path/../Frameworks
    fpath=${fpath/$replace/$SPARKLE_FRAMEWORK_PATH}
    if [ -e "$fpath" ]; then
      bundle_framework $target $fpath $f
    fi
  done
}

cd "$BASEDIR"
bundle_all_libs ${EXECUTABLE}
bundle_qt_frameworks ${EXECUTABLE}
bundle_qt_plugins ${EXECUTABLE}
bundle_other_frameworks ${EXECUTABLE} "Sparkle"

