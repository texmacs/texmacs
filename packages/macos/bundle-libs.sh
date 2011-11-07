#!/bin/bash 

EXECUTABLE=${1}
BUNDLE_RESOURCES=${1%/*}/../Resources
BUNDLE_FRAMEWORKS=${1%/*}/../Frameworks
BUNDLE_PLUGINS=${1%/*}/../Plugins

if [ x${QT_FRAMEWORKS_PATH}x == xx ]; then
  QT_FRAMEWORKS_PATH=/Library/Frameworks
fi

if [ x${QT_PLUGINS_PATH}x == xx ]; then
 if [exists $QT_FRAMEWORKS_PATH/plugins]; then 
  QT_PLUGINS_PATH=$QT_FRAMEWORKS_PATH/plugins;
 else 
  QT_PLUGINS_PATH=/Developer/Applications/Qt/plugins
 fi
fi

echo Qt Frameworks path [$QT_FRAMEWORKS_PATH]

function bundle_install_lib {
  echo Bundling [$2] in [${BUNDLE_RESOURCES}/lib/$3] for [$1]
  if [ ! -f ${BUNDLE_RESOURCES}/lib/${3} ]; then
    cp ${2} ${BUNDLE_RESOURCES}/lib
  fi
  install_name_tool -id @executable_path/../Resources/lib/${3} ${BUNDLE_RESOURCES}/lib/${3}
  bundle_all_libs ${BUNDLE_RESOURCES}/lib/${3}
  install_name_tool -change ${2} @executable_path/../Resources/lib/${3} ${1}
}

function bundle_all_libs {
  echo Bundling all for ${1}
  for lib in $( otool -L ${1}  | grep -o '/\(opt\|sw\|Users\|usr/local\)/.*/lib[^/]*dylib' ) ; do 
	bundle_install_lib ${1} ${lib} $(basename ${lib})  
  done
}



function bundle_install_plugin {
  RELPATH=@executable_path/../Plugins/${2#${BUNDLE_PLUGINS}}
  echo Bundling plugin [$2] for [$1] relpath [${RELPATH}]
  install_name_tool -id ${RELPATH} ${2}
  install_name_tool -change ${3} ${RELPATH} ${1}
  bundle_all_libs ${2}
  bundle_qt_frameworks ${2}
}

function bundle_qt_plugins {
  echo Bundling Qt plugins for ${1}
  cp -R ${QT_PLUGINS_PATH} ${BUNDLE_PLUGINS}
  for lib in $( find ${BUNDLE_PLUGINS} -name \*.dylib -print ) ; do 
	  bundle_install_plugin ${1} ${lib} $(basename ${lib})  
  done
}



function bundle_framework {
 if [ ! ${4##*/} == ${1##*/} ]; then
  echo Bundling Framework [${2}] to [${3}/Versions/${4}] for [${1}]
  if [ ! -e ${BUNDLE_FRAMEWORKS}/${3} ]; then
    mkdir ${BUNDLE_FRAMEWORKS}/${3}
    mkdir ${BUNDLE_FRAMEWORKS}/${3}/Versions
    mkdir ${BUNDLE_FRAMEWORKS}/${3}/Versions/4
    ln -s 4 ${BUNDLE_FRAMEWORKS}/${3}/Versions/Current
    ln -s 4 ${BUNDLE_FRAMEWORKS}/${3}/Versions/4.0
#    lipo -thin $ARCH ${2}/Versions/${4} -output ${BUNDLE_FRAMEWORKS}/${3}/Versions/${4}
    cp ${2}/Versions/${4} ${BUNDLE_FRAMEWORKS}/${3}/Versions/${4}
    if [ -e  ${2}/Versions/4/Resources ]; then
      cp -R ${2}/Versions/4/Resources ${BUNDLE_FRAMEWORKS}/${3}/Versions/4
      ln -s Versions/4/Resources ${BUNDLE_FRAMEWORKS}/${3}/Resources
    fi
    bundle_qt_frameworks ${BUNDLE_FRAMEWORKS}/${3}/Versions/${4}
  fi
  install_name_tool -id @executable_path/../Frameworks/${3}/Versions/${4} ${BUNDLE_FRAMEWORKS}/${3}/Versions/${4}
  install_name_tool -change ${5} @executable_path/../Frameworks/${3}/Versions/${4} ${1} 
 fi
}

function bundle_qt_frameworks {
  if [ ! -e ${BUNDLE_FRAMEWORKS} ]; then
    mkdir ${BUNDLE_FRAMEWORKS}
  fi
  for f in $( otool -L ${1}  | grep -o '\(.*Qt.*\.framework/Versions/[^: ]*\) ' ) ; do 
    fname=${f%%/Versions/*}
    if [ ! -e ${fname} ]; then
      if [ -e ${QT_FRAMEWORKS_PATH}/${fname} ]; then
        fname=${QT_FRAMEWORKS_PATH}/${fname}
      fi
    fi 
    if [ -e ${fname} ]; then
  	  bundle_framework  ${1} ${fname} ${fname##*/} ${f#*Versions/} ${f}
    fi
  done
}


bundle_all_libs ${EXECUTABLE}
bundle_qt_frameworks ${EXECUTABLE}
bundle_qt_plugins ${EXECUTABLE}
