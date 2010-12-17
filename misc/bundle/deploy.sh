#!/bin/bash 

MISC_DIR=${2} 
APP_BUNDLE=${1}
EXECUTABLE=${APP_BUNDLE}/Contents/MacOS/TeXmacs 
BUNDLE_RESOURCES=${APP_BUNDLE}/Contents/Resources
BUNDLE_FRAMEWORKS=${APP_BUNDLE}/Contents/Frameworks

function bundle_install_lib {
  echo Bundling [$2] in [${BUNDLE_FRAMEWORKS}/$3] for [$1]
  if [ ! -f ${BUNDLE_FRAMEWORKS}/$3 ]; then
    cp ${2} ${BUNDLE_FRAMEWORKS}/$3
    install_name_tool -id @executable_path/../Frameworks/${3} ${BUNDLE_FRAMEWORKS}/$3
    bundle_all_libs ${BUNDLE_FRAMEWORKS}/$3
  fi
  install_name_tool -change ${2} @executable_path/../Frameworks/${3} ${1}
}

function bundle_all_libs {
  echo Bundling all for ${1}
  for lib in $( otool -L ${1}  | grep -o '/\(opt\|sw\|Users\)/.*/lib[^/]*dylib' ) ; do 
	bundle_install_lib ${1} ${lib} $(basename ${lib})  
  done
}


function bundle_framework {
 if [ ! ${4##*/} == ${1##*/} ]; then
  echo Bundling Framework [${2}] to [${3}/Versions/${4}] for [${1}]
  if [ ! -e ${BUNDLE_FRAMEWORKS}/${3} ]; then
    cp -R ${2} ${BUNDLE_FRAMEWORKS}
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
      if [ -e /Library/Frameworks/${fname} ]; then
        fname=/Library/Frameworks/${fname}
      fi
    fi 
    if [ -e ${fname} ]; then
  	  bundle_framework  ${1} ${fname} ${fname##*/} ${f#*Versions/} ${f}
    fi
  done
}

mkdir ${BUNDLE_FRAMEWORKS}
bundle_all_libs ${EXECUTABLE}
PATH=${MISC_DIR}:$PATH macdeployqt ${APP_BUNDLE} -verbose=2
