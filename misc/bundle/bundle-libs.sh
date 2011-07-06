#!/bin/bash 

EXECUTABLE=${1}
BUNDLE_RESOURCES=${1%/*}/../Resources
BUNDLE_FRAMEWORKS=${1%/*}/../Frameworks

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


function bundle_framework {
 if [ ! ${4##*/} == ${1##*/} ]; then
  echo Bundling Framework [${2}] to [${3}/Versions/${4}] for [${1}]
  if [ ! -e ${BUNDLE_FRAMEWORKS}/${3} ]; then
    mkdir ${BUNDLE_FRAMEWORKS}/${3}
    mkdir ${BUNDLE_FRAMEWORKS}/${3}/Versions
    mkdir ${BUNDLE_FRAMEWORKS}/${3}/Versions/4
    ln -s 4 ${BUNDLE_FRAMEWORKS}/${3}/Versions/Current
    ln -s 4 ${BUNDLE_FRAMEWORKS}/${3}/Versions/4.0
    lipo -thin `arch` ${2}/Versions/${4} -output ${BUNDLE_FRAMEWORKS}/${3}/Versions/${4}
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
      if [ -e /Library/Frameworks/${fname} ]; then
        fname=/Library/Frameworks/${fname}
      fi
    fi 
    if [ -e ${fname} ]; then
  	  bundle_framework  ${1} ${fname} ${fname##*/} ${f#*Versions/} ${f}
    fi
  done
}


bundle_all_libs ${EXECUTABLE}
bundle_qt_frameworks ${EXECUTABLE}
