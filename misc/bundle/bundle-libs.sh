#!/bin/bash 

EXECUTABLE=${1}
BUNDLE_RESOURCES=${1%/*}/../Resources

function bundle_install_lib {
  echo Bundling [$2] in [${BUNDLE_RESOURCES}/lib/$3] for [$1]
  if [ ! -f ${BUNDLE_RESOURCES}/lib/${3} ]; then
    cp ${2} ${BUNDLE_RESOURCES}/lib
    install_name_tool -id @executable_path/../Resources/lib/${3} ${BUNDLE_RESOURCES}/lib/${3}
    bundle_all_libs ${BUNDLE_RESOURCES}/lib/${3}
  fi
  install_name_tool -change ${2} @executable_path/../Resources/lib/${3} ${1}
}

function bundle_all_libs {
  echo Bundling all for ${1}
  for lib in $( otool -L ${1}  | grep -o '/\(opt\|sw\|Users\)/.*/lib[^/]*dylib' ) ; do 
	bundle_install_lib ${1} ${lib} $(basename ${lib})  
  done
}

bundle_all_libs ${EXECUTABLE}