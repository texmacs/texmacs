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
  bundle_all_libs ${BUNDLE_FRAMEWORKS}/$3
  fi
  install_name_tool -id @executable_path/../Frameworks/${3} ${BUNDLE_FRAMEWORKS}/$3
  install_name_tool -change ${2} @executable_path/../Frameworks/${3} ${1}
}

function bundle_all_libs {
  echo Bundling all for ${1}
  for lib in $( otool -L ${1}  | grep -o '/\(opt\|sw\|Users\)/.*/lib[^/]*dylib' ) ; do 
	bundle_install_lib ${1} ${lib} $(basename ${lib})  
  done
}


function fix_lib {
  echo Fixing [$2] in [${BUNDLE_FRAMEWORKS}/$3] for [$1]
  fix_libs ${BUNDLE_FRAMEWORKS}/$3
  install_name_tool -id @executable_path/../Frameworks/${3} ${BUNDLE_FRAMEWORKS}/$3
  install_name_tool -change ${2} @executable_path/../Frameworks/${3} ${1}
}

function fix_libs {
  echo Fixing all for ${1}
  for lib in $( otool -L ${1}  | grep -o '/\(opt\|sw\|Users\)/.*/lib[^/]*dylib' ) ; do 
	 fix_lib ${1} ${lib} $(basename ${lib})  
  done
}

function fix_framework {
 if [ ! ${4##*/} == ${1##*/} ]; then
  REF=@executable_path/../Frameworks/${3}/Versions/${4} 
  echo Fixing Framework [${2}] to [${REF}] for [${1}]
  fix_frameworks ${BUNDLE_FRAMEWORKS}/${3}/Versions/${4}
  install_name_tool -id ${REF} ${BUNDLE_FRAMEWORKS}/${3}/Versions/${4}
  install_name_tool -change ${5} ${REF} ${1} 
 fi
}

function fix_frameworks {
  for f in $( otool -L ${1}  | grep -o '\(.*Qt.*\.framework/Versions/[^: ]*\) ' ) ; do 
    fname=${f%%/Versions/*}
    if [ ! -e ${fname} ]; then
      if [ -e /Library/Frameworks/${fname} ]; then
        fname=/Library/Frameworks/${fname}
      fi
    fi 
    if [ -e ${fname} ]; then
  	  fix_framework  ${1} ${fname} ${fname##*/} ${f#*Versions/} ${f}
    fi
  done
}

function fix_plugin {
  REF= @executable_path/../${2}
  echo Fixing Plugin [${1}] to [${REF}]
  lipo -thin i386 ${1} -output ${1}.i386
  mv ${1}.i386 ${1}
  fix_frameworks ${1}
  install_name_tool -id ${REF} ${1}
}




if [ ! -e ${BUNDLE_FRAMEWORKS} ]; then
  mkdir ${BUNDLE_FRAMEWORKS}
fi
#bundle_all_libs ${EXECUTABLE}
#PATH=${MISC_DIR}:$PATH macdeployqt ${APP_BUNDLE} -verbose=2
#fix_libs ${EXECUTABLE}
#fix_frameworks ${EXECUTABLE}

for f in   ${APP_BUNDLE}/Contents/PlugIns/*/*.dylib  ; do
  fix_plugin $f ${f##*Contents/}
done
  