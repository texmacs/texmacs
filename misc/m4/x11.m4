#-------------------------------------------------------------------
# Modify the X include files to make them C++-compatible, if needed
#-------------------------------------------------------------------
 
AC_DEFUN(LC_X_HEADERS,[
  AX_SAVE_FLAGS
  LC_CLEAR_FLAGS
  AC_PATH_X
# path building : the first two levels 
  lstx=/{usr,opt}/{.,local,[xX]386,XFree86,athena,openwin,lpp,unsupported}
# path building : the next two levels 
  lstx=$lstx/{{.,[[Xx]]11*,share,Xamples}/include,include/[[Xx]]11*}
  if [[[ $no_x != "yes" ]]] 
  then [[[ -z $x_includes ]]] && x_includes="$(dirname $x_libraries)/include"
    lstx="$x_includes $lstx"
  fi
  no_x=yes
  for x_includes in $(eval echo $lstx)
  do if [[ -d $x_includes ]]
      then CPPFLAGS="-I$x_includes"
        AC_CHECK_HEADERS(X11/Xlib.h X11/Xutil.h, [
          unset no_x
        ],[
          unset x_libraries #the AC_X_PATH path must be forgotten
          no_x="yes"
        ])
        test $no_x || break 2 #found headers
      fi
  done
  if test $no_x
  then unset x_includes
  else x_libraries=${x_includes/include/lib}
  fi   AX_RESTORE_FLAGS
  LC_SCATTER_FLAGS([-I$x_includes -L$x_libraries -lXext -lX11],[X11])
  LC_SUBST(X11)
  LC_COMBINE_FLAGS([X11])
])
