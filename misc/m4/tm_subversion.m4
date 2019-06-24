
#--------------------------------------------------------------------
#
# MODULE      : tm_subversion.m4
# DESCRIPTION : Version management settings
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_SUBVERSION],[
  AC_MSG_CHECKING(current Subversion revision number)
  SVNREV=`svnversion -n . 2>/dev/null`
  SVNREV=${SVNREV#*:}
 if { test "$SVNREV" = "" || test "$SVNREV" = "exported" ; } ; then 
    SVNREV=`cat $srcdir/SVNREV`
    AC_MSG_RESULT($SVNREV, read from $srcdir/SVNREV)
  else 
    echo "$SVNREV" > $srcdir/SVNREV
    echo "$SVNREV" > $srcdir/TeXmacs/SVNREV
    AC_MSG_RESULT($SVNREV)
  fi
  
  AC_SUBST(SVNREV)
  SVNINT=${SVNREV#*:}
  SVNINT=$(echo ${SVNINT%%@<:@^@<:@:digit:@:>@@:>@*})

#Naming package strategy
#   if test "$SVNREV" != $DEVEL_REVISION
#   then AC_SUBST(REVISION,[-${SVNINT}])
#   else AC_SUBST(REVISION,[""])
#   fi
#Not Implemeted yet
  AC_SUBST(REVISION,[""])
  
  if test "$SVNREV" != "$DEVEL_REVISION"
  then AC_DEFINE_UNQUOTED(TEXMACS_REVISION, ["Custom $SVNREV"],[Svn build revision])
  else AC_DEFINE_UNQUOTED(TEXMACS_REVISION,[$DEVEL_REVISION],[Svn build revision])
  fi

  SVNPATCH=0
  test SVNINT != SVNREV && SVNPATCH=VS_FF_PATCHED
  AC_SUBST(SVNPATCH)
])
