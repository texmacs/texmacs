
#--------------------------------------------------------------------
#
# MODULE      : tm_repo.m4
# DESCRIPTION : Get libraries from SDK
# COPYRIGHT   : (C) 2017  Joris van der Hoeven, Denis Raux
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_REPO],[
  AC_ARG_WITH(tmrepo,
    AS_HELP_STRING([--with-tmrepo@<:@=no@:>@],[absolute texmacs sdk path]),[
      [[ -d $withval ]] || AC_MSG_ERROR([tmrepo path not found])
      TMREPO=${withval%%/}
      AC_MSG_NOTICE([Using TeXmacs SDK at $TMREPO])
    ],[])

  if test -n "$TMREPO"
  then 
    PATH=$TMREPO/bin:$PATH

    # memorize current pkgconfig location
    if test -x "$(type -P pkg-config)"
    then oldpkgdefault=$(pkg-config --variable pc_path pkg-config)
      if test -n "$oldpkgdefault"
      then oldpkgconfig="$oldpkgdefault"
      else oldpkgconfig=/usr/lib/pkgconfig:/usr/share/lib/pkgconfig
      fi
    else AC_MSG_WARN([pkg-config not found : configuration may fail])
    fi
	
    export PKG_CONFIG_PATH="$TMREPO/lib/pkgconfig:$PKG_CONFIG_PATH:$oldpkgconfig"
    LC_MERGE_FLAGS([-I$TMREPO/include],[CPPFLAGS])
    # LC_SCATTER_FLAGS([-I$TMREPO/include],[BASE])
    LC_MERGE_FLAGS([-L$TMREPO/lib],[LDFLAGS])
    AC_SUBST(TMREPO)
    AC_SUBST(REPOPATH, ["export PATH=$PATH"])
    AC_SUBST(PKGPATH, ["export PKG_CONFIG_PATH=$PKG_CONFIG_PATH"])
  fi
])
