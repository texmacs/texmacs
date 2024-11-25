
#--------------------------------------------------------------------
#
# MODULE      : tm_fastalloc.m4
# DESCRIPTION : Fast memory allocator for TeXmacs
# COPYRIGHT   : (C) 2000, 2017  Joris van der Hoeven
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([TM_FASTALLOC],[
  AC_ARG_ENABLE(fastalloc,
  [  --disable-fastalloc     omit fast allocator for small objects],
      [], [enable_fastalloc=$CONFIG_FASTALLOC])
  case "$enable_fastalloc" in
      yes)
	  ;;
      no)
	  AC_MSG_RESULT([disabling fast allocator for small objects])
	  AC_DEFINE(NO_FAST_ALLOC, 1, [Disable fast memory allocator])
	  ;;
      *)
	  AC_MSG_ERROR([bad option --enable-fastalloc=$enable_fastalloc])
	  ;;
  esac
])
