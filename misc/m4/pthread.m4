
#--------------------------------------------------------------------
#
# MODULE      : pthread.m4
# DESCRIPTION : TeXmacs configuration options for pthread
# COPYRIGHT   : (C) 2025 Liza Belos
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

AC_DEFUN([LC_PTHREAD],[
  LC_CHECK_LIB([pthread],[pthread_create],[
    LC_APPEND_FLAG([-lpthread],[LDFLAGS])
  ],[:],[],[$0_extralibs])
])