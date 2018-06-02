
/******************************************************************************
* MODULE     : tm_configure.gen.in or tm_configure.gen.h
* DESCRIPTION: Defines system dependent macros (using autoconf)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_CONFIGURE_H
#define TM_CONFIGURE_H

@CONFIG_STD_SETENV@

#define TEXMACS_VERSION "@DEVEL_VERSION@"
#define TEXMACS_SOURCES "@tmorig@"
#define OS_@CONFIG_OS@
#define WORD_LENGTH @CONFIG_WORD_LENGTH@
#define WORD_LENGTH_INC @CONFIG_WORD_LENGTH_INC@
#define WORD_MASK @CONFIG_WORD_MASK@
#define MAX_FAST @CONFIG_MAX_FAST@

#define HOST_OS "@CONFIG_HOST_OS@"
#define HOST_VENDOR "@CONFIG_HOST_VENDOR@"
#define HOST_CPU "@CONFIG_HOST_CPU@"
#define BUILD_USER "@CONFIG_USER@"
#define BUILD_DATE "@CONFIG_DATE@"

#define TM_DEVEL "@tm_devel@"
#define TM_DEVEL_RELEASE "@tm_devel_release@"
#define TM_STABLE "@tm_stable@"
#define TM_STABLE_RELEASE "@tm_stable_release@"

#endif // defined TM_CONFIGURE_H
