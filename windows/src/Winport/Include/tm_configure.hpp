
/******************************************************************************
* MODULE     : tm_configure.gen.in or tm_configure.gen.h
* DESCRIPTION: Defines system dependent macros (using autoconf)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TM_CONFIGURE_H
#define TM_CONFIGURE_H

#ifdef OS_WIN32

#define DYNAMIC_LINKING 1
#define USE_GET_TIME_OF_DAY 1
//#define DOTS_OK 1
#define guile_str_size_t int
#define STD_SETENV

//#define OS_GNU_LINUX
#define WORD_LENGTH 4
#define WORD_LENGTH_INC 3
#define WORD_MASK 0xfffffffc
#define MAX_FAST 260 // WORD_LENGTH more than power of 2

#define GUILE_A

#define TEXMACS_VERSION "1.0.1.3"

#define TEXMACS_COPYRIGHT (string("(c) 1999-2003 by Joris van der Hoeven"))

#define TEXMACS_TGZ "@tmtgz@"
#define TEXMACS_RPM "@tmrpm@"
#define TEXMACS_STGZ "@tmstgz@"
#define TEXMACS_SRPM "@tmsrpm@"

#define USE_FREETYPE
#define LINKED_FREETYPE

#endif

#endif // defined TM_CONFIGURE_H
