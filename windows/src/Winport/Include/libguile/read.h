/* classes: h_files */

#ifndef READH
#define READH
/*	Copyright (C) 1995,1996, 2000 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */


#include "libguile/__scm.h"

#include "libguile/options.h"


/* SCM_LINE_INCREMENTORS are the characters which cause the line count to
 * be incremented for the purposes of error reporting.  This feature
 * is only used for scheme code loaded from files.
 *
 * SCM_WHITE_SPACES are other characters which should be treated like spaces
 * in programs.
 */

#define SCM_LINE_INCREMENTORS  '\n'

#ifdef MSDOS
# define SCM_SINGLE_SPACES  ' ':case '\r':case '\f': case 26
#else
# define SCM_SINGLE_SPACES  ' ':case '\r':case '\f'
#endif

#define SCM_WHITE_SPACES  SCM_SINGLE_SPACES: case '\t'

GUILE_API extern scm_option scm_read_opts[];

#define SCM_COPY_SOURCE_P      scm_read_opts[0].val
#define SCM_RECORD_POSITIONS_P scm_read_opts[1].val
#define SCM_CASE_INSENSITIVE_P scm_read_opts[2].val
#define SCM_KEYWORD_STYLE      scm_read_opts[3].val
#define SCM_N_READ_OPTIONS 4



GUILE_API extern SCM scm_read_options (SCM setting);
GUILE_API extern SCM scm_read (SCM port);
GUILE_API extern char * scm_grow_tok_buf (SCM * tok_buf);
GUILE_API extern int scm_flush_ws (SCM port, const char *eoferr);
GUILE_API extern int scm_casei_streq (char * s1, char * s2);
GUILE_API extern SCM scm_lreadr (SCM * tok_buf, SCM port, SCM *copy);
GUILE_API extern scm_sizet scm_read_token (int ic, SCM * tok_buf, SCM port, int weird);
GUILE_API extern SCM scm_lreadparen (SCM * tok_buf, SCM port, char *name, SCM *copy);
GUILE_API extern SCM scm_lreadrecparen (SCM * tok_buf, SCM port, char *name, SCM *copy);
GUILE_API extern SCM scm_read_hash_extend (SCM chr, SCM proc);
GUILE_API extern void scm_init_read (void);

#endif  /* READH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
