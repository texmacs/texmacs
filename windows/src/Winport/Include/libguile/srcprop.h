/* classes: h_files */

#ifndef SRCPROPH
#define SRCPROPH
/*	Copyright (C) 1995,1996, 2000 Free Software Foundation
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
 * If you do not wish that, delete this exception notice.
 *
 * The author can be reached at djurfeldt@nada.kth.se
 * Mikael Djurfeldt, SANS/NADA KTH, 10044 STOCKHOLM, SWEDEN */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"



/* {The old whash table interface}
 * *fixme* This is a temporary solution until weak hash table access
 * has been optimized for speed (which is quite necessary, if they are
 * used for recording of source code positions...)
 */

#define scm_whash_handle SCM

#define scm_whash_get_handle(whash, key) scm_hash_fn_get_handle (whash, key, scm_ihashq, scm_sloppy_assq, 0)
#define SCM_WHASHFOUNDP(h) (!SCM_FALSEP (h))
#define SCM_WHASHREF(whash, handle) SCM_CDR (handle)
#define SCM_WHASHSET(whash, handle, obj) SCM_SETCDR (handle, obj)
#define scm_whash_create_handle(whash, key) scm_hash_fn_create_handle_x (whash, key, SCM_UNSPECIFIED, scm_ihashq, scm_sloppy_assq, 0)
#define scm_whash_lookup(whash, obj) scm_hash_fn_ref (whash, obj, SCM_BOOL_F, scm_ihashq, scm_sloppy_assq, 0)
#define scm_whash_insert(whash, key, obj) \
do { \
  register SCM w = (whash); \
  SCM_WHASHSET (w, scm_whash_create_handle (w, key), obj); \
} while (0)


/* {Source properties}
 */

GUILE_API extern long scm_tc16_srcprops;

typedef struct scm_srcprops
{
  unsigned long pos;
  SCM fname;
  SCM copy;
  SCM plist;
} scm_srcprops;

#define SRCPROPS_CHUNKSIZE 2047 /* Number of srcprops per chunk */
typedef struct scm_srcprops_chunk
{
  struct scm_srcprops_chunk *next;
  scm_srcprops srcprops[1];
} scm_srcprops_chunk;

#define SRCPROPSP(p) (SCM_NIMP(p) && (SCM_TYP16 (p) == scm_tc16_srcprops))
#define SRCPROPBRK(p) (SCM_BOOL (SCM_CELL_WORD_0 (p) & (1L << 16)))
#define SRCPROPPOS(p) ((scm_srcprops *) SCM_CELL_WORD_1 (p))->pos
#define SRCPROPLINE(p) (SRCPROPPOS(p) >> 12)
#define SRCPROPCOL(p) (SRCPROPPOS(p) & 0x0fffL)
#define SRCPROPFNAME(p) ((scm_srcprops *) SCM_CELL_WORD_1 (p))->fname
#define SRCPROPCOPY(p) ((scm_srcprops *) SCM_CELL_WORD_1 (p))->copy
#define SRCPROPPLIST(p) ((scm_srcprops *) SCM_CELL_WORD_1 (p))->plist
#define SETSRCPROPBRK(p) (SCM_SETOR_CAR (p, (1L << 16)))
#define CLEARSRCPROPBRK(p) SCM_SETAND_CAR (p, ~(1L << 16))
#define SRCPROPMAKPOS(l,c) (((l) << 12) + (c))
#define SETSRCPROPPOS(p,l,c) (SRCPROPPOS (p) = SRCPROPMAKPOS (l, c))
#define SETSRCPROPLINE(p,l) SETSRCPROPPOS (p, l, SRCPROPCOL (p))
#define SETSRCPROPCOL(p,c) SETSRCPROPPOS (p, SRCPROPLINE (p), c)

#define SRCBRKP(x) (SCM_NIMP (t.arg1 = scm_whash_lookup (scm_source_whash, (x)))\
		    && SRCPROPSP (t.arg1)\
		    && (SCM_CELL_WORD_0 (t.arg1) & (1L << 16)))

#define PROCTRACEP(x) SCM_NFALSEP (scm_procedure_property (x, scm_sym_trace))

GUILE_API extern SCM scm_sym_filename;
GUILE_API extern SCM scm_sym_copy;
GUILE_API extern SCM scm_sym_line;
GUILE_API extern SCM scm_sym_column;
GUILE_API extern SCM scm_sym_breakpoint;




GUILE_API extern SCM scm_srcprops_to_plist (SCM obj);
GUILE_API extern SCM scm_make_srcprops (int line, int col, SCM fname, SCM copy, SCM plist);
GUILE_API extern SCM scm_source_property (SCM obj, SCM key);
GUILE_API extern SCM scm_set_source_property_x (SCM obj, SCM key, SCM datum);
GUILE_API extern SCM scm_source_properties (SCM obj);
GUILE_API extern SCM scm_set_source_properties_x (SCM obj, SCM props);
GUILE_API extern void scm_finish_srcprop (void);
GUILE_API extern void scm_init_srcprop (void);

#endif /* SRCPROPH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
