/* classes: h_files */

#ifndef PROCSH
#define PROCSH
/*	Copyright (C) 1995, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
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

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */


#include "libguile/__scm.h"




/* Subrs 
 */

typedef struct
{
  SCM handle;			/* link back to procedure object */
  SCM name;
  SCM *generic;			/* 0 if no generic support
				 * *generic == 0 until first method
				 */
  SCM properties;		/* procedure properties */
  SCM documentation;
} scm_subr_entry;

#define SCM_SUBRNUM(subr) (SCM_CELL_WORD_0 (subr) >> 8)
#define SCM_SET_SUBRNUM(subr, num) \
        SCM_SET_CELL_WORD_0 (subr, (num << 8) + SCM_TYP7 (subr))
#define SCM_SUBR_ENTRY(x) (scm_subr_table[SCM_SUBRNUM (x)])
#define SCM_SNAME(x) (SCM_SUBR_ENTRY (x).name)
#define SCM_SUBRF(x) ((SCM (*)()) SCM_CELL_WORD_1 (x))
#define SCM_SET_SUBRF(x, v) (SCM_SET_CELL_WORD_1 ((x), (v)))
#define SCM_DSUBRF(x) ((double (*)()) SCM_CELL_WORD_1 (x))
#define SCM_CCLO_SUBR(x) (SCM_VELTS(x)[0])

#define SCM_SUBR_GENERIC(x) (SCM_SUBR_ENTRY (x).generic)
#define SCM_SUBR_PROPS(x) (SCM_SUBR_ENTRY (x).properties)
#define SCM_SUBR_DOC(x) (SCM_SUBR_ENTRY (x).documentation)

/* Closures
 */

#define SCM_CLOSUREP(x) (SCM_NIMP(x) && (SCM_TYP3 (x) == scm_tc3_closure))
#define SCM_CLOSCAR(x) SCM_PACK (SCM_CELL_WORD_0 (x) - scm_tc3_closure)
#define SCM_CODE(x) SCM_CAR (SCM_CLOSCAR (x))
#define SCM_PROCPROPS(x) SCM_CDR (SCM_CLOSCAR (x))
#define SCM_SETPROCPROPS(x, p) SCM_SETCDR (SCM_CLOSCAR (x), p)
#define SCM_SETCODE(x, e) (SCM_SET_CELL_WORD_0 (x, SCM_UNPACK (scm_cons ((e), SCM_EOL)) \
                           + scm_tc3_closure))
#define SCM_ENV(x) SCM_CDR(x)
#define SCM_SETENV(x, e) SCM_SETCDR (x, e)
#define SCM_TOP_LEVEL(ENV)  (SCM_NULLP (ENV) || (SCM_EQ_P (scm_procedure_p (SCM_CAR (ENV)), SCM_BOOL_T)))

/* Procedure-with-setter

   Four representations for procedure-with-setters were
   considered before selecting this one:

   1. A closure where the CODE and ENV slots are used to represent
   the getter and a new SETTER slot is used for the setter.  The
   original getter is stored as a `getter' procedure property.  For
   closure getters, the CODE and ENV slots contains a copy of the
   getter's CODE and ENV slots.  For subr getters, the CODE contains
   a call to the subr.

   2. A compiled closure with a call to the getter in the cclo
   procedure.  The getter and setter are stored in slots 1 and 2.

   3. An entity (i.e. a struct with an associated procedure) with a
   call to the getter in the entity procedure and the setter stored
   in slot 0.  The original getter is stored in slot 1.

   4. A new primitive procedure type supported in the evaluator.  The
   getter and setter are stored in a GETTER and SETTER slot.  A call
   to this procedure type results in a retrieval of the getter and a
   jump back to the correct eval dispatcher.

   Representation 4 was selected because of efficiency and
   simplicity.

   Rep 1 has the advantage that there is zero penalty for closure
   getters, but primitive getters will get considerable overhead
   because the procedure-with-getter will be a closure which calls
   the getter.

   Rep 3 has the advantage that a GOOPS accessor can be a subclass of
   <procedure-with-setter>, but together with rep 2 it suffers from a
   three level dispatch for non-GOOPS getters:

     cclo/struct --> dispatch proc --> getter

   This is because the dispatch procedure must take an extra initial
   argument (cclo for rep 2, struct for rep 3).

   Rep 4 has the single disadvantage that it uses up one tc7 type
   code, but the plan for uniform vectors will very likely free tc7
   codes, so this is probably no big problem.  Also note that the
   GETTER and SETTER slots can live directly on the heap, using the
   new four-word cells.  */

#define SCM_PROCEDURE_WITH_SETTER_P(obj) (SCM_NIMP(obj) && (SCM_TYP7 (obj) == scm_tc7_pws))
#define SCM_PROCEDURE(obj) SCM_CELL_OBJECT_1 (obj)
#define SCM_SETTER(obj) SCM_CELL_OBJECT_2 (obj)

GUILE_API extern scm_subr_entry *scm_subr_table;
GUILE_API extern int scm_subr_table_size;
GUILE_API extern int scm_subr_table_room;



GUILE_API extern void scm_mark_subr_table (void);
GUILE_API extern void scm_free_subr_entry (SCM subr);
GUILE_API extern SCM scm_make_subr (const char *name, int type, SCM (*fcn) ());
GUILE_API extern SCM scm_make_subr_with_generic (const char *name,
				       int type,
				       SCM (*fcn) (),
				       SCM *gf);
GUILE_API extern SCM scm_make_subr_opt (const char *name, 
                              int type, 
                              SCM (*fcn) (),
                              int set);
GUILE_API extern SCM scm_makcclo (SCM proc, long len);
GUILE_API extern SCM scm_procedure_p (SCM obj);
GUILE_API extern SCM scm_closure_p (SCM obj);
GUILE_API extern SCM scm_thunk_p (SCM obj);
GUILE_API extern int scm_subr_p (SCM obj);
GUILE_API extern SCM scm_procedure_documentation (SCM proc);
GUILE_API extern SCM scm_procedure_with_setter_p (SCM obj);
GUILE_API extern SCM scm_make_procedure_with_setter (SCM procedure, SCM setter);
GUILE_API extern SCM scm_procedure (SCM proc);
GUILE_API extern SCM scm_setter (SCM proc);
GUILE_API extern void scm_init_subr_table (void);
GUILE_API extern void scm_init_procs (void);

#ifdef GUILE_DEBUG
GUILE_API extern SCM scm_make_cclo (SCM proc, SCM len);
#endif /*GUILE_DEBUG*/


#endif  /* PROCSH */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
