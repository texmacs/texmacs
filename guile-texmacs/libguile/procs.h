/* classes: h_files */

#ifndef SCM_PROCS_H
#define SCM_PROCS_H

/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2006 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */



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
} scm_t_subr_entry;

#define SCM_SUBRNUM(subr) (SCM_CELL_WORD_0 (subr) >> 8)
#define SCM_SET_SUBRNUM(subr, num) \
        SCM_SET_CELL_WORD_0 (subr, (num << 8) + SCM_TYP7 (subr))
#define SCM_SUBR_ENTRY(x) (scm_subr_table[SCM_SUBRNUM (x)])
#define SCM_SNAME(x) (SCM_SUBR_ENTRY (x).name)
#define SCM_SUBRF(x) ((SCM (*)()) SCM_CELL_WORD_1 (x))
#define SCM_SET_SUBRF(x, v) (SCM_SET_CELL_WORD_1 ((x), (v)))
#define SCM_DSUBRF(x) ((double (*)()) SCM_CELL_WORD_1 (x))
#define SCM_SUBR_PROPS(x) (SCM_SUBR_ENTRY (x).properties)
#define SCM_SUBR_GENERIC(x) (SCM_SUBR_ENTRY (x).generic)

#define SCM_CCLO_LENGTH(x) (SCM_CELL_WORD_0 (x) >> 8)
#define SCM_MAKE_CCLO_TAG(v)  (((v) << 8) + scm_tc7_cclo)
#define SCM_SET_CCLO_LENGTH(x, v) (SCM_SET_CELL_WORD_0 ((x), SCM_MAKE_CCLO_TAG(v)))
#define SCM_CCLO_BASE(x) ((scm_t_bits *) SCM_CELL_WORD_1 (x))
#define SCM_SET_CCLO_BASE(x, v) (SCM_SET_CELL_WORD_1 ((x), (v)))

#define SCM_CCLO_REF(x, i) (SCM_PACK (SCM_CCLO_BASE (x) [i]))
#define SCM_CCLO_SET(x, i, v) (SCM_CCLO_BASE (x) [i] = SCM_UNPACK (v))

#define SCM_CCLO_SUBR(x) (SCM_CCLO_REF ((x), 0))
#define SCM_SET_CCLO_SUBR(x, v) (SCM_CCLO_SET ((x), 0, (v)))

/* Closures
 */

#define SCM_CLOSUREP(x) (!SCM_IMP(x) && (SCM_TYP3 (x) == scm_tc3_closure))
#define SCM_CLOSCAR(x) SCM_PACK (SCM_CELL_WORD_0 (x) - scm_tc3_closure)
#define SCM_CODE(x) SCM_CAR (SCM_CLOSCAR (x))
#define SCM_CLOSURE_FORMALS(x) SCM_CAR (SCM_CODE (x))
#define SCM_CLOSURE_BODY(x) SCM_CDR (SCM_CODE (x))
#define SCM_PROCPROPS(x) SCM_CDR (SCM_CLOSCAR (x))
#define SCM_SETPROCPROPS(x, p) SCM_SETCDR (SCM_CLOSCAR (x), p)
#define SCM_SETCODE(x, e) (SCM_SET_CELL_WORD_0 (x, SCM_UNPACK (scm_cons ((e), SCM_EOL)) \
                           + scm_tc3_closure))
#define SCM_ENV(x) SCM_CELL_OBJECT_1 (x)
#define SCM_SETENV(x, e) SCM_SET_CELL_OBJECT_1 ((x), (e))
#define SCM_TOP_LEVEL(ENV)  (scm_is_null (ENV) || (scm_is_true (scm_procedure_p (SCM_CAR (ENV)))))

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

#define SCM_PROCEDURE_WITH_SETTER_P(obj) (!SCM_IMP(obj) && (SCM_TYP7 (obj) == scm_tc7_pws))
#define SCM_PROCEDURE(obj) SCM_CELL_OBJECT_1 (obj)
#define SCM_SETTER(obj) SCM_CELL_OBJECT_2 (obj)

SCM_API scm_t_subr_entry *scm_subr_table;
SCM_API long scm_subr_table_size;
SCM_API long scm_subr_table_room;



SCM_API void scm_mark_subr_table (void);
SCM_API void scm_free_subr_entry (SCM subr);
SCM_API SCM scm_c_make_subr (const char *name, long type, SCM (*fcn)());
SCM_API SCM scm_c_make_subr_with_generic (const char *name, long type,
					  SCM (*fcn)(), SCM *gf);
SCM_API SCM scm_c_define_subr (const char *name, long type, SCM (*fcn)());
SCM_API SCM scm_c_define_subr_with_generic (const char *name, long type,
					    SCM (*fcn)(), SCM *gf);
SCM_API SCM scm_makcclo (SCM proc, size_t len);
SCM_API SCM scm_procedure_p (SCM obj);
SCM_API SCM scm_closure_p (SCM obj);
SCM_API SCM scm_thunk_p (SCM obj);
SCM_API int scm_subr_p (SCM obj);
SCM_API SCM scm_procedure_documentation (SCM proc);
SCM_API SCM scm_procedure_with_setter_p (SCM obj);
SCM_API SCM scm_make_procedure_with_setter (SCM procedure, SCM setter);
SCM_API SCM scm_procedure (SCM proc);
SCM_API SCM scm_setter (SCM proc);
SCM_API void scm_init_subr_table (void);
SCM_API void scm_init_procs (void);

#ifdef GUILE_DEBUG
SCM_API SCM scm_make_cclo (SCM proc, SCM len);
#endif /*GUILE_DEBUG*/

#endif  /* SCM_PROCS_H */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
