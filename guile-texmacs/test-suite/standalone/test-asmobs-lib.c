/* Copyright (C) 1999,2000,2001,2003, 2006, 2008 Free Software Foundation, Inc.
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

#ifndef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

long asmob000;
long asmob100;
long asmob010;
long asmob001;
long asmob200;
long asmob110;
long asmob020;
long asmob101;
long asmob011;
long asmob300;
long asmob210;
long asmob120;
long asmob030;
long asmob201;
long asmob021;
long asmob111;

/* since we don't have SCM_DEFINE_STATIC or similar */
SCM scm_make_asmob000 (void);
SCM scm_make_asmob001 (void);
SCM scm_make_asmob010 (void);
SCM scm_make_asmob011 (void);
SCM scm_make_asmob100 (void);
SCM scm_make_asmob101 (void);
SCM scm_make_asmob110 (void);
SCM scm_make_asmob111 (void);
SCM scm_make_asmob120 (void);
SCM scm_make_asmob020 (void);
SCM scm_make_asmob021 (void);
SCM scm_make_asmob200 (void);
SCM scm_make_asmob201 (void);
SCM scm_make_asmob210 (void);
SCM scm_make_asmob030 (void);
SCM scm_make_asmob300 (void);


SCM_DEFINE (scm_make_asmob000, "make-asmob000", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob000, 0);
}

SCM_DEFINE (scm_make_asmob100, "make-asmob100", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob100, 0);
}

SCM_DEFINE (scm_make_asmob010, "make-asmob010", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob010, 0);
}

SCM_DEFINE (scm_make_asmob001, "make-asmob001", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob001, 0);
}

SCM_DEFINE (scm_make_asmob200, "make-asmob200", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob200, 0);
}

SCM_DEFINE (scm_make_asmob110, "make-asmob110", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob110, 0);
}

SCM_DEFINE (scm_make_asmob020, "make-asmob020", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob020, 0);
}

SCM_DEFINE (scm_make_asmob101, "make-asmob101", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob101, 0);
}

SCM_DEFINE (scm_make_asmob011, "make-asmob011", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob011, 0);
}

SCM_DEFINE (scm_make_asmob300, "make-asmob300", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob300, 0);
}

SCM_DEFINE (scm_make_asmob210, "make-asmob210", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob210, 0);
}

SCM_DEFINE (scm_make_asmob120, "make-asmob120", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob120, 0);
}

SCM_DEFINE (scm_make_asmob030, "make-asmob030", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob030, 0);
}

SCM_DEFINE (scm_make_asmob201, "make-asmob201", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob201, 0);
}

SCM_DEFINE (scm_make_asmob021, "make-asmob021", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob021, 0);
}

SCM_DEFINE (scm_make_asmob111, "make-asmob111", 0, 0, 0, (), "")
{
  SCM_RETURN_NEWSMOB (asmob111, 0);
}

static SCM
apply0 (SCM smob)
{
  return SCM_EOL;
}

static SCM
apply1 (SCM smob, SCM a1)
{
  if (SCM_UNBNDP (a1)) a1 = SCM_BOOL_F;
  return scm_list_1 (a1);
}

static SCM
apply2 (SCM smob, SCM a1, SCM a2)
{
  if (SCM_UNBNDP (a1)) a1 = SCM_BOOL_F;
  if (SCM_UNBNDP (a2)) a2 = SCM_BOOL_F;
  return scm_list_2 (a1, a2);
}

static SCM
apply3 (SCM smob, SCM a1, SCM a2, SCM rest)
{
  if (SCM_UNBNDP (a1)) a1 = SCM_BOOL_F;
  if (SCM_UNBNDP (a2)) a2 = SCM_BOOL_F;
  if (SCM_UNBNDP (rest)) rest = SCM_BOOL_F;
  return scm_list_3 (a1, a2, rest);
}

void libtest_asmobs_init (void);

void
libtest_asmobs_init ()
{
  asmob000 = scm_make_smob_type ("asmob000", 0);
  scm_set_smob_apply (asmob000, apply0, 0, 0, 0);
  asmob100 = scm_make_smob_type ("asmob100", 0);
  scm_set_smob_apply (asmob100, apply1, 1, 0, 0);
  asmob010 = scm_make_smob_type ("asmob010", 0);
  scm_set_smob_apply (asmob010, apply1, 0, 1, 0);
  asmob001 = scm_make_smob_type ("asmob001", 0);
  scm_set_smob_apply (asmob001, apply1, 0, 0, 1);
  asmob200 = scm_make_smob_type ("asmob200", 0);
  scm_set_smob_apply (asmob200, apply2, 2, 0, 0);
  asmob110 = scm_make_smob_type ("asmob110", 0);
  scm_set_smob_apply (asmob110, apply2, 1, 1, 0);
  asmob020 = scm_make_smob_type ("asmob020", 0);
  scm_set_smob_apply (asmob020, apply2, 0, 2, 0);
  asmob101 = scm_make_smob_type ("asmob101", 0);
  scm_set_smob_apply (asmob101, apply2, 1, 0, 1);
  asmob011 = scm_make_smob_type ("asmob011", 0);
  scm_set_smob_apply (asmob011, apply2, 0, 1, 1);
  asmob300 = scm_make_smob_type ("asmob300", 0);
  scm_set_smob_apply (asmob300, apply3, 3, 0, 0);
  asmob210 = scm_make_smob_type ("asmob210", 0);
  scm_set_smob_apply (asmob210, apply3, 2, 1, 0);
  asmob120 = scm_make_smob_type ("asmob120", 0);
  scm_set_smob_apply (asmob120, apply3, 1, 2, 0);
  asmob030 = scm_make_smob_type ("asmob030", 0);
  scm_set_smob_apply (asmob030, apply3, 0, 3, 0);
  asmob201 = scm_make_smob_type ("asmob201", 0);
  scm_set_smob_apply (asmob201, apply3, 2, 0, 1);
  asmob021 = scm_make_smob_type ("asmob021", 0);
  scm_set_smob_apply (asmob021, apply3, 0, 2, 1);
  asmob111 = scm_make_smob_type ("asmob111", 0);
  scm_set_smob_apply (asmob111, apply3, 1, 1, 1);
# include "test-asmobs-lib.x"
}
