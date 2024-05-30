/* GDB interface for Guile
 * Copyright (C) 1996,1997,1999,2000,2001,2002,2004
 * Free Software Foundation, Inc.
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "libguile/_scm.h"

#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libguile/strports.h"
#include "libguile/read.h"
#include "libguile/eval.h"
#include "libguile/chars.h"
#include "libguile/modules.h"
#include "libguile/ports.h"
#include "libguile/fluids.h"
#include "libguile/strings.h"
#include "libguile/init.h"

#include "libguile/gdbint.h"

/* {Support for debugging with gdb}
 *
 * TODO:
 *
 * 1. Redirect outputs
 * 2. Catch errors
 * 3. Prevent print from causing segmentation fault when given broken pairs
 */

#define GDB_TYPE SCM

#include "libguile/gdb_interface.h"



/* Be carefull when this macro is true.
   scm_gc_running_p is set during gc.
 */
#define SCM_GC_P (scm_gc_running_p)

/* Macros that encapsulate blocks of code which can be called by the
 * debugger.
 */
#define SCM_BEGIN_FOREIGN_BLOCK \
do { \
  scm_print_carefully_p = 1; \
} while (0)


#define SCM_END_FOREIGN_BLOCK \
do { \
  scm_print_carefully_p = 0; \
} while (0)


#define RESET_STRING { gdb_output_length = 0; }

#define SEND_STRING(str) \
do { \
  gdb_output = (char *) (str); \
  gdb_output_length = strlen ((const char *) (str)); \
} while (0)


/* {Gdb interface}
 */

unsigned short gdb_options = GDB_HAVE_BINDINGS;

char *gdb_language = "lisp/c";

SCM gdb_result;

char *gdb_output;

int gdb_output_length;

int scm_print_carefully_p;

static SCM gdb_input_port;
static int port_mark_p, stream_mark_p, string_mark_p;

static SCM gdb_output_port;


static void
unmark_port (SCM port)
{
  SCM stream, string;
  port_mark_p = SCM_GC_MARK_P (port);
  SCM_CLEAR_GC_MARK (port);
  stream = SCM_PACK (SCM_STREAM (port));
  stream_mark_p = SCM_GC_MARK_P (stream);
  SCM_CLEAR_GC_MARK (stream);
  string = SCM_CDR (stream);
  string_mark_p = SCM_GC_MARK_P (string);
  SCM_CLEAR_GC_MARK (string);
}


static void
remark_port (SCM port)
{
  SCM stream = SCM_PACK (SCM_STREAM (port));
  SCM string = SCM_CDR (stream);
  if (string_mark_p)
    SCM_SET_GC_MARK (string);
  if (stream_mark_p)
    SCM_SET_GC_MARK (stream);
  if (port_mark_p)
    SCM_SET_GC_MARK (port);
}


int
gdb_maybe_valid_type_p (SCM value)
{
  return SCM_IMP (value) || scm_in_heap_p (value);
}


int
gdb_read (char *str)
{
  SCM ans;
  int status = 0;
  RESET_STRING;
  /* Need to be restrictive about what to read? */
  if (SCM_GC_P)
    {
      char *p;
      for (p = str; *p != '\0'; ++p)
	switch (*p)
	  {
	  case '(':
	  case '\'':
	  case '"':
	    SEND_STRING ("Can't read this kind of expressions during gc");
	    return -1;
	  case '#':
	    if (*++p == '\0')
	      goto premature;
	    if (*p == '\\')
	      {
		if (*++p != '\0')
		  continue;
	      premature:
		SEND_STRING ("Premature end of lisp expression");
		return -1;
	      }
	  default:
	    continue;
	  }
    }
  SCM_BEGIN_FOREIGN_BLOCK;
  unmark_port (gdb_input_port);
  scm_seek (gdb_input_port, SCM_INUM0, scm_from_int (SEEK_SET));
  scm_puts (str, gdb_input_port);
  scm_truncate_file (gdb_input_port, SCM_UNDEFINED);
  scm_seek (gdb_input_port, SCM_INUM0, scm_from_int (SEEK_SET));

  /* Read one object */
  ans = scm_read (gdb_input_port);
  if (SCM_GC_P)
    {
      if (SCM_NIMP (ans))
	{
	  SEND_STRING ("Non-immediate created during gc.  Memory may be trashed.");
	  status = -1;
	  goto exit;
	}
    }
  gdb_result = ans;
  /* Protect answer from future GC */
  if (SCM_NIMP (ans))
    scm_permanent_object (ans);
exit:
  remark_port (gdb_input_port);
  SCM_END_FOREIGN_BLOCK;
  return status;
}


int
gdb_eval (SCM exp)
{
  RESET_STRING;
  if (SCM_GC_P)
    {
      SEND_STRING ("Can't evaluate lisp expressions during gc");
      return -1;
    }
  SCM_BEGIN_FOREIGN_BLOCK;
  {
    SCM env = scm_top_level_env (SCM_TOP_LEVEL_LOOKUP_CLOSURE);
    gdb_result = scm_permanent_object (scm_i_eval_x (exp, env));
  }
  SCM_END_FOREIGN_BLOCK;
  return 0;
}


int
gdb_print (SCM obj)
{
  if (!scm_initialized_p)
    SEND_STRING ("*** Guile not initialized ***");
  else
    {
      RESET_STRING;
      SCM_BEGIN_FOREIGN_BLOCK;
      /* Reset stream */
      scm_seek (gdb_output_port, SCM_INUM0, scm_from_int (SEEK_SET));
      scm_write (obj, gdb_output_port);
      scm_truncate_file (gdb_output_port, SCM_UNDEFINED);
      {
	scm_t_port *pt = SCM_PTAB_ENTRY (gdb_output_port);

	scm_flush (gdb_output_port);
	*(pt->write_buf + pt->read_buf_size) = 0;
	SEND_STRING (pt->read_buf);
      }
      SCM_END_FOREIGN_BLOCK;
    }
  return 0;
}


int
gdb_binding (SCM name, SCM value)
{
  RESET_STRING;
  if (SCM_GC_P)
    {
      SEND_STRING ("Can't create new bindings during gc");
      return -1;
    }
  SCM_BEGIN_FOREIGN_BLOCK;
  {
    SCM var = scm_sym2var (name, SCM_TOP_LEVEL_LOOKUP_CLOSURE, SCM_BOOL_T);
    SCM_VARIABLE_SET (var, value);
  }
  SCM_END_FOREIGN_BLOCK;
  return 0;
}

void
scm_init_gdbint ()
{
  static char *s = "scm_init_gdb_interface";
  SCM port;

  scm_print_carefully_p = 0;
  
  port = scm_mkstrport (SCM_INUM0,
			scm_c_make_string (0, SCM_UNDEFINED),
			SCM_OPN | SCM_WRTNG,
			s);
  gdb_output_port = scm_permanent_object (port);
  
  port = scm_mkstrport (SCM_INUM0,
			scm_c_make_string (0, SCM_UNDEFINED),
			SCM_OPN | SCM_RDNG | SCM_WRTNG,
			s);
  gdb_input_port = scm_permanent_object (port);
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
