/* Copyright (C) 1995,1996,1998,1999,2000,2001, 2002, 2003, 2006 Free Software Foundation, Inc.
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

#include <stdio.h>
#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/eval.h"
#include "libguile/chars.h"
#include "libguile/fports.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"

#include "libguile/validate.h"
#include "libguile/vports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - soft ports}
 * 
 */


static scm_t_bits scm_tc16_sfport;


static void
sf_flush (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  SCM stream = SCM_PACK (pt->stream);

  if (pt->write_pos > pt->write_buf)
    {
      /* write the byte. */
      scm_call_1 (SCM_SIMPLE_VECTOR_REF (stream, 0),
		  SCM_MAKE_CHAR (*pt->write_buf));
      pt->write_pos = pt->write_buf;
  
      /* flush the output.  */
      {
	SCM f = SCM_SIMPLE_VECTOR_REF (stream, 2);

	if (scm_is_true (f))
	  scm_call_0 (f);
      }
    }
}

static void
sf_write (SCM port, const void *data, size_t size)
{
  SCM p = SCM_PACK (SCM_STREAM (port));

  scm_call_1 (SCM_SIMPLE_VECTOR_REF (p, 1),
	      scm_from_locale_stringn ((char *) data, size));
}

/* calling the flush proc (element 2) is in case old code needs it,
   but perhaps softports could the use port buffer in the same way as
   fports.  */

/* places a single char in the input buffer.  */
static int 
sf_fill_input (SCM port)
{
  SCM p = SCM_PACK (SCM_STREAM (port));
  SCM ans;

  ans = scm_call_0 (SCM_SIMPLE_VECTOR_REF (p, 3)); /* get char.  */
  if (scm_is_false (ans) || SCM_EOF_OBJECT_P (ans))
    return EOF;
  SCM_ASSERT (SCM_CHARP (ans), ans, SCM_ARG1, "sf_fill_input");
  {
    scm_t_port *pt = SCM_PTAB_ENTRY (port);    

    *pt->read_buf = SCM_CHAR (ans);
    pt->read_pos = pt->read_buf;
    pt->read_end = pt->read_buf + 1;
    return *pt->read_buf;
  }
}


static int 
sf_close (SCM port)
{
  SCM p = SCM_PACK (SCM_STREAM (port));
  SCM f = SCM_SIMPLE_VECTOR_REF (p, 4);
  if (scm_is_false (f))
    return 0;
  f = scm_call_0 (f);
  errno = 0;
  return scm_is_false (f) ? EOF : 0;
}


static int 
sf_input_waiting (SCM port)
{
  SCM p = SCM_PACK (SCM_STREAM (port));
  if (SCM_SIMPLE_VECTOR_LENGTH (p) >= 6)
    {
      SCM f = SCM_SIMPLE_VECTOR_REF (p, 5);
      if (scm_is_true (f))
	return scm_to_int (scm_call_0 (f));
    }
  /* Default is such that char-ready? for soft ports returns #t, as it
     did before this extension was implemented. */
  return 1;
}



SCM_DEFINE (scm_make_soft_port, "make-soft-port", 2, 0, 0,
           (SCM pv, SCM modes),
	    "Return a port capable of receiving or delivering characters as\n"
	    "specified by the @var{modes} string (@pxref{File Ports,\n"
	    "open-file}).  @var{pv} must be a vector of length 5 or 6.  Its\n"
	    "components are as follows:\n"
	    "\n"
	    "@enumerate 0\n"
	    "@item\n"
	    "procedure accepting one character for output\n"
	    "@item\n"
	    "procedure accepting a string for output\n"
	    "@item\n"
	    "thunk for flushing output\n"
	    "@item\n"
	    "thunk for getting one character\n"
	    "@item\n"
	    "thunk for closing port (not by garbage collection)\n"
	    "@item\n"
	    "(if present and not @code{#f}) thunk for computing the number of\n"
	    "characters that can be read from the port without blocking.\n"
	    "@end enumerate\n"
	    "\n"
	    "For an output-only port only elements 0, 1, 2, and 4 need be\n"
	    "procedures.  For an input-only port only elements 3 and 4 need\n"
	    "be procedures.  Thunks 2 and 4 can instead be @code{#f} if\n"
	    "there is no useful operation for them to perform.\n"
	    "\n"
	    "If thunk 3 returns @code{#f} or an @code{eof-object}\n"
	    "(@pxref{Input, eof-object?, ,r5rs, The Revised^5 Report on\n"
	    "Scheme}) it indicates that the port has reached end-of-file.\n"
	    "For example:\n"
	    "\n"
	    "@lisp\n"
	    "(define stdout (current-output-port))\n"
	    "(define p (make-soft-port\n"
	    "           (vector\n"
	    "            (lambda (c) (write c stdout))\n"
	    "            (lambda (s) (display s stdout))\n"
	    "            (lambda () (display \".\" stdout))\n"
	    "            (lambda () (char-upcase (read-char)))\n"
	    "            (lambda () (display \"@@\" stdout)))\n"
	    "           \"rw\"))\n"
	    "\n"
	    "(write p p) @result{} #<input-output: soft 8081e20>\n"
	    "@end lisp")
#define FUNC_NAME s_scm_make_soft_port
{
  int vlen;
  scm_t_port *pt;
  SCM z;

  SCM_VALIDATE_VECTOR (1, pv);
  vlen = SCM_SIMPLE_VECTOR_LENGTH (pv);
  SCM_ASSERT ((vlen == 5) || (vlen == 6), pv, 1, FUNC_NAME);
  SCM_VALIDATE_STRING (2, modes);
  
  scm_i_scm_pthread_mutex_lock (&scm_i_port_table_mutex);
  z = scm_new_port_table_entry (scm_tc16_sfport);
  pt = SCM_PTAB_ENTRY (z);
  scm_port_non_buffer (pt);
  SCM_SET_CELL_TYPE (z, scm_tc16_sfport | scm_i_mode_bits (modes));

  SCM_SETSTREAM (z, SCM_UNPACK (pv));
  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);
  return z;
}
#undef FUNC_NAME


static scm_t_bits
scm_make_sfptob ()
{
  scm_t_bits tc = scm_make_port_type ("soft", sf_fill_input, sf_write);

  scm_set_port_mark (tc, scm_markstream);
  scm_set_port_flush (tc, sf_flush);
  scm_set_port_close (tc, sf_close);
  scm_set_port_input_waiting (tc, sf_input_waiting);

  return tc;
}

void
scm_init_vports ()
{
  scm_tc16_sfport = scm_make_sfptob ();

#include "libguile/vports.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
