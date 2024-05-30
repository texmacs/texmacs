/* Copyright (C) 1995,1996,1998,1999,2000,2001,2002, 2003, 2005, 2006 Free Software Foundation, Inc.
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libguile/unif.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/read.h"
#include "libguile/root.h"
#include "libguile/strings.h"
#include "libguile/modules.h"
#include "libguile/validate.h"
#include "libguile/deprecation.h"

#include "libguile/strports.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif



/* {Ports - string ports}
 *
 */

/* NOTES:

   We break the rules set forth by strings.h about accessing the
   internals of strings here.  We can do this since we can guarantee
   that the string used as pt->stream is not in use by anyone else.
   Thus, it's representation will not change asynchronously.

   (Ports aren't thread-safe yet anyway...)

   write_buf/write_end point to the ends of the allocated string.
   read_buf/read_end in principle point to the part of the string which
   has been written to, but this is only updated after a flush.
   read_pos and write_pos in principle should be equal, but this is only true
   when rw_active is SCM_PORT_NEITHER.

   ENHANCE-ME - output blocks:

   The current code keeps an output string as a single block.  That means
   when the size is increased the entire old contents must be copied.  It'd
   be more efficient to begin a new block when the old one is full, so
   there's no re-copying of previous data.

   To make seeking efficient, keeping the pieces in a vector might be best,
   though appending is probably the most common operation.  The size of each
   block could be progressively increased, so the bigger the string the
   bigger the blocks.

   When `get-output-string' is called the blocks have to be coalesced into a
   string, the result could be kept as a single big block.  If blocks were
   strings then `get-output-string' could notice when there's just one and
   return that with a copy-on-write (though repeated calls to
   `get-output-string' are probably unlikely).

   Another possibility would be to extend the port mechanism to let SCM
   strings come through directly from `display' and friends.  That way if a
   big string is written it can be kept as a copy-on-write, saving time
   copying and maybe saving some space.  */


scm_t_bits scm_tc16_strport;


static int
stfill_buffer (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  
  if (pt->read_pos >= pt->read_end)
    return EOF;
  else
    return scm_return_first_int (*pt->read_pos, port);
}

/* change the size of a port's string to new_size.  this doesn't
   change read_buf_size.  */
static void 
st_resize_port (scm_t_port *pt, off_t new_size)
{
  SCM old_stream = SCM_PACK (pt->stream);
  const char *src = scm_i_string_chars (old_stream);
  char *dst;
  SCM new_stream = scm_i_make_string (new_size, &dst);
  unsigned long int old_size = scm_i_string_length (old_stream);
  unsigned long int min_size = min (old_size, new_size);
  unsigned long int i;

  off_t index = pt->write_pos - pt->write_buf;

  pt->write_buf_size = new_size;

  for (i = 0; i != min_size; ++i)
    dst[i] = src[i];

  scm_remember_upto_here_1 (old_stream);

  /* reset buffer. */
  {
    pt->stream = SCM_UNPACK (new_stream);
    pt->read_buf = pt->write_buf = (unsigned char *)dst;
    pt->read_pos = pt->write_pos = pt->write_buf + index;
    pt->write_end = pt->write_buf + pt->write_buf_size;
    pt->read_end = pt->read_buf + pt->read_buf_size;
  }
}

/* amount by which write_buf is expanded.  */
#define SCM_WRITE_BLOCK 80

/* ensure that write_pos < write_end by enlarging the buffer when
   necessary.  update read_buf to account for written chars.

   The buffer is enlarged by 1.5 times, plus SCM_WRITE_BLOCK.  Adding just a
   fixed amount is no good, because there's a block copy for each increment,
   and that copying would take quadratic time.  In the past it was found to
   be very slow just adding 80 bytes each time (eg. about 10 seconds for
   writing a 100kbyte string).  */

static void
st_flush (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (pt->write_pos == pt->write_end)
    {
      st_resize_port (pt, pt->write_buf_size * 3 / 2 + SCM_WRITE_BLOCK);
    }
  pt->read_pos = pt->write_pos;
  if (pt->read_pos > pt->read_end)
    {
      pt->read_end = (unsigned char *) pt->read_pos;
      pt->read_buf_size = pt->read_end - pt->read_buf;
    }
  pt->rw_active = SCM_PORT_NEITHER;
}

static void
st_write (SCM port, const void *data, size_t size)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  const char *input = (char *) data;

  while (size > 0)
    {
      int space = pt->write_end - pt->write_pos;
      int write_len = (size > space) ? space : size;
      
      memcpy ((char *) pt->write_pos, input, write_len);
      pt->write_pos += write_len;
      size -= write_len;
      input += write_len;
      if (write_len == space)
	st_flush (port);
    }
}

static void
st_end_input (SCM port, int offset)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  
  if (pt->read_pos - pt->read_buf < offset)
    scm_misc_error ("st_end_input", "negative position", SCM_EOL);

  pt->write_pos = (unsigned char *) (pt->read_pos = pt->read_pos - offset);
  pt->rw_active = SCM_PORT_NEITHER;
}

static off_t
st_seek (SCM port, off_t offset, int whence)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  off_t target;

  if (pt->rw_active == SCM_PORT_READ && offset == 0 && whence == SEEK_CUR)
    /* special case to avoid disturbing the unread-char buffer.  */
    {
      if (pt->read_buf == pt->putback_buf)
	{
	  target = pt->saved_read_pos - pt->saved_read_buf
	    - (pt->read_end - pt->read_pos);
	}
      else
	{
	  target = pt->read_pos - pt->read_buf;
	}
    }
  else
    /* all other cases.  */
    {
      if (pt->rw_active == SCM_PORT_WRITE)
	st_flush (port);
  
      if (pt->rw_active == SCM_PORT_READ)
	scm_end_input (port);

      switch (whence)
	{
	case SEEK_CUR:
	  target = pt->read_pos - pt->read_buf + offset;
	  break;
	case SEEK_END:
	  target = pt->read_end - pt->read_buf + offset;
	  break;
	default: /* SEEK_SET */
	  target = offset;
	  break;
	}

      if (target < 0)
	scm_misc_error ("st_seek", "negative offset", SCM_EOL);
  
      if (target >= pt->write_buf_size)
	{
	  if (!(SCM_CELL_WORD_0 (port) & SCM_WRTNG))
	    {
	      if (target > pt->write_buf_size)
		{
		  scm_misc_error ("st_seek", 
				  "seek past end of read-only strport",
				  SCM_EOL);
		}
	    }
	  else
	    {
	      st_resize_port (pt, target + (target == pt->write_buf_size
					    ? SCM_WRITE_BLOCK
					    : 0));
	    }
	}
      pt->read_pos = pt->write_pos = pt->read_buf + target;
      if (pt->read_pos > pt->read_end)
	{
	  pt->read_end = (unsigned char *) pt->read_pos;
	  pt->read_buf_size = pt->read_end - pt->read_buf;
	}
    }
  return target;
}

static void
st_truncate (SCM port, off_t length)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);

  if (length > pt->write_buf_size)
    st_resize_port (pt, length);

  pt->read_buf_size = length;
  pt->read_end = pt->read_buf + length;
  if (pt->read_pos > pt->read_end)
    pt->read_pos = pt->read_end;
  
  if (pt->write_pos > pt->read_end)
    pt->write_pos = pt->read_end;
}

SCM 
scm_mkstrport (SCM pos, SCM str, long modes, const char *caller)
{
  SCM z;
  scm_t_port *pt;
  size_t str_len, c_pos;

  SCM_ASSERT (scm_is_string (str), str, SCM_ARG1, caller);

  str_len = scm_i_string_length (str);
  c_pos = scm_to_unsigned_integer (pos, 0, str_len);

  if (!((modes & SCM_WRTNG) || (modes & SCM_RDNG)))
    scm_misc_error ("scm_mkstrport", "port must read or write", SCM_EOL);

  /* XXX
 
     Make a new string to isolate us from changes to the original.
     This is done so that we can rely on scm_i_string_chars to stay in
     place even across SCM_TICKs.

     Additionally, when we are going to write to the string, we make a
     copy so that we can write to it without having to use
     scm_i_string_writable_chars.
  */

  if (modes & SCM_WRTNG)
    str = scm_c_substring_copy (str, 0, str_len);
  else
    str = scm_c_substring (str, 0, str_len);

  scm_i_scm_pthread_mutex_lock (&scm_i_port_table_mutex);
  z = scm_new_port_table_entry (scm_tc16_strport);
  pt = SCM_PTAB_ENTRY(z);
  SCM_SETSTREAM (z, SCM_UNPACK (str));
  SCM_SET_CELL_TYPE(z, scm_tc16_strport|modes);
  /* see above why we can use scm_i_string_chars here. */
  pt->write_buf = pt->read_buf = (unsigned char *) scm_i_string_chars (str);
  pt->read_pos = pt->write_pos = pt->read_buf + c_pos;
  pt->write_buf_size = pt->read_buf_size = str_len;
  pt->write_end = pt->read_end = pt->read_buf + pt->read_buf_size;

  pt->rw_random = 1;

  scm_i_pthread_mutex_unlock (&scm_i_port_table_mutex);

  /* ensure write_pos is writable. */
  if ((modes & SCM_WRTNG) && pt->write_pos == pt->write_end)
    st_flush (z);
  return z;
}

/* create a new string from a string port's buffer.  */
SCM scm_strport_to_string (SCM port)
{
  scm_t_port *pt = SCM_PTAB_ENTRY (port);
  SCM str;
  char *dst;
  
  if (pt->rw_active == SCM_PORT_WRITE)
    st_flush (port);

  str = scm_i_make_string (pt->read_buf_size, &dst);
  memcpy (dst, (char *) pt->read_buf, pt->read_buf_size);
  scm_remember_upto_here_1 (port);
  return str;
}

SCM_DEFINE (scm_object_to_string, "object->string", 1, 1, 0,
	    (SCM obj, SCM printer),
	    "Return a Scheme string obtained by printing @var{obj}.\n"
	    "Printing function can be specified by the optional second\n"
	    "argument @var{printer} (default: @code{write}).")
#define FUNC_NAME s_scm_object_to_string
{
  SCM str, port;

  if (!SCM_UNBNDP (printer))
    SCM_VALIDATE_PROC (2, printer);

  str = scm_c_make_string (0, SCM_UNDEFINED);
  port = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_WRTNG, FUNC_NAME);

  if (SCM_UNBNDP (printer))
    scm_write (obj, port);
  else
    scm_call_2 (printer, obj, port);

  return scm_strport_to_string (port);
}
#undef FUNC_NAME

SCM_DEFINE (scm_call_with_output_string, "call-with-output-string", 1, 0, 0, 
           (SCM proc),
	    "Calls the one-argument procedure @var{proc} with a newly created output\n"
	    "port.  When the function returns, the string composed of the characters\n"
	    "written into the port is returned.")
#define FUNC_NAME s_scm_call_with_output_string
{
  SCM p;

  p = scm_mkstrport (SCM_INUM0, 
		     scm_make_string (SCM_INUM0, SCM_UNDEFINED),
		     SCM_OPN | SCM_WRTNG,
                     FUNC_NAME);
  scm_call_1 (proc, p);

  return scm_get_output_string (p);
}
#undef FUNC_NAME

SCM_DEFINE (scm_call_with_input_string, "call-with-input-string", 2, 0, 0,
           (SCM string, SCM proc),
	    "Calls the one-argument procedure @var{proc} with a newly\n"
	    "created input port from which @var{string}'s contents may be\n"
	    "read.  The value yielded by the @var{proc} is returned.")
#define FUNC_NAME s_scm_call_with_input_string
{
  SCM p = scm_mkstrport(SCM_INUM0, string, SCM_OPN | SCM_RDNG, FUNC_NAME);
  return scm_call_1 (proc, p);
}
#undef FUNC_NAME

SCM_DEFINE (scm_open_input_string, "open-input-string", 1, 0, 0,
	    (SCM str),
	    "Take a string and return an input port that delivers characters\n"
	    "from the string. The port can be closed by\n"
	    "@code{close-input-port}, though its storage will be reclaimed\n"
	    "by the garbage collector if it becomes inaccessible.")
#define FUNC_NAME s_scm_open_input_string
{
  SCM p = scm_mkstrport(SCM_INUM0, str, SCM_OPN | SCM_RDNG, FUNC_NAME);
  return p;
}
#undef FUNC_NAME

SCM_DEFINE (scm_open_output_string, "open-output-string", 0, 0, 0, 
	    (void),
	    "Return an output port that will accumulate characters for\n"
	    "retrieval by @code{get-output-string}. The port can be closed\n"
	    "by the procedure @code{close-output-port}, though its storage\n"
	    "will be reclaimed by the garbage collector if it becomes\n"
	    "inaccessible.")
#define FUNC_NAME s_scm_open_output_string
{
  SCM p;

  p = scm_mkstrport (SCM_INUM0, 
		     scm_make_string (SCM_INUM0, SCM_UNDEFINED),
		     SCM_OPN | SCM_WRTNG,
                     FUNC_NAME);
  return p;
}
#undef FUNC_NAME

SCM_DEFINE (scm_get_output_string, "get-output-string", 1, 0, 0, 
	    (SCM port),
	    "Given an output port created by @code{open-output-string},\n"
	    "return a string consisting of the characters that have been\n"
	    "output to the port so far.")
#define FUNC_NAME s_scm_get_output_string
{
  SCM_VALIDATE_OPOUTSTRPORT (1, port);
  return scm_strport_to_string (port);
}
#undef FUNC_NAME


/* Given a null-terminated string EXPR containing a Scheme expression
   read it, and return it as an SCM value. */
SCM
scm_c_read_string (const char *expr)
{
  SCM port = scm_mkstrport (SCM_INUM0,
			    scm_from_locale_string (expr),
			    SCM_OPN | SCM_RDNG,
			    "scm_c_read_string");
  SCM form;

  /* Read expressions from that port; ignore the values.  */
  form = scm_read (port);

  scm_close_port (port);
  return form;
}

/* Given a null-terminated string EXPR containing Scheme program text,
   evaluate it, and return the result of the last expression evaluated.  */
SCM
scm_c_eval_string (const char *expr)
{
  return scm_eval_string (scm_from_locale_string (expr));
}

SCM
scm_c_eval_string_in_module (const char *expr, SCM module)
{
  return scm_eval_string_in_module (scm_from_locale_string (expr), module);
}


static SCM
inner_eval_string (void *data)
{
  SCM port = (SCM)data;
  SCM form;
  SCM ans = SCM_UNSPECIFIED;

  /* Read expressions from that port; ignore the values.  */
  while (!SCM_EOF_OBJECT_P (form = scm_read (port)))
    ans = scm_primitive_eval_x (form);

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early.  */

  return ans;
}

SCM_DEFINE (scm_eval_string_in_module, "eval-string", 1, 1, 0, 
            (SCM string, SCM module),
	    "Evaluate @var{string} as the text representation of a Scheme\n"
	    "form or forms, and return whatever value they produce.\n"
	    "Evaluation takes place in the given module, or the current\n"
            "module when no module is given.\n"
            "While the code is evaluated, the given module is made the\n"
	    "current one.  The current module is restored when this\n"
            "procedure returns.")
#define FUNC_NAME s_scm_eval_string_in_module
{
  SCM port = scm_mkstrport (SCM_INUM0, string, SCM_OPN | SCM_RDNG,
			    FUNC_NAME);
  if (SCM_UNBNDP (module))
    module = scm_current_module ();
  else
    SCM_VALIDATE_MODULE (2, module);
  return scm_c_call_with_current_module (module,
					 inner_eval_string, (void *)port);
}
#undef FUNC_NAME

SCM
scm_eval_string (SCM string)
{
  return scm_eval_string_in_module (string, SCM_UNDEFINED);
}

static scm_t_bits
scm_make_stptob ()
{
  scm_t_bits tc = scm_make_port_type ("string", stfill_buffer, st_write);

  scm_set_port_mark        (tc, scm_markstream);
  scm_set_port_end_input   (tc, st_end_input);
  scm_set_port_flush       (tc, st_flush);
  scm_set_port_seek        (tc, st_seek);
  scm_set_port_truncate    (tc, st_truncate);

  return tc;
}

void
scm_init_strports ()
{
  scm_tc16_strport = scm_make_stptob ();

#include "libguile/strports.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
