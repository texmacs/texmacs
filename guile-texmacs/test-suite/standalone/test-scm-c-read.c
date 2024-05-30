/* Copyright (C) 2008 Free Software Foundation, Inc.
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

/* Exercise `scm_c_read ()' and the port type API.  Verify assumptions that
   can be made by port type implementations.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>
#include <assert.h>



/* Size of our port's internal buffer.  */
#define PORT_BUFFER_SIZE 1024

/* Return a new port of type PORT_TYPE.  */
static inline SCM
make_port (scm_t_bits port_type)
{
  SCM port;
  char *c_buffer;
  scm_t_port *c_port;

  c_buffer = scm_gc_calloc (PORT_BUFFER_SIZE, "custom-port-buffer");

  port = scm_new_port_table_entry (port_type);

  /* Associate C_BUFFER with PORT, for test purposes.  */
  SCM_SETSTREAM (port, (scm_t_bits) c_buffer);

  /* Use C_BUFFER as PORT's internal buffer.  */
  c_port = SCM_PTAB_ENTRY (port);
  c_port->read_pos = c_port->read_buf = (unsigned char *) c_buffer;
  c_port->read_end = (unsigned char *) c_buffer + PORT_BUFFER_SIZE;
  c_port->read_buf_size = PORT_BUFFER_SIZE;

  /* Mark PORT as open and readable.  */
  SCM_SET_CELL_TYPE (port, port_type | SCM_OPN | SCM_RDNG);

  return port;
}

/* Read one byte from PORT.  */
static int
fill_input (SCM port)
{
  int result;
  scm_t_port *c_port = SCM_PTAB_ENTRY (port);

  /* Make sure that C_PORT's internal buffer wasn't changed behind our back.
     See http://lists.gnu.org/archive/html/guile-devel/2008-11/msg00042.html
     for an example where this assumption matters.  */
  assert (c_port->read_buf == (unsigned char *) SCM_STREAM (port));
  assert (c_port->read_buf_size == PORT_BUFFER_SIZE);

  if (c_port->read_pos >= c_port->read_end)
    result = EOF;
  else
    result = (int) *c_port->read_pos++;

  return result;
}

/* Return true (non-zero) if BUF contains only zeros.  */
static inline int
zeroed_buffer_p (const char *buf, size_t len)
{
  size_t i;

  for (i = 0; i < len; i++)
    if (buf[i] != 0)
      return 0;

  return 1;
}

/* Run the test.  */
static void *
do_start (void *arg)
{
  SCM port;
  scm_t_bits port_type;
  char buffer[PORT_BUFFER_SIZE + (PORT_BUFFER_SIZE / 2)];
  size_t read, last_read;

  port_type = scm_make_port_type ("custom-input-port", fill_input, NULL);
  port = make_port (port_type);

  read = 0;
  do
    {
      last_read = scm_c_read (port, &buffer[read], 123);
      assert (last_read <= 123);
      assert (zeroed_buffer_p (&buffer[read], last_read));

      read += last_read;
    }
  while (last_read > 0 && read < sizeof (buffer));

  /* We shouldn't be able to read more than what's in PORT's buffer.  */
  assert (read == PORT_BUFFER_SIZE);

  return NULL;
}


int
main (int argc, char *argv[])
{
  scm_with_guile (do_start, NULL);

  return 0;
}
