/* "net_db.c" network database support
 * Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2006, 2009 Free Software Foundation, Inc.
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



/* Written in 1994 by Aubrey Jaffer.
 * Thanks to Hallvard.Tretteberg@si.sintef.no for inspiration and discussion.
 * Rewritten by Gary Houston to be a closer interface to the C socket library.
 * Split into net_db.c and socket.c.
 */


#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <errno.h>

#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/dynwind.h"

#include "libguile/validate.h"
#include "libguile/net_db.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#ifdef __MINGW32__
#include "win32-socket.h"
#endif

#if !defined (HAVE_H_ERRNO) && !defined (__MINGW32__) && !defined (__CYGWIN__)
/* h_errno not found in netdb.h, maybe this will help.  */
extern int h_errno;
#endif

#if defined HAVE_HSTRERROR && !HAVE_DECL_HSTRERROR	\
  && !defined __MINGW32__ && !defined __CYGWIN__
/* Some OSes, such as Tru64 5.1b, lack a declaration for hstrerror(3).  */
extern const char *hstrerror (int);
#endif



SCM_SYMBOL (scm_host_not_found_key, "host-not-found");
SCM_SYMBOL (scm_try_again_key, "try-again");
SCM_SYMBOL (scm_no_recovery_key, "no-recovery");
SCM_SYMBOL (scm_no_data_key, "no-data");

static void scm_resolv_error (const char *subr, SCM bad_value)
{
#ifdef NETDB_INTERNAL
  if (h_errno == NETDB_INTERNAL)
    {
      /* errno supposedly contains a useful value.  */
      scm_syserror (subr);
    }
  else
#endif
    {
      SCM key;
      const char *errmsg;

      switch (h_errno)
	{
	case HOST_NOT_FOUND:
	  key = scm_host_not_found_key;
	  errmsg = "Unknown host"; 
	  break;
	case TRY_AGAIN:	
	  key = scm_try_again_key;
	  errmsg = "Host name lookup failure";
	  break;
	case NO_RECOVERY:
	  key = scm_no_recovery_key;
	  errmsg = "Unknown server error"; 
	  break;
	case NO_DATA:
	  key = scm_no_data_key;
	  errmsg = "No address associated with name";
	  break;
	default:
	  scm_misc_error (subr, "Unknown resolver error", SCM_EOL);
	  errmsg = NULL;
	}

#ifdef HAVE_HSTRERROR
      errmsg = (const char *) hstrerror (h_errno);
#endif
      scm_error (key, subr, errmsg, SCM_BOOL_F, SCM_EOL);
    }
}

/* Should take an extra arg for address format (will be needed for IPv6).
   Should use reentrant facilities if available.
 */

SCM_DEFINE (scm_gethost, "gethost", 0, 1, 0, 
            (SCM host),
	    "@deffnx {Scheme Procedure} gethostbyname hostname\n"
	    "@deffnx {Scheme Procedure} gethostbyaddr address\n"
	    "Look up a host by name or address, returning a host object.  The\n"
	    "@code{gethost} procedure will accept either a string name or an integer\n"
	    "address; if given no arguments, it behaves like @code{gethostent} (see\n"
	    "below).  If a name or address is supplied but the address can not be\n"
	    "found, an error will be thrown to one of the keys:\n"
	    "@code{host-not-found}, @code{try-again}, @code{no-recovery} or\n"
	    "@code{no-data}, corresponding to the equivalent @code{h_error} values.\n"
	    "Unusual conditions may result in errors thrown to the\n"
	    "@code{system-error} or @code{misc_error} keys.")
#define FUNC_NAME s_scm_gethost
{
  SCM result = scm_c_make_vector (5, SCM_UNSPECIFIED);
  SCM lst = SCM_EOL;
  struct hostent *entry;
  struct in_addr inad;
  char **argv;
  int i = 0;

  if (SCM_UNBNDP (host))
    {
#ifdef HAVE_GETHOSTENT
      entry = gethostent ();
#else
      entry = NULL;
#endif
      if (! entry)
	{
	  /* As far as I can tell, there's no good way to tell whether
             zero means an error or end-of-file.  The trick of
             clearing errno before calling gethostent and checking it
             afterwards doesn't cut it, because, on Linux, it seems to
             try to contact some other server (YP?) and fails, which
             is a benign failure.  */
	  return SCM_BOOL_F;
	}
    }
  else if (scm_is_string (host))
    {
      char *str = scm_to_locale_string (host);
      entry = gethostbyname (str);
      free (str);
    }
  else
    {
      inad.s_addr = htonl (scm_to_ulong (host));
      entry = gethostbyaddr ((char *) &inad, sizeof (inad), AF_INET);
    }

  if (!entry)
    scm_resolv_error (FUNC_NAME, host);
  
  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->h_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->h_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_int (entry->h_addrtype));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_int (entry->h_length));
  if (sizeof (struct in_addr) != entry->h_length)
    {
      SCM_SIMPLE_VECTOR_SET(result, 4, SCM_BOOL_F);
      return result;
    }
  for (argv = entry->h_addr_list; argv[i]; i++);
  while (i--)
    {
      inad = *(struct in_addr *) argv[i];
      lst = scm_cons (scm_from_ulong (ntohl (inad.s_addr)), lst);
    }
  SCM_SIMPLE_VECTOR_SET(result, 4, lst);
  return result;
}
#undef FUNC_NAME


/* In all subsequent getMUMBLE functions, when we're called with no
   arguments, we're supposed to traverse the tables entry by entry.
   However, there doesn't seem to be any documented way to distinguish
   between end-of-table and an error; in both cases the functions
   return zero.  Gotta love Unix.  For the time being, we clear errno,
   and if we get a zero and errno is set, we signal an error.  This
   doesn't seem quite right (what if errno gets set as part of healthy
   operation?), but it seems to work okay.  We'll see.  */

#if defined(HAVE_GETNETENT) && defined(HAVE_GETNETBYNAME) && defined(HAVE_GETNETBYADDR)
SCM_DEFINE (scm_getnet, "getnet", 0, 1, 0, 
            (SCM net),
	    "@deffnx {Scheme Procedure} getnetbyname net-name\n"
	    "@deffnx {Scheme Procedure} getnetbyaddr net-number\n"
	    "Look up a network by name or net number in the network database.  The\n"
	    "@var{net-name} argument must be a string, and the @var{net-number}\n"
	    "argument must be an integer.  @code{getnet} will accept either type of\n"
	    "argument, behaving like @code{getnetent} (see below) if no arguments are\n"
	    "given.")
#define FUNC_NAME s_scm_getnet
{
  SCM result = scm_c_make_vector (4, SCM_UNSPECIFIED);
  struct netent *entry;
  int eno;

  if (SCM_UNBNDP (net))
    {
      entry = getnetent ();
      if (! entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
    }
  else if (scm_is_string (net))
    {
      char *str = scm_to_locale_string (net);
      entry = getnetbyname (str);
      eno = errno;
      free (str);
    }
  else
    {
      unsigned long netnum = scm_to_ulong (net);
      entry = getnetbyaddr (netnum, AF_INET);
      eno = errno;
    }

  if (!entry)
    SCM_SYSERROR_MSG ("no such network ~A", scm_list_1 (net), eno);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->n_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->n_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_int (entry->n_addrtype));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_ulong (entry->n_net));
  return result;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_GETPROTOENT) || defined (__MINGW32__)
SCM_DEFINE (scm_getproto, "getproto", 0, 1, 0, 
            (SCM protocol),
	    "@deffnx {Scheme Procedure} getprotobyname name\n"
	    "@deffnx {Scheme Procedure} getprotobynumber number\n"
	    "Look up a network protocol by name or by number.  @code{getprotobyname}\n"
	    "takes a string argument, and @code{getprotobynumber} takes an integer\n"
	    "argument.  @code{getproto} will accept either type, behaving like\n"
	    "@code{getprotoent} (see below) if no arguments are supplied.")
#define FUNC_NAME s_scm_getproto
{
  SCM result = scm_c_make_vector (3, SCM_UNSPECIFIED);
  struct protoent *entry;
  int eno;

  if (SCM_UNBNDP (protocol))
    {
      entry = getprotoent ();
      if (! entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
    }
  else if (scm_is_string (protocol))
    {
      char *str = scm_to_locale_string (protocol);
      entry = getprotobyname (str);
      eno = errno;
      free (str);
    }
  else
    {
      unsigned long protonum = scm_to_ulong (protocol);
      entry = getprotobynumber (protonum);
      eno = errno;
    }

  if (!entry)
    SCM_SYSERROR_MSG ("no such protocol ~A", scm_list_1 (protocol), eno);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->p_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->p_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_int (entry->p_proto));
  return result;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_GETSERVENT) || defined (__MINGW32__)
static SCM
scm_return_entry (struct servent *entry)
{
  SCM result = scm_c_make_vector (4, SCM_UNSPECIFIED);

  SCM_SIMPLE_VECTOR_SET(result, 0, scm_from_locale_string (entry->s_name));
  SCM_SIMPLE_VECTOR_SET(result, 1, scm_makfromstrs (-1, entry->s_aliases));
  SCM_SIMPLE_VECTOR_SET(result, 2, scm_from_uint16 (ntohs (entry->s_port)));
  SCM_SIMPLE_VECTOR_SET(result, 3, scm_from_locale_string (entry->s_proto));
  return result;
}

SCM_DEFINE (scm_getserv, "getserv", 0, 2, 0,
            (SCM name, SCM protocol),
	    "@deffnx {Scheme Procedure} getservbyname name protocol\n"
	    "@deffnx {Scheme Procedure} getservbyport port protocol\n"
	    "Look up a network service by name or by service number, and return a\n"
	    "network service object.  The @var{protocol} argument specifies the name\n"
	    "of the desired protocol; if the protocol found in the network service\n"
	    "database does not match this name, a system error is signalled.\n\n"
	    "The @code{getserv} procedure will take either a service name or number\n"
	    "as its first argument; if given no arguments, it behaves like\n"
	    "@code{getservent} (see below).")
#define FUNC_NAME s_scm_getserv
{
  struct servent *entry;
  char *protoname;
  int eno;

  if (SCM_UNBNDP (name))
    {
      entry = getservent ();
      if (!entry)
	{
	  /* There's no good way to tell whether zero means an error
             or end-of-file, so we always return #f.  See `gethost'
             for details. */
	  return SCM_BOOL_F;
	}
      return scm_return_entry (entry);
    }

  scm_dynwind_begin (0);

  protoname = scm_to_locale_string (protocol);
  scm_dynwind_free (protoname);

  if (scm_is_string (name))
    {
      char *str = scm_to_locale_string (name);
      entry = getservbyname (str, protoname);
      eno = errno;
      free (str);
    }
  else
    {
      entry = getservbyport (htons (scm_to_int (name)), protoname);
      eno = errno;
    }

  if (!entry)
    SCM_SYSERROR_MSG("no such service ~A", scm_list_1 (name), eno);

  scm_dynwind_end ();
  return scm_return_entry (entry);
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETHOSTENT) && defined(HAVE_ENDHOSTENT)
SCM_DEFINE (scm_sethost, "sethost", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endhostent}.\n"
	    "Otherwise it is equivalent to @code{sethostent stayopen}.")
#define FUNC_NAME s_scm_sethost
{
  if (SCM_UNBNDP (stayopen))
    endhostent ();
  else
    sethostent (scm_is_true (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined(HAVE_SETNETENT) && defined(HAVE_ENDNETENT) 
SCM_DEFINE (scm_setnet, "setnet", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endnetent}.\n"
	    "Otherwise it is equivalent to @code{setnetent stayopen}.")
#define FUNC_NAME s_scm_setnet
{
  if (SCM_UNBNDP (stayopen))
    endnetent ();
  else
    setnetent (scm_is_true (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_SETPROTOENT) && defined (HAVE_ENDPROTOENT) || defined (__MINGW32__)
SCM_DEFINE (scm_setproto, "setproto", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endprotoent}.\n"
	    "Otherwise it is equivalent to @code{setprotoent stayopen}.")
#define FUNC_NAME s_scm_setproto
{
  if (SCM_UNBNDP (stayopen))
    endprotoent ();
  else
    setprotoent (scm_is_true (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

#if defined (HAVE_SETSERVENT) && defined (HAVE_ENDSERVENT) || defined (__MINGW32__)
SCM_DEFINE (scm_setserv, "setserv", 0, 1, 0, 
            (SCM stayopen),
	    "If @var{stayopen} is omitted, this is equivalent to @code{endservent}.\n"
	    "Otherwise it is equivalent to @code{setservent stayopen}.")
#define FUNC_NAME s_scm_setserv
{
  if (SCM_UNBNDP (stayopen))
    endservent ();
  else
    setservent (scm_is_true (stayopen));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif


void 
scm_init_net_db ()
{
  scm_add_feature ("net-db");
#include "libguile/net_db.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
