/* Copyright (C) 2004, 2006, 2008 Free Software Foundation, Inc.
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
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/feature.h"
#include "libguile/i18n.h"
#include "libguile/strings.h"
#include "libguile/dynwind.h"

#include "gettext.h"
#include <locale.h>


int
scm_i_to_lc_category (SCM category, int allow_lc_all)
{
  int c_category = scm_to_int (category);
  switch (c_category)
    {
#ifdef LC_CTYPE
    case LC_CTYPE:
#endif
#ifdef LC_NUMERIC
    case LC_NUMERIC:
#endif
#ifdef LC_COLLATE
    case LC_COLLATE:
#endif
#ifdef LC_TIME
    case LC_TIME:
#endif
#ifdef LC_MONETARY
    case LC_MONETARY:
#endif
#ifdef LC_MESSAGES
    case LC_MESSAGES:
#endif
#ifdef LC_PAPER
    case LC_PAPER:
#endif
#ifdef LC_NAME
    case LC_NAME:
#endif
#ifdef LC_ADDRESS
    case LC_ADDRESS:
#endif
#ifdef LC_TELEPHONE
    case LC_TELEPHONE:
#endif
#ifdef LC_MEASUREMENT
    case LC_MEASUREMENT:
#endif
#ifdef LC_IDENTIFICATION
    case LC_IDENTIFICATION:
#endif
      return c_category;
#ifdef LC_ALL
    case LC_ALL:
      if (allow_lc_all)
	return c_category;
#endif  
    }
  scm_wrong_type_arg (0, 0, category);
}

SCM_DEFINE (scm_gettext, "gettext", 1, 2, 0,
	    (SCM msgid, SCM domain, SCM category),
	    "Return the translation of @var{msgid} in the message domain "
	    "@var{domain}. @var{domain} is optional and defaults to the "
	    "domain set through (textdomain).  @var{category} is optional "
	    "and defaults to LC_MESSAGES.")
#define FUNC_NAME s_scm_gettext
{
  char *c_msgid;
  char const *c_result;
  SCM result;

  scm_dynwind_begin (0);

  c_msgid = scm_to_locale_string (msgid);
  scm_dynwind_free (c_msgid);

  if (SCM_UNBNDP (domain))
    {
      /* 1 argument case.  */
      c_result = gettext (c_msgid);
    }
  else
    {
      char *c_domain;

      c_domain = scm_to_locale_string (domain);
      scm_dynwind_free (c_domain);

      if (SCM_UNBNDP (category))
	{
	  /* 2 argument case.  */
	  c_result = dgettext (c_domain, c_msgid);
	}
      else
	{
	  /* 3 argument case.  */
	  int c_category;

	  c_category = scm_i_to_lc_category (category, 0);
	  c_result = dcgettext (c_domain, c_msgid, c_category);
	}
    }

  if (c_result == c_msgid)
    result = msgid;
  else
    result = scm_from_locale_string (c_result);

  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_ngettext, "ngettext", 3, 2, 0,
	    (SCM msgid, SCM msgid_plural, SCM n, SCM domain, SCM category),
	    "Return the translation of @var{msgid}/@var{msgid_plural} in the "
	    "message domain @var{domain}, with the plural form being chosen "
	    "appropriately for the number @var{n}.  @var{domain} is optional "
	    "and defaults to the domain set through (textdomain). "
	    "@var{category} is optional and defaults to LC_MESSAGES.")
#define FUNC_NAME s_scm_ngettext
{
  char *c_msgid;
  char *c_msgid_plural;
  unsigned long c_n;
  const char *c_result;
  SCM result;

  scm_dynwind_begin (0);

  c_msgid = scm_to_locale_string (msgid);
  scm_dynwind_free (c_msgid);

  c_msgid_plural = scm_to_locale_string (msgid_plural);
  scm_dynwind_free (c_msgid_plural);

  c_n = scm_to_ulong (n);

  if (SCM_UNBNDP (domain))
    {
      /* 3 argument case.  */
      c_result = ngettext (c_msgid, c_msgid_plural, c_n);
    }
  else
    {
      char *c_domain;

      c_domain = scm_to_locale_string (domain);
      scm_dynwind_free (c_domain);

      if (SCM_UNBNDP (category))
	{
	  /* 4 argument case.  */
	  c_result = dngettext (c_domain, c_msgid, c_msgid_plural, c_n);
	}
      else
	{
	  /* 5 argument case.  */
	  int c_category;

	  c_category = scm_i_to_lc_category (category, 0);
	  c_result = dcngettext (c_domain, c_msgid, c_msgid_plural, c_n,
				 c_category);
	}
    }

  if (c_result == c_msgid)
    result = msgid;
  else if (c_result == c_msgid_plural)
    result = msgid_plural;
  else
    result = scm_from_locale_string (c_result);
  
  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_textdomain, "textdomain", 0, 1, 0,
	    (SCM domainname),
	    "If optional parameter @var{domainname} is supplied, "
	    "set the textdomain.  "
	    "Return the textdomain.")
#define FUNC_NAME s_scm_textdomain
{
  char const *c_result;
  char *c_domain;
  SCM result = SCM_BOOL_F;

  scm_dynwind_begin (0);

  if (SCM_UNBNDP (domainname))
    c_domain = NULL;
  else
    {
      c_domain = scm_to_locale_string (domainname);
      scm_dynwind_free (c_domain);
    }

  c_result = textdomain (c_domain);
  if (c_result != NULL)
    result = scm_from_locale_string (c_result);
  else if (!SCM_UNBNDP (domainname))
    SCM_SYSERROR;

  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bindtextdomain, "bindtextdomain", 1, 1, 0,
	    (SCM domainname, SCM directory),
	    "If optional parameter @var{directory} is supplied, "
	    "set message catalogs to directory @var{directory}.  "
	    "Return the directory bound to @var{domainname}.")
#define FUNC_NAME s_scm_bindtextdomain
{
  char *c_domain;
  char *c_directory;
  char const *c_result;
  SCM result;

  scm_dynwind_begin (0);

  if (SCM_UNBNDP (directory))
    c_directory = NULL;
  else
    {
      c_directory = scm_to_locale_string (directory);
      scm_dynwind_free (c_directory);
    }

  c_domain = scm_to_locale_string (domainname);
  scm_dynwind_free (c_domain);

  c_result = bindtextdomain (c_domain, c_directory);

  if (c_result != NULL)
    result = scm_from_locale_string (c_result);
  else if (!SCM_UNBNDP (directory))
    SCM_SYSERROR;
  else
    result = SCM_BOOL_F;

  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bind_textdomain_codeset, "bind-textdomain-codeset", 1, 1, 0,
	    (SCM domainname, SCM encoding),
	    "If optional parameter @var{encoding} is supplied, "
	    "set encoding for message catalogs of @var{domainname}.  "
	    "Return the encoding of @var{domainname}.")
#define FUNC_NAME s_scm_bind_textdomain_codeset
{
  char *c_domain;
  char *c_encoding;
  char const *c_result;
  SCM result;

  scm_dynwind_begin (0);

  if (SCM_UNBNDP (encoding))
    c_encoding = NULL;
  else
    {
      c_encoding = scm_to_locale_string (encoding);
      scm_dynwind_free (c_encoding);
    }

  c_domain = scm_to_locale_string (domainname);
  scm_dynwind_free (c_domain);

  c_result = bind_textdomain_codeset (c_domain, c_encoding);

  if (c_result != NULL)
    result = scm_from_locale_string (c_result);
  else if (!SCM_UNBNDP (encoding))
    SCM_SYSERROR;
  else
    result = SCM_BOOL_F;

  scm_dynwind_end ();
  return result;
}
#undef FUNC_NAME

void 
scm_init_i18n ()
{
  scm_add_feature ("i18n");
#include "libguile/i18n.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
