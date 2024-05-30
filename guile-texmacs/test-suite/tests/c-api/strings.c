
/* NOTE: this code was never being run.  The same tests have been
   migrated to standalone/test-gh.c */

/* strings.c --- test the Guile C API's string handling functions
   Jim Blandy <jimb@red-bean.com> --- August 1999  */

#include <guile/gh.h>

#include "testlib.h"

static int
string_equal (SCM str, char *lit)
{
  int len = strlen (lit);
  
  return (SCM_LENGTH (str) == len
	  && ! memcmp (SCM_ROCHARS (str), lit, len));
}

void
test_gh_set_substr ()
{
  test_context_t cx = test_enter_context ("gh_set_substr");
  SCM string;

  string = gh_str02scm ("Free, darnit!");
  test_pass_if ("make a string", gh_string_p (string));

  gh_set_substr ("dammit", string, 6, 6);
  test_pass_if ("gh_set_substr from literal",
		string_equal (string, "Free, dammit!"));
  
  /* Make sure that we can use the string itself as a source.

     I guess this behavior isn't really visible, since the GH API
     doesn't provide any direct access to the string contents.  But I
     think it should, eventually.  You can't write efficient string
     code if you have to copy the string just to look at it.  */

  /* Copy a substring to an overlapping region to its right.  */
  gh_set_substr (SCM_CHARS (string), string, 4, 6);
  test_pass_if ("gh_set_substr shifting right",
		string_equal (string, "FreeFree, it!"));
  
  string = gh_str02scm ("Free, darnit!");
  test_pass_if ("make another string", gh_string_p (string));

  /* Copy a substring to an overlapping region to its left.  */
  gh_set_substr (SCM_CHARS (string) + 6, string, 2, 6);
  test_pass_if ("gh_set_substr shifting right",
		string_equal (string, "Frdarnitrnit!"));

  test_restore_context (cx);
}

void 
main_prog (int argc, char *argv[])
{
  test_context_t strings = test_enter_context ("strings.c");

  test_gh_set_substr ();

  test_restore_context (strings);

  exit (test_summarize ());
}

int 
main (int argc, char *argv[])
{
  gh_enter (argc, argv, main_prog);
  return 0;
}
