/* testlib.c --- reporting test results
   Jim Blandy <jimb@red-bean.com> --- August 1999 */

#include <stdlib.h>
#include <stdio.h>

#include "testlib.h"



/* Dying.  */

static void
fatal (char *message)
{
  fprintf (stderr, "%s\n", message);
  exit (1);
}


/* Contexts.  */

/* If it gets deeper than this, that's probably an error, right?  */
#define MAX_NESTING 10

int depth = 0;
char *context_name_stack[MAX_NESTING];
int marker;
int context_marker_stack[MAX_NESTING];

test_context_t
test_enter_context (char *name)
{
  if (depth >= MAX_NESTING)
    fatal ("test contexts nested too deeply");

  /* Generate a unique marker value for this context.  */
  marker++;

  context_name_stack[depth] = name;
  context_marker_stack[depth] = marker;

  depth++;

  return marker;
}

void
test_restore_context (test_context_t context)
{
  if (depth <= 0)
    fatal ("attempt to leave outermost context");

  depth--;

  /* Make sure that we're exiting the same context we last entered.  */
  if (context_marker_stack[depth] != context)
    fatal ("contexts not nested properly");
}


/* Reporting results.  */

int count_passes, count_fails;

static void
print_test_name (char *name)
{
  int i;

  for (i = 0; i < depth; i++)
    printf ("%s: ", context_name_stack[i]);

  printf ("%s", name);
}

static void
print_result (char *result, char *name)
{
  printf ("%s: ", result);
  print_test_name (name);
  putchar ('\n');
}

void
test_pass (char *name)
{
  print_result ("PASS", name);
  count_passes++;
}

void
test_fail (char *name)
{
  print_result ("FAIL", name);
  count_fails++;
}

void
test_pass_if (char *name, int condition)
{
  (condition ? test_pass : test_fail) (name);
}


/* Printing a summary.  */

/* Print a summary of the reported test results.  Return zero if
   no failures occurred, one otherwise.  */

int
test_summarize ()
{
  putchar ('\n');

  printf ("passes:      %d\n", count_passes);
  printf ("failures:    %d\n", count_fails);
  printf ("total tests: %d\n", count_passes + count_fails);

  return (count_fails != 0);
}
