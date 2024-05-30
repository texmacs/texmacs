/* testlib.h --- reporting test results
   Jim Blandy <jimb@red-bean.com> --- August 1999 */

#ifndef TESTLIB_H
#define TESTLIB_H

extern void test_pass (char *name);
extern void test_fail (char *name);
extern void test_pass_if (char *name, int condition);

/* We need a way to keep track of what groups of tests we're currently
   within.  A call to test_enter_context assures that future tests
   will be reported with a name prefixed by NAME, until we call
   test_restore_context with the value it returned.

   Calls to test_enter_context and test_restore_context should be
   properly nested; passing the context around allows them to detect
   mismatches.

   It is the caller's responsibility to free NAME after exiting the
   context.  (This is trivial if you're passing string literals to
   test_enter_context.)  */

typedef int test_context_t;
extern test_context_t test_enter_context (char *name);
extern void test_restore_context (test_context_t context);

#endif /* TESTLIB_H */
