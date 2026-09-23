/******************************************************************************
* MODULE     : tm_test.hpp
* DESCRIPTION: a small test harness in the dialect of TeXmacs
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// A test file written against this header is ordinary TeXmacs C++: it uses
// string, cout and the containers of the kernel, and needs neither Qt nor
// any other framework. It declares its checks with CHECK, CHECK_MSG and
// CHECK_EQ, gives up on a missing resource with SKIP, and ends with
//
//   int main () { RUN (test_one); RUN (test_two); return test_report (); }
//
// which returns 0 when everything passed and 1 otherwise, so that the
// Makefile can tell. Each test is one function of no arguments; a failed
// check reports the file and the line and lets the test go on, the way a
// test of a table of values should.

#ifndef TM_TEST_H
#define TM_TEST_H

#include "string.hpp"

static int    test_nr_run    = 0;
static int    test_nr_failed = 0;
static int    test_nr_skipped= 0;
static bool   test_failed    = false;
static bool   test_skipped   = false;
static string test_name      = "";

inline void
test_failure (string msg, const char* file, int line) {
  if (!test_failed) test_nr_failed++;
  test_failed= true;
  cout << "FAILED " << test_name << ": " << msg
       << " at " << file << ":" << line << "\n";
}

inline void
test_check (bool ok, string msg, const char* file, int line) {
  if (!ok) test_failure (msg, file, line);
}

// as_string is ambiguous for a string, which both a tree and an url accept,
// so the values are shown through an overload set of our own
inline string test_show (bool x) { return x? string ("true"): string ("false"); }
inline string test_show (int x) { return as_string (x); }
inline string test_show (long int x) { return as_string (x); }
inline string test_show (long long int x) { return as_string (x); }
inline string test_show (double x) { return as_string (x); }
inline string test_show (string x) { return x; }
inline string test_show (const char* x) { return string (x); }

template<class T, class U> inline void
test_check_eq (T x, U y, string xs, string ys, const char* file, int line) {
  if (!(x == y))
    test_failure (xs * " is " * test_show (x) * ", not " *
                  ys * " = " * test_show (y), file, line);
}

inline void
test_give_up (string reason) {
  if (!test_skipped) test_nr_skipped++;
  test_skipped= true;
  cout << "skipped " << test_name << ": " << reason << "\n";
}

inline void
test_run (string name, void (*body) ()) {
  test_name   = name;
  test_failed = false;
  test_skipped= false;
  test_nr_run++;
  body ();
  if (!test_failed && !test_skipped) cout << "passed " << name << "\n";
}

inline int
test_report () {
  cout << "Totals: " << (test_nr_run - test_nr_failed - test_nr_skipped)
       << " passed, " << test_nr_failed << " failed, "
       << test_nr_skipped << " skipped\n";
  return test_nr_failed == 0? 0: 1;
}

#define CHECK(cond) test_check (cond, #cond, __FILE__, __LINE__)
#define CHECK_MSG(cond, msg) test_check (cond, msg, __FILE__, __LINE__)
#define CHECK_EQ(x, y) test_check_eq (x, y, #x, #y, __FILE__, __LINE__)
#define SKIP(reason) { test_give_up (reason); return; }
#define RUN(body) test_run (#body, body)

#endif // defined TM_TEST_H
