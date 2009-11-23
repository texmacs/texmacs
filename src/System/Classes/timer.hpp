
/******************************************************************************
* MODULE     : timer.hpp
* DESCRIPTION: timers
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TIMER_H
#define TIMER_H
#include "string.hpp"
#include "tm_configure.hpp"

#ifndef HAVE_TIME_T
#define HAVE_TIME_T
typedef long time_t;
#endif

#ifdef OS_SUN
#include <sys/types.h>
#endif

#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h>
#else
#include <sys/timeb.h>
#ifdef OS_SUN
extern "C" {
  extern int ftime __P ((struct timeb *__timebuf));
};
#endif
#endif

time_t raw_time ();
time_t texmacs_time ();

void   bench_start (string task);
void   bench_cumul (string task);
void   bench_end   (string task);
void   bench_reset (string task);
void   bench_print (string task);
void   bench_print ();

#endif // defined TIMER_H
