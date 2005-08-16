
/******************************************************************************
* MODULE     : timer.hpp
* DESCRIPTION: timers
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

time_t texmacs_time ();

void   bench_start (string task);
void   bench_cumul (string task);
void   bench_end   (string task);
void   bench_reset (string task);
void   bench_print (string task);
void   bench_print ();

#endif // defined TIMER_H
