
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
#include "basic.hpp"
#include "tm_configure.hpp"

#ifdef OS_SUN
#include <sys/types.h>
#endif

#ifdef USE_GET_TIME_OF_DAY
#ifdef OS_WIN32
#include "systime.hpp"
#else
#include <sys/time.h>
#endif
#else
#include <sys/timeb.h>
#ifdef OS_SUN
extern "C" {
  extern int ftime __P ((struct timeb *__timebuf));
};
#endif
#endif

time_t texmacs_time ();

struct timer_rep: public concrete_struct {
  time_t begin;
  time_t cumul;

  timer_rep ();
  void   start ();
  void   restart ();
  time_t watch ();
  time_t stop ();
};

class timer {
  CONCRETE(timer);
  timer ();
};
CONCRETE_CODE(timer);

#endif // defined TIMER_H
