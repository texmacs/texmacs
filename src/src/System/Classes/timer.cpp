
/******************************************************************************
* MODULE     : timer.cpp
* DESCRIPTION: timers
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "timer.hpp"
#ifdef OS_WIN32
#include "sysmisc.hpp"
#endif

time_t
texmacs_time () {
#ifdef USE_GET_TIME_OF_DAY
#ifdef OS_WIN32
  struct timeval_ tp;
#else
  struct timeval tp;
#endif
  gettimeofday (&tp, NULL);
  return (time_t) ((tp.tv_sec * 1000) + (tp.tv_usec / 1000));
#else
  timeb tb;
  ftime (&tb);
  return (time_t) ((tb.time * 1000) + tb.millitm);
#endif
}

timer_rep::timer_rep () { start (); }
timer::timer () { rep= new timer_rep (); }

void
timer_rep::start () {
  begin= texmacs_time ();
  cumul= 0;
}

void
timer_rep::restart () {
  begin= texmacs_time ();
}

time_t
timer_rep::watch () {
  return texmacs_time ()- begin;
}

time_t
timer_rep::stop () {
  cumul += watch ();
  return cumul;
}
