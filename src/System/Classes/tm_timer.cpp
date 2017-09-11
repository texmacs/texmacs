
/******************************************************************************
* MODULE     : timer.cpp
* DESCRIPTION: timers
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_timer.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"

static hashmap<string,int> timing_level (0);
static hashmap<string,int> timing_nr    (0);
static hashmap<string,int> timing_cumul (0);
static hashmap<string,int> timing_last  (0);

/******************************************************************************
* Getting the time
******************************************************************************/

time_t
raw_time () {
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday (&tp, NULL);
  return (time_t) ((tp.tv_sec * 1000) + (tp.tv_usec / 1000));
#else
  timeb tb;
  ftime (&tb);
  return (time_t) ((tb.time * 1000) + tb.millitm);
#endif
}

static time_t start_time= raw_time ();

time_t
texmacs_time () {
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday (&tp, NULL);
  return ((time_t) ((tp.tv_sec * 1000) + (tp.tv_usec / 1000))) - start_time;
#else
  timeb tb;
  ftime (&tb);
  return ((time_t) ((tb.time * 1000) + tb.millitm)) - start_time;
#endif
}

/******************************************************************************
* Routines for benchmarking
******************************************************************************/

void
bench_start (string task) {
  // start timer for a given type of task
  if (timing_level [task] == 0)
    timing_last (task)= (int) texmacs_time ();
  timing_level (task) ++;
}

void
bench_cumul (string task) {
  // end timer for a given type of task, but don't reset timer
  timing_level (task) --;
  if (timing_level [task] == 0) {
    int ms= ((int) texmacs_time ()) - timing_last (task);
    timing_nr    (task) ++;
    timing_cumul (task) += ms;
    timing_last -> reset (task);
  }
}

void
bench_end (string task) {
  // end timer for a given type of task, print result and reset timer
  bench_cumul (task);
  bench_print (task);
  bench_reset (task);
}

void
bench_reset (string task) {
  // reset timer for a given type of task
  timing_level->reset (task);
  timing_nr   ->reset (task);
  timing_cumul->reset (task);
  timing_last ->reset (task);
}

void
bench_print (string task) {
  // print timing for a given type of task
  if (DEBUG_BENCH) {
    int nr= timing_nr [task];
    std_bench << "Task '" << task << "' took "
              << timing_cumul [task] << " ms";
    if (nr > 1) std_bench << " (" << nr << " invocations)";
    std_bench << "\n";
  }
}

static array<string>
collect (hashmap<string,int> h) {
  array<string> a;
  iterator<string> it= iterate (h);
  while (it->busy ())
    a << it->next ();
  merge_sort (a);
  return a;
}

void
bench_print () {
  // print timings for all types of tasks
  array<string> a= collect (timing_cumul);
  int i, n= N(a);
  for (i=0; i<n; i++)
    bench_print (a[i]);
}
