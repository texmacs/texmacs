/******************************************************************************
 * MODULE     : tm_winsparkle.cpp
 * DESCRIPTION: Manager class for the autoupdater WinSparkle framework
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *              2019 modified by Gregoire Lecerf
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_configure.hpp"

#if defined (USE_SPARKLE) && defined (OS_MINGW)

#include "tm_winsparkle.hpp"
#include "string.hpp"
#include <winsparkle.h>

tm_winsparkle::~tm_winsparkle ()
{
  win_sparkle_cleanup();
}

bool tm_winsparkle::setCheckInterval (int hours)
{
  if (running) return false;
  interval = hours <= 0 ? 0
    : max (MinimumCheckInterval, min (MaximumCheckInterval, hours));
  win_sparkle_set_update_check_interval (interval * 3600);
  win_sparkle_set_automatic_check_for_updates (interval > 0 ? 1 : 0);
  return true;
}

time_t tm_winsparkle::lastCheck() const
{
  return win_sparkle_get_last_check_time();
}

bool tm_winsparkle::checkInBackground ()
{
  // WinSparkle docs state that configuration must be finished before the first
  // call to win_sparkle_init(), so we block any further attempts to change it.
  if (running || interval <= 0) return false;
  running = true;
  win_sparkle_init();
  win_sparkle_check_update_without_ui();
  return true;
}

bool tm_winsparkle::checkInForeground ()
{
  if (running) return false;
  running = true;
  win_sparkle_init();
  win_sparkle_check_update_with_ui();
  return true;
}

#endif  // defined (USE_SPARKLE) && defined (OS_MINGW)
