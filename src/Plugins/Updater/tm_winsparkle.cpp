/******************************************************************************
 * MODULE     : tm_winsparkle.cpp
 * DESCRIPTION: Manager class for the autoupdater WinSparkle framework
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_configure.hpp"

  // HACK, should be fixed with autotools
#if defined (OS_MINGW) && defined (USE_SPARKLE) 

#include "tm_winsparkle.hpp"
#include "string.hpp"
#include <winsparkle.h>

tm_winsparkle::~tm_winsparkle ()
{
  win_sparkle_cleanup();
}

bool tm_winsparkle::setAppcast (url _appcast_url)
{
  if (running) return false;
  if (appcast == _appcast_url) return true;
  
  appcast = _appcast_url;
  c_string s (as_string (_appcast_url));  // FIXME! This has to be UTF8!
  win_sparkle_set_appcast_url (s);
  
  return true;
}

bool tm_winsparkle::setAutomaticChecks (bool enable)
{
  if (running) return false;
#if WIN_SPARKLE_CHECK_VERSION(0,4,0)
  win_sparkle_set_automatic_check_for_updates (enable ? 1 : 0);
#endif
  return true;
}

bool tm_winsparkle::setCheckInterval (int hours)
{
  if (running) return false;
  if (interval == hours) return true;
  
  interval = max (MinimumCheckInterval, min (MaximumCheckInterval, hours));
  
#if WIN_SPARKLE_CHECK_VERSION(0,4,0)
  win_sparkle_set_update_check_interval (interval * 3600);
#endif
  return true;
}

time_t tm_winsparkle::lastCheck() const
{
#if WIN_SPARKLE_CHECK_VERSION(0,4,0)
  return win_sparkle_get_last_check_time();
#else
  return 0;
#endif
}

bool tm_winsparkle::checkInBackground ()
{
  // WinSparkle docs state that configuration must be finished before the first
  // call to win_sparkle_init(), so we block any further attempts to change it.
  if (running) return false;
  running = true;
  win_sparkle_init();
#if WIN_SPARKLE_CHECK_VERSION(0,4,0)
  win_sparkle_check_update_without_ui();
#endif
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

#endif  // defined (OS_MINGW) && defined (USE_SPARKLE)
