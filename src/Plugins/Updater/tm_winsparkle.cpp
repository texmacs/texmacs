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
#if (defined (OS_MINGW) || defined (OS_WIN32))  && defined (USE_SPARKLE) 

#include "tm_winsparkle.hpp"
#include "string.hpp"
#include <winsparkle.h>

tm_winsparkle::~tm_winsparkle ()
{
  win_sparkle_cleanup();
}

bool tm_winsparkle::setAppcast (url _appcast_url)
{
  if (running) {
    if (DEBUG_STD)
      cout << "Updater] WARNING: unable to change appcast for running instance.\n";
    return false;
  }

  c_string s (as_string (_appcast_url));  // FIXME! This has to be UTF8!
  win_sparkle_set_appcast_url (s);
  return true;
}

bool tm_winsparkle::setAutomaticChecks (bool enable)
{
  if (running)
    return false;

    // TODO...
  return true;
}

bool tm_winsparkle::setCheckInterval (int hours)
{
  if (running)
    return false;
  
    // TODO...
  return true;
}

time_t tm_winsparkle::lastCheck() const
{
  return 0;
}

bool tm_winsparkle::checkInBackground ()
{
  win_sparkle_init();
    // WinSparkle docs state that configuration must be finished before the
    // first call to win_sparkle_init(), so we block any further attempts to
    // change it.
  running = true;
  return true;
}

bool tm_winsparkle::checkInForeground ()
{
  win_sparkle_check_update_with_ui();
  return true;
}

#endif  // (defined (OS_MINGW) || defined (OS_WIN32)) && defined (USE_SPARKLE)

