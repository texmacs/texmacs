/******************************************************************************
 * MODULE     : tm_updater.cpp
 * DESCRIPTION: Base class for auto-update frameworks like (Win)Sparkle
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "config.h"
#include "tm_updater.hpp"
#include "scheme.hpp"

#if defined (OS_MACOS) && defined (USE_SPARKLE)
#include "tm_sparkle.hpp"
#elif defined (OS_MINGW) && defined (USE_SPARKLE)
#include "tm_winsparkle.hpp"
#endif


tm_updater* tm_updater::instance ()
{
  static tm_updater* _instance = NULL;
  
  url appcast = get_preference ("updater:appcast");
  if (appcast == "default") {
    if (DEBUG_STD)
      cout << "Updater] ERROR: no appcast URL was set in the preferences.\n";
    return 0;
  }

  if (! _instance) {
    #if defined (OS_MACOS) && defined (USE_SPARKLE)
      _instance = new tm_sparkle ();
    #elif defined (OS_MINGW) && defined (USE_SPARKLE)
      _instance = new tm_winsparkle ();
    #else
      _instance = new tm_updater ();
    #endif
  }

  ASSERT (_instance != NULL, "Unable to instantiate updater.");
  return _instance;
}

/******************************************************************************
 * Scheme interface
 ******************************************************************************/

bool updater_supported ()
{
  #if defined (OS_MACOS) && defined (USE_SPARKLE)
    return true;
  #elif defined (OS_MINGW) && defined (USE_SPARKLE)
    return true;
  #endif
    return false;
}

bool updater_is_running ()
{
  tm_updater* updater = tm_updater::instance ();
  return updater && updater->isRunning();
}

bool updater_check_background ()
{
  tm_updater* updater = tm_updater::instance ();
  return updater && updater->checkInBackground();
}

bool updater_check_foreground ()
{
  tm_updater* updater = tm_updater::instance ();
  return updater && updater->checkInForeground();
}

bool updater_set_appcast (url appcast)
{
  tm_updater* updater = tm_updater::instance ();
  return updater && updater->setAppcast (appcast);
}

bool updater_set_interval (int hours)
{
  tm_updater* updater = tm_updater::instance ();
  return updater && updater->setCheckInterval (hours);
}

bool updater_set_automatic (bool enable)
{
  tm_updater* updater = tm_updater::instance ();
  return updater && updater->setAutomaticChecks (enable);
}

time_t updater_last_check ()
{
  tm_updater* updater = tm_updater::instance ();
  return updater ? updater->lastCheck () : 0;
}
