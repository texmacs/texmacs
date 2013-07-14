/******************************************************************************
 * MODULE     : tm_updater.cpp
 * DESCRIPTION: Base class for auto-update frameworks like (Win)Sparkle
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_updater.hpp"

#if defined (OS_MACOS) && defined (USE_SPARKLE)
#include "tm_sparkle.hpp"
#elif defined (OS_WIN32) && defined (USE_SPARKLE)
#include "tm_winsparkle.hpp"
#endif


tm_updater& tm_updater::instance (url _appcast_url)
{
  static tm_updater* _instance = NULL;
  
  if (! _instance) {
#if defined (OS_MACOS) && defined (USE_SPARKLE)
    _instance = new(std::nothrow) tm_sparkle (_appcast_url);
#elif defined (OS_WIN32) && defined (USE_SPARKLE)
    _instance = new(std::nothrow) tm_winsparkle (_appcast_url);
#else
    _instance = new(std::nothrow) tm_updater (_appcast_url);
#endif
  } else {
    if (_instance->getAppcast() != _appcast_url) {
      if (! _instance->isRunning()) {
        cout << "Updater] WARNING: changing appcast url from "
             << as_string (_instance->getAppcast()) << " to "
             << as_string (_appcast_url) << ".\n";
        _instance->setAppcast (_appcast_url);
      } else {
        cout << "Updater] ERROR: unable to set appcast url of busy updater.\n";
      }
    }
  }
  ASSERT (instance != NULL, "Unable to instantiate updater.");
  return *_instance;
}

/******************************************************************************
 * Scheme interface
 ******************************************************************************/

bool check_updates_background (url appcast)
{
  tm_updater& updater = tm_updater::instance (appcast);
  return !updater.isRunning() && updater.checkInBackground();
}

bool check_updates_foreground (url appcast)
{
  tm_updater& updater = tm_updater::instance (appcast);
  return !updater.isRunning() && updater.checkInForeground();
}