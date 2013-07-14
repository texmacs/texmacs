/******************************************************************************
 * MODULE     : tm_sparkle.mm
 * DESCRIPTION: Manager class for the autoupdater Sparkle framework
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "tm_configure.hpp"

  // HACK, should be fixed with autotools
#if defined (OS_MACOS) && defined (USE_SPARKLE)

#include "tm_sparkle.hpp"
#include "string.hpp"
#include "Cocoa/mac_cocoa.h"
#include <Sparkle/Sparkle.h>

/*! A class to hide the Cocoa types from the c++ header. */
class tm_sparkle::tm_suupdater
{
public:
  tm_suupdater () {
      // Enable Cocoa’s memory management instantiating an Autorelease Pool
    pool = [[NSAutoreleasePool alloc] init];
    p = [[SUUpdater sharedUpdater] retain];
  }
  ~tm_suupdater () {
    [pool release];
  }
  SUUpdater* p;
  NSAutoreleasePool* pool;
};


tm_sparkle::tm_sparkle (url _appcast_url) : tm_updater (_appcast_url)
{
  c_string s (as_string (_appcast_url));  // FIXME! This has to be UTF8!

  cout << "Updater] Instantiating Sparkle object for "
       << as_string (appcast) << LF;
  
  updater = new tm_suupdater;
  
  NSURL* url = [NSURL URLWithString: [NSString stringWithUTF8String: s]];
  [updater->p setFeedURL: url];
}

tm_sparkle::~tm_sparkle ()
{
  cout << "Updater] Deleting Sparkle object for "
       << as_string (appcast) << LF;
  delete updater;
}

bool tm_sparkle::isRunning() const
{
  return [updater->p updateInProgress];
}

bool tm_sparkle::checkInBackground ()
{
  cout << "Updater] Starting background check for updates at "
       << as_string (appcast) << LF;
  [updater->p checkForUpdatesInBackground];
  return true;
}

bool tm_sparkle::checkInForeground ()
{
  [updater->p checkForUpdates:nil];
  return true;
}

#endif // defined (OS_MACOS) && defined (USE_SPARKLE)
