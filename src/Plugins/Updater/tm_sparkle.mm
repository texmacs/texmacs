/******************************************************************************
 * MODULE     : tm_sparkle.mm
 * DESCRIPTION: Manager class for the autoupdater Sparkle framework
 * COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

  // HACK, should be fixed with autotools
#if defined (OS_MACOS) && defined (USE_SPARKLE)

#include "tm_sparkle.hpp"
#include "string.hpp"
#include "Cocoa/mac_cocoa.h"
#include <Sparkle/Sparkle.h>

/*! A class to hide the type SUUpdater from the c++ header. */
class tm_sparkle::tm_suupdater
{
public:
  SUUpdater* p;
  NSAutoreleasePool* pool;
};

tm_sparkle::tm_sparkle (url _appcast_url) : tm_updater (_appcast_url)
{
  c_string s (as_string (_appcast_url));  // FIXME! This has to be UTF8!
  
    // Enable Cocoa’s memory management instantiating an Autorelease Pool
  updater->pool = [[NSAutoreleasePool alloc] init];

  updater->p = [[SUUpdater sharedUpdater] retain];
  NSURL* url = [NSURL URLWithString: [NSString stringWithUTF8String: s]];
  [updater->p setFeedURL: url];
}

tm_sparkle::~tm_sparkle ()
{
  [updater->p release];
  [updater->pool release];
}

bool tm_sparkle::isRunning() const
{
  return [updater->p updateInProgress];
}

bool tm_sparkle::checkInBackground ()
{
  [updater->p checkForUpdatesInBackground];
  return true;
}

bool tm_sparkle::checkInForeground ()
{
  [updater->p checkForUpdates:nil];
  return true;
}

#endif // defined (OS_MACOS) && defined (USE_SPARKLE)
