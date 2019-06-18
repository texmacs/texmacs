
/******************************************************************************
* MODULE     : tm_sparkle.mm
* DESCRIPTION: Manager class for the autoupdater Sparkle framework
* COPYRIGHT  : (C) 2013 Miguel de Benito Delgado
*              2019 modified by Gregoire Lecerf
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


tm_sparkle::tm_sparkle () : tm_updater ()
{
  if (DEBUG_STD)
    debug_updater << "Instantiating Sparkle object.\n";

  updater = new tm_suupdater;
}

tm_sparkle::~tm_sparkle ()
{
  if (DEBUG_STD)
    debug_updater << "Deleting Sparkle object" << LF;
  delete updater;
}

bool tm_sparkle::isRunning() const
{
  return [updater->p updateInProgress];
}

time_t tm_sparkle::lastCheck() const
{
  NSDate* last = [updater->p lastUpdateCheckDate];
  if (! last) return 0;
  return [last timeIntervalSince1970];
  /*
  NSString* ns = [NSDateFormatter localizedStringFromDate: last
                                  dateStyle:NSDateFormatterShortStyle
                                  timeStyle:NSDateFormatterShortStyle];
  string s = [ns cStringUsingEncoding:NSUTF8StringEncoding];
  return s;
   */
}

bool tm_sparkle::setCheckInterval (int hours)
{
  if (interval == hours)
    return true;

  interval = max (MinimumCheckInterval, min (MaximumCheckInterval, hours));

  if (DEBUG_STD)
    debug_updater << "Changing interval from "
                  << interval << " to " << hours << " hour(s).\n";

  [updater->p setUpdateCheckInterval: interval*3600];
  [updater->p resetUpdateCycle];
  return true;
}

bool tm_sparkle::checkInBackground ()
{
  if (isRunning()) {
    if (DEBUG_STD)
      debug_updater << "ERROR: an update is already in progress.\n";
      return false;
  }

  if (DEBUG_STD)
    debug_updater << "Scheduling background check" << LF;
  
  [updater->p checkForUpdatesInBackground];
  return true;
}

bool tm_sparkle::checkInForeground ()
{
  if (isRunning()) {
    if (DEBUG_STD)
      debug_updater << "ERROR: an update is already in progress.\n";
    return false;
  }

  if (DEBUG_STD)
    debug_updater << "Starting foreground check" << LF;

  [updater->p checkForUpdates:nil];
  return true;
}

#endif // defined (OS_MACOS) && defined (USE_SPARKLE)
