/******************************************************************************
 * MODULE     : mac_app.mm
 * DESCRIPTION: NSApplication related function for the X11 interface
 * COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "mac_app.h"
#include "Cocoa/mac_cocoa.h"
#include "mac_utilities.h"
#include "url.hpp"
#include "converter.hpp"
#include "../Guile/scheme.hpp"

static string 
from_nsstring (NSString *s) {
  const char *cstr = [s cStringUsingEncoding:NSUTF8StringEncoding];
  return utf8_to_cork(string((char*)cstr));
}


@interface TMAppDelegate : NSObject {
}
- (BOOL)application:(NSApplication *)theApplication openFile:(NSString *)filename ;
@end

@implementation TMAppDelegate
- (BOOL)application:(NSApplication *)theApplication openFile:(NSString *)filename 
{
  (void) theApplication;
  call ("texmacs-load-buffer", object(url_system (from_nsstring(filename))), object("generic"), object(1), object(false));
  return YES;
}
@end

NSAutoreleasePool *pool = nil;
TMAppDelegate *delegate = nil;

void init_mac_application ()
{
  [NSApplication sharedApplication];
  pool = [[NSAutoreleasePool alloc] init]; 
  delegate = [[TMAppDelegate alloc] init];
  [NSApp setDelegate: delegate];
  [NSApp finishLaunching];
  
  mac_begin_remote ();
}

void finalize_mac_application ()
{  
  mac_end_remote ();

  [pool release];
  [NSApp setDelegate:nil];
  [delegate release];
}

void process_mac_events ()
{
  do {
    [pool release];
    pool = [[NSAutoreleasePool alloc] init];
    
    NSEvent *event =
    [NSApp
     nextEventMatchingMask:NSAnyEventMask
     untilDate:nil //[NSDate distantFuture]
     inMode:NSDefaultRunLoopMode
     dequeue:YES];
    
    if (!event) break;
    [NSApp sendEvent:event];
    [NSApp updateWindows];
  } while (true);
  
}
