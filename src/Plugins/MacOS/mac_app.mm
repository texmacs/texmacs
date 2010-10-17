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
#include "url.hpp"
#include "converter.hpp"
#include "../Guile/scheme.hpp"

#ifdef QTTEXMACS
#include <QtGui>
#endif

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
}

void finalize_mac_application ()
{
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

#ifdef QTTEXMACS
#if 1
//HACK:
// the following code fixes a bug in Qt/Cocoa which do not correctly handle
// Ctrl+Tab key combination. In particular no QKeyDown event is generated for
// Shift+Tab and Ctrl+Tab. For this reason we intercept the event at the Cocoa
// level and just perform manually the translation to equivalent Qt event which
// is then sent directly to the focused widget. 
// It is rather simplistic approach but seems to work.
// Since it is an hack, the filter is  installed  only if we link againts the 
// bugged version of Qt.
// This filter is installed in qt_gui.cpp
// To use the API we need to compile in ObjC 2.0 since blocks are required

NSEvent *mac_handler_body(NSEvent *event) 
{
  if (([event type] == NSKeyDown) || ([event type] == NSKeyUp)) {
    unichar key = [[event charactersIgnoringModifiers] characterAtIndex:0];
    if ((key == NSTabCharacter) || (key == NSBackTabCharacter)) {
      NSUInteger nsmods = [event modifierFlags];
      Qt::KeyboardModifiers modifs = 0;
      if (key == NSBackTabCharacter) modifs |= Qt::ShiftModifier;
      if (nsmods &  NSControlKeyMask) modifs |= Qt::MetaModifier;
      if (nsmods &  NSAlternateKeyMask) modifs |= Qt::AltModifier;
      if (nsmods &  NSCommandKeyMask) modifs |= Qt::ControlModifier;

#if 0 // DEBUGGING CODE
      QString str;
      if (key == NSBackTabCharacter) str.append("Shift+");
      if (nsmods &  NSControlKeyMask) str.append("Ctrl+");
      if (nsmods &  NSAlternateKeyMask) str.append("Alt+");
      if (nsmods &  NSCommandKeyMask) str.append("Meta+");
      str.append("Tab");
      cout << str.toAscii().constData() << LF;
#endif      
      
      QKeyEvent *qe = new QKeyEvent(([event type] == NSKeyDown) ? 
                                        QEvent::KeyPress : QEvent::KeyRelease, 
                                    Qt::Key_Tab, modifs);
      QApplication::postEvent(qApp->focusWidget(), qe);
      return nil;
      }
  }
  return event;
}

void mac_install_filter() {
#if NS_BLOCKS_AVAILABLE
  pool = [[NSAutoreleasePool alloc] init];

  NSEvent * (^mac_handler)(NSEvent * ) = ^ (NSEvent *event) {
    return mac_handler_body(event);
  };
  [NSEvent addLocalMonitorForEventsMatchingMask: NSKeyDownMask | NSKeyUpMask 
                                        handler:mac_handler];
  [pool release];
  
#endif
}

#endif // HACK
#endif // QTTEXMACS

bool 
mac_alternate_startup() {
  NSUInteger nsmods = [NSEvent modifierFlags];
  return (nsmods &  NSAlternateKeyMask);
}

