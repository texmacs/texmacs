/******************************************************************************
 * MODULE     : mac_utilities.mm
 * DESCRIPTION: Cocoa related utilites (also for TeXmacs/Qt)
 * COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "mac_utilities.h"
#include "Cocoa/mac_cocoa.h"
#include "url.hpp"

#ifdef QTTEXMACS
#include <QtGui>
#include <Carbon/Carbon.h>
#endif

bool 
mac_alternate_startup() {
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 1060
  NSUInteger nsmods = [NSEvent modifierFlags];
  return (nsmods &  NSAlternateKeyMask);
#else
  return ((CGEventSourceFlagsState(kCGEventSourceStateCombinedSessionState) 
           & NSDeviceIndependentModifierFlagsMask) == kCGEventFlagMaskAlternate);
#endif
}


void mac_fix_paths()
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  /* add appropriate TEXMACS_PATH to the current environment */
#if 0
  setenv("TEXMACS_PATH",
         [[[[NSBundle mainBundle] resourcePath] 
           stringByAppendingPathComponent:@"share/TeXmacs"] 
          cStringUsingEncoding:NSUTF8StringEncoding],
         1);
#endif
  /* add TeX directory */
  /* FIXME: make this user-defined */
  // FIXME: encoding here is not quite correct!!!
  setenv("PATH",
         [[[NSString stringWithCString:getenv("PATH") encoding:NSASCIIStringEncoding] 
           stringByAppendingString:@":/usr/texbin"]
          cStringUsingEncoding:NSUTF8StringEncoding],
         1); 
  setenv("GUILE_LOAD_PATH","/opt/local/share/guile/1.8",1);
  system("printenv");
  [pool release];  
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
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  
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

void cancel_tracking(NSMenu *menu) {
  [menu cancelTrackingWithoutAnimation];
  for (NSMenuItem *item in [menu itemArray]) {
    if ([item submenu]) {
      cancel_tracking([item submenu]);
    }
  }
}

#ifdef Q_WS_MAC
void mac_cancel_menu_tracking() {
#ifdef QT_MAC_USE_COCOA
  cout << "pippo\n";
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSMenu *mainMenu = [  [NSApplication sharedApplication]
                      mainMenu];
  [mainMenu cancelTrackingWithoutAnimation];
  {
    NSString *nss = [NSString stringWithCString:"\x1b" encoding:NSASCIIStringEncoding];
    NSEvent *ke = [NSEvent keyEventWithType: NSKeyDown location:NSMakePoint(0,0) modifierFlags:0 
                                  timestamp:1 windowNumber:0 context:0 characters:nss 
                charactersIgnoringModifiers:nss isARepeat:NO keyCode:0x1b];
    [mainMenu performKeyEquivalent:ke];
  }
  cancel_tracking(mainMenu);
  [pool release];
#else
  CancelMenuTracking(AcquireRootMenu(), true, 0);
#endif
}
#endif
