/******************************************************************************
 * MODULE     : mac_utilities.mm
 * DESCRIPTION: Cocoa related utilites (also for TeXmacs/Qt)
 * COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "url.hpp"
#include "mac_utilities.h"
#include "tm_timer.hpp"
#include "gui.hpp"
#include "analyze.hpp"
#include "hashset.hpp"
#include "iterator.hpp"

#undef FAILED // redefined by CARBON
#define extend CARBON_extends // avoid name collision
#include "Cocoa/mac_cocoa.h"
#include <Carbon/Carbon.h>
#include <crt_externs.h>
#include "HIDRemote.h"
#undef extend

#undef FAILED // restore TeXmacs definition
#ifdef USE_EXCEPTIONS
#define FAILED(msg) { tm_throw (msg); }
#else
#ifdef DEBUG_ASSERT
#define FAILED(msg) { tm_failure (msg); assert (false); }
#else
#define FAILED(msg) { tm_failure (msg); }
#endif
#endif

#ifdef QTTEXMACS
#include <QApplication>
#include <QKeyEvent>
#include <QString>
#include "Qt/QTMWidget.hpp"
#include "Qt/qt_gui.hpp"
#include "Qt/qt_utilities.hpp"
#endif

bool 
mac_alternate_startup () {
#if __MAC_OS_X_VERSION_MIN_REQUIRED >= 1060
  NSUInteger nsmods = [NSEvent modifierFlags];
  return (nsmods &  NSAlternateKeyMask);
#else
  return ((CGEventSourceFlagsState(kCGEventSourceStateCombinedSessionState) 
           & NSDeviceIndependentModifierFlagsMask) == kCGEventFlagMaskAlternate);
#endif
}


#ifdef AQUATEXMACS
void 
mac_fix_paths () {
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
#endif




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

NSEvent *
mac_handler_body (NSEvent *event) {
  if (([event type] == NSKeyDown) || ([event type] == NSKeyUp)) {
    NSString *nss = [event charactersIgnoringModifiers];
    if ([nss length] > 0) {
      unichar key = [nss characterAtIndex:0];
      if ((key == NSTabCharacter) || (key == NSBackTabCharacter) ) {
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
        cout << from_qstring (str) << LF;
#endif
        
        QKeyEvent *qe = new QKeyEvent(([event type] == NSKeyDown) ? 
                                      QEvent::KeyPress : QEvent::KeyRelease, 
                                      Qt::Key_Tab, modifs);
        QApplication::postEvent(qApp->focusWidget(), qe);
        return nil;
      }
      if (key == 0x0051 || key == 0x0071) {
        NSUInteger nsmods = [event modifierFlags];
        Qt::KeyboardModifiers modifs = 0;
        if (key == NSBackTabCharacter) modifs |= Qt::ShiftModifier;
        if (nsmods &  NSControlKeyMask) modifs |= Qt::MetaModifier;
        if (nsmods &  NSAlternateKeyMask) modifs |= Qt::AltModifier;
        if (nsmods &  NSCommandKeyMask) modifs |= Qt::ControlModifier;
        if (nsmods & NSCommandKeyMask) {
          QKeyEvent *qe = new QKeyEvent(([event type] == NSKeyDown) ? 
                                        QEvent::KeyPress : QEvent::KeyRelease, 
                                        Qt::Key_Q, modifs);
          QApplication::postEvent(qApp->focusWidget(), qe);
          return nil;
        }
      }
    }
  }
  return event;
}
  
void 
mac_install_filter () {
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

//#ifdef Q_WS_MAC
#if 0

// this code is not used. It was an hack. Maybe sometimes in the future we
// should drop it

void 
cancel_tracking (NSMenu *menu) {
  [menu cancelTracking];
  for (NSMenuItem *item in [menu itemArray]) {
    if ([item submenu]) {
      cancel_tracking([item submenu]);
    }
  }
}


void 
mac_cancel_menu_tracking () {
#ifdef QT_MAC_USE_COCOA
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

/*********************/
/* remote controller */
/*********************/

@interface TMRemoteDelegate : NSObject <HIDRemoteDelegate> 
{
  // -- HID Remote --
	HIDRemote			*hidRemote;
}
- (void) setupRemote ;
- (void) cleanupRemote ;
- (void) startStopRemote:(bool) _start;
- (NSString *) buttonNameForButtonCode:(HIDRemoteButtonCode)buttonCode;
@end

@implementation TMRemoteDelegate
- (void) setupRemote
{
	if (!hidRemote)
	{
		if ((hidRemote = [[HIDRemote alloc] init]) != nil)
		{
			[hidRemote setDelegate:self];
		}
	}
}

static string 
from_nsstring (NSString *s) {
  const char *cstr = [s cStringUsingEncoding:NSUTF8StringEncoding];
  return utf8_to_cork(string((char*)cstr));
}


- (void) hidRemote:(HIDRemote *)theHidRemote
   eventWithButton:(HIDRemoteButtonCode)buttonCode
         isPressed:(BOOL)isPressed
fromHardwareWithAttributes:(NSMutableDictionary *)attributes
{
  (void) theHidRemote; (void) attributes;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; 
  mac_remote_button (from_nsstring([self buttonNameForButtonCode:buttonCode]), isPressed);
  [pool release];
}

- (void) cleanupRemote
{
	if ([hidRemote isStarted])
	{
		[hidRemote stopRemoteControl];
	}
	[hidRemote setDelegate:nil];
	[hidRemote release];
	hidRemote = nil;
}

- (NSString *)buttonNameForButtonCode:(HIDRemoteButtonCode)buttonCode
{
	switch (buttonCode)
	{
		case kHIDRemoteButtonCodeUp:
			return (@"ir-up");
      break;
      
		case kHIDRemoteButtonCodeDown:
			return (@"ir-down");
      break;
      
		case kHIDRemoteButtonCodeLeft:
			return (@"ir-left");
      break;
      
		case kHIDRemoteButtonCodeRight:
			return (@"ir-right");
      break;
      
		case kHIDRemoteButtonCodeCenter:
			return (@"ir-center");
      break;
      
		case kHIDRemoteButtonCodePlay:
			return (@"ir-play");
      break;
      
		case kHIDRemoteButtonCodeMenu:
			return (@"ir-menu");
      break;
      
		case kHIDRemoteButtonCodeUpHold:
			return (@"ir-up-hold");
      break;
      
		case kHIDRemoteButtonCodeDownHold:
			return (@"ir-down-hold");
      break;
      
		case kHIDRemoteButtonCodeLeftHold:
			return (@"ir-left-hold");
      break;
      
		case kHIDRemoteButtonCodeRightHold:
			return (@"ir-right-hold");
      break;
      
		case kHIDRemoteButtonCodeCenterHold:
			return (@"ir-center-hold");
      break;
      
		case kHIDRemoteButtonCodePlayHold:
			return (@"ir-play-hold");
      break;
      
		case kHIDRemoteButtonCodeMenuHold:
			return (@"ir-menu-hold");
      break;
      
    default:
      ;
	}
	
  return ([NSString stringWithFormat:@"ir-button-%x", (int)buttonCode]);
}

- (void) startStopRemote:(bool) _start
{
	// Has the HID Remote already been started?
	if ([hidRemote isStarted] && (!_start))
	{
		// HID Remote already started. Stop it.
		[hidRemote stopRemoteControl];
	}
	else if (_start)
	{
		// HID Remote has not been started yet. Start it.
		HIDRemoteMode remoteMode = kHIDRemoteModeNone;
		NSString *remoteModeName = nil;

#ifdef X11TEXMACS
    int mode = 1;
#else
    int mode = 2;
#endif

		switch (mode)
		{
			case 0:
				remoteMode = kHIDRemoteModeShared;
				remoteModeName = @"shared";
        break;
        
			case 1:
				remoteMode = kHIDRemoteModeExclusive;
				remoteModeName = @"exclusive";
        break;
        
			case 2:
				remoteMode = kHIDRemoteModeExclusiveAuto;
				remoteModeName = @"exclusive (auto)";
        break;
		}
    
		// Check whether the installation of Candelair is required to reliably operate in this mode
		if ([HIDRemote isCandelairInstallationRequiredForRemoteMode:remoteMode])
		{
			// Reliable usage of the remote in this mode under this operating system version
			// requires the Candelair driver to be installed. Tell the user about it.
			NSAlert *alert;
			
			if ((alert = [NSAlert alertWithMessageText:NSLocalizedString(@"Candelair driver installation necessary", @"")
			                             defaultButton:NSLocalizedString(@"Download", @"")
                                 alternateButton:NSLocalizedString(@"More information", @"")
                                     otherButton:NSLocalizedString(@"Cancel", @"")
                       informativeTextWithFormat:NSLocalizedString(@"An additional driver needs to be installed before %@ can reliably access the remote under the OS version installed on your computer.", @""), [[NSBundle mainBundle] objectForInfoDictionaryKey:(id)kCFBundleNameKey]]) != nil)
			{
				switch ([alert runModal])
				{
					case NSAlertDefaultReturn:
						[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"http://www.candelair.com/download/"]];
            break;
            
					case NSAlertAlternateReturn:
						[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"http://www.candelair.com/"]];
            break;
				}
			}
		}	
		else
		{
			// Candelair is either already installed or not required under this OS release => proceed!
			if ([hidRemote startRemoteControl:remoteMode])
			{
				// Start was successful, perform UI changes and log it.
//				[self appendToLog:[NSString stringWithFormat:@"-- Starting HID Remote in %@ mode successful --", remoteModeName]];
//				[startStopButton setTitle:@"Stop"];
//				[modeButton setEnabled:NO];
			}
			else
			{
				// Start failed. Log about it
//				[self appendToLog:[NSString stringWithFormat:@"Starting HID Remote in %@ mode failed", remoteModeName]];
			}
		}
	}
}

@end


TMRemoteDelegate* remote_delegate = nil;

void 
mac_begin_remote () {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; 
  if (!remote_delegate) {
    remote_delegate = [[TMRemoteDelegate alloc] init];
  }
  [remote_delegate setupRemote];
  [remote_delegate startStopRemote:true];
  [pool release];
}

void 
mac_end_remote () {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; 
  [remote_delegate startStopRemote:false];
  [remote_delegate cleanupRemote];
  [pool release];
}

void 
mac_remote_button (string button, bool pressed) {
  if (pressed) external_event (button, texmacs_time ());
}


// scale factor detection
// from http://src.chromium.org/svn/trunk/src/ui/gfx/screen_mac.mm

#if !defined(MAC_OS_X_VERSION_10_7) || \
MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7

@interface NSScreen (LionAPI)
- (CGFloat)backingScaleFactor;
@end

#endif  // 10.7

double
mac_screen_scale_factor() {
  CGFloat scale;
  NSScreen *screen = [NSScreen mainScreen];
    
  if ([screen respondsToSelector:@selector(backingScaleFactor)])
      scale = [screen backingScaleFactor];
  else
      scale = [screen userSpaceScaleFactor];
  return scale;
}

// end scale factor detection

#if defined (MAC_OS_X_VERSION_10_10)
/* A bug in OSX Yosemite inserts duplicate entries in the environment. This
 affects child processes: in particular, the PATH is not properly inherited
 unless we remove the duplicates and most plugins fail to start (since they are
 indirectly invoked through tm_* scripts.
 
 This code is adapted from Joe Cheng's https://github.com/jcheng5/envmunge
 */
void
mac_fix_yosemite_bug() {
  hashset<string> entries;
  hashset<string> duplicates;
  
    // Find duplicate entries in the environment
  for (char** entry = *_NSGetEnviron(); *entry; ++entry) {
    array<string> pair = tokenize (string (*entry), "=");
    if (N(pair) < 2) continue;
    string key = pair[0];
    if (entries->contains(key)) duplicates->insert(key);
    else                        entries->insert(key);
  }
    // Remove duplicate entries
  iterator<string> it = iterate (duplicates);
  while (it->busy()) {
    c_string name (it->next());
    const char* val = getenv (name); // remember value (first one on the list?)
    if (val != NULL) {
      unsetenv (name);       // removes all instances of name
      setenv (name, val, 0); // restore name=val
    }
  }
}
#endif   // defined (MAC_OS_X_VERSION_10_10)


static id background_activity= nil;

void
mac_begin_server () {
  if (background_activity == nil) {
    id background_activity = [[NSProcessInfo processInfo]
                               beginActivityWithOptions: NSActivityBackground
                              reason: @"TeXmacs server running"];
    [background_activity retain];
  }
}

void
mac_end_server () {
  if (background_activity) {
    [[NSProcessInfo processInfo] endActivity: background_activity];
    [background_activity release];
  }
}

