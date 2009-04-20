
/******************************************************************************
* MODULE     : TMView.mm
* DESCRIPTION: Main TeXmacs view
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#import "TMView.h"
#include "converter.hpp"
#include "message.hpp"
#include "aqua_renderer.h"
#include "aqua_gui.h"

#define PIXEL 256

extern bool aqua_update_flag;
extern int time_credit;
extern int timeout_time;

hashmap<int,string> nskeymap(NULL);

inline void scale (NSPoint &point)
{	
	point.x *= PIXEL; point.y *= -PIXEL;
}

inline void scaleSize (NSSize &point)
{	
	point.width *= PIXEL; point.height *= PIXEL;
}

inline void unscaleSize (NSSize &point)
{	
	point.width /= PIXEL; point.height /= PIXEL;
}



@interface TMRect : NSObject
{
	NSRect rect;
}
- initWithRect:(NSRect)_rect;
- (NSRect)rect;
@end

@implementation TMRect
- initWithRect:(NSRect)_rect
{
	[super init];
	rect = _rect;
	return self;
}
- (NSRect)rect { return rect; }
@end


@interface TMView (Private)
- (void)setNeedsDisplayInTMRect:(TMRect*)r;
- (void)delayedUpdate;
@end




@implementation TMView


inline void map(int code, string name)
{
  nskeymap(code) = name;
}

void initkeymap()
{
  map(0x0d,"return");
  map(0x09,"tab");
  map(0xf728,"backspace");
  map(0xf003,"enter");
  map(0x1b,"escape");
  map(0x0003,"K-enter");
  map(0x7f,"backspace");
  
  map( NSUpArrowFunctionKey       ,"up" );
  map( NSDownArrowFunctionKey     ,"down" );
  map( NSLeftArrowFunctionKey     ,"left" );
  map( NSRightArrowFunctionKey    ,"right" );
  map( NSF1FunctionKey    ,"F1" );
  map( NSF2FunctionKey    ,"F2" );
  map( NSF3FunctionKey    ,"F3" );
  map( NSF4FunctionKey    ,"F4" );
  map( NSF5FunctionKey    ,"F5" );
  map( NSF6FunctionKey    ,"F6" );
  map( NSF7FunctionKey    ,"F7" );
  map( NSF8FunctionKey    ,"F8" );
  map( NSF9FunctionKey    ,"F9" );
  map( NSF10FunctionKey   ,"F10" );
  map( NSF11FunctionKey   ,"F11" );
  map( NSF12FunctionKey   ,"F12" );
  map( NSF13FunctionKey   ,"F13" );
  map( NSF14FunctionKey   ,"F14" );
  map( NSF15FunctionKey   ,"F15" );
  map( NSF16FunctionKey   ,"F16" );
  map( NSF17FunctionKey   ,"F17" );
  map( NSF18FunctionKey   ,"F18" );
  map( NSF19FunctionKey   ,"F19" );
  map( NSF20FunctionKey   ,"F20" );
  map( NSF21FunctionKey   ,"F21" );
  map( NSF22FunctionKey   ,"F22" );
  map( NSF23FunctionKey   ,"F23" );
  map( NSF24FunctionKey   ,"F24" );
  map( NSF25FunctionKey   ,"F25" );
  map( NSF26FunctionKey   ,"F26" );
  map( NSF27FunctionKey   ,"F27" );
  map( NSF28FunctionKey   ,"F28" );
  map( NSF29FunctionKey   ,"F29" );
  map( NSF30FunctionKey   ,"F30" );
  map( NSF31FunctionKey   ,"F31" );
  map( NSF32FunctionKey   ,"F32" );
  map( NSF33FunctionKey   ,"F33" );
  map( NSF34FunctionKey   ,"F34" );
  map( NSF35FunctionKey   ,"F35" );
  map( NSInsertFunctionKey        ,"insert" );
  map( NSDeleteFunctionKey        ,"delete" );
  map( NSHomeFunctionKey  ,"home" );
  map( NSBeginFunctionKey         ,"begin" );
  map( NSEndFunctionKey   ,"end" );
  map( NSPageUpFunctionKey        ,"pageup" );
  map( NSPageDownFunctionKey      ,"pagedown" );
  map( NSPrintScreenFunctionKey   ,"printscreen" );
  map( NSScrollLockFunctionKey    ,"scrolllock" );
  map( NSPauseFunctionKey         ,"pause" );
  map( NSSysReqFunctionKey        ,"sysreq" );
  map( NSBreakFunctionKey         ,"break" );
  map( NSResetFunctionKey         ,"reset" );
  map( NSStopFunctionKey  ,"stop" );
  map( NSMenuFunctionKey  ,"menu" );
  map( NSUserFunctionKey  ,"user" );
  map( NSSystemFunctionKey        ,"system" );
  map( NSPrintFunctionKey         ,"print" );
  map( NSClearLineFunctionKey     ,"clear" );
  map( NSClearDisplayFunctionKey  ,"cleardisplay" );
  map( NSInsertLineFunctionKey    ,"insertline" );
  map( NSDeleteLineFunctionKey    ,"deleteline" );
  map( NSInsertCharFunctionKey    ,"insert" );
  map( NSDeleteCharFunctionKey    ,"delete" );
  map( NSPrevFunctionKey  ,"prev" );
  map( NSNextFunctionKey  ,"next" );
  map( NSSelectFunctionKey        ,"select" );
  map( NSExecuteFunctionKey       ,"execute" );
  map( NSUndoFunctionKey  ,"undo" );
  map( NSRedoFunctionKey  ,"redo" );
  map( NSFindFunctionKey  ,"find" );
  map( NSHelpFunctionKey  ,"help" );
  map( NSModeSwitchFunctionKey    ,"modeswitch" );  
}


- (id)initWithFrame:(NSRect)frame {
  self = [super initWithFrame:frame];
  if (self) {
    // Initialization code here.
    wid = NULL;
    processingCompose = NO;
    workingText = nil;
    delayed_rects = [[NSMutableArray arrayWithCapacity:100] retain];
  }
  return self;
}

-(void) dealloc
{
  [delayed_rects release];
  [self deleteWorkingText];
  [super dealloc];
}

- (void) setWidget:(widget_rep*) w
{
	wid = (simple_widget_rep*)w;
}

- (widget_rep*)widget
{
	return  (widget_rep*)wid;
}

- (void)setNeedsDisplayInTMRect:(TMRect*)r
{
  [self setNeedsDisplayInRect:[r rect]];
}

- (void)viewWillMoveToWindow:(NSWindow *)newWindow
{
  // query widget preferred size
  SI w,h;
  wid->handle_get_size_hint (w,h);
  NSSize s = NSMakeSize(w,h);
  unscaleSize(s);
  [self setFrameSize:s];
}

- (void)delayedUpdate
{
  NSMutableArray *arr = delayed_rects;
  NSEnumerator *enumerator = [arr objectEnumerator];
  TMRect *anObject;
  delayed_rects = [[NSMutableArray arrayWithCapacity:10] retain];
  while ((anObject = [enumerator nextObject])) {
    [self displayRect:[anObject rect]];
  }
  [arr release];
}



- (void)drawRect:(NSRect)rect 
{
  if (aqua_update_flag) {
    [delayed_rects addObject:[[[TMRect alloc] initWithRect:rect] autorelease]];
    return;
  }
    
	// Drawing code here.
	if ([self inLiveResize])
	{
		NSRect bounds = [self bounds];
		[[NSColor blackColor] set];
		[NSBezierPath strokeRect:NSInsetRect(bounds,1,1)];
		//    return;
	}
//	cout << "DRAWING : " << rect.origin.x << ","<< rect.origin.x << ","<< rect.size.width<< "," << rect.size.height <<  "\n";
//	NSRect bounds = [self bounds];
	
  {
		basic_renderer r = the_aqua_renderer();
    int x1 = rect.origin.x;
    int y1 = rect.origin.y+rect.size.height;
    int x2 = rect.origin.x+rect.size.width;
    int y2 = rect.origin.y;
    
    r -> begin([NSGraphicsContext currentContext]);
  //  r -> set_origin(0,0);
    r -> encode (x1,y1);
    r -> encode (x2,y2);
 //   cout << "DRAWING RECT " << x1 << "," << y1 << "," << x2 << "," << y2 << LF;
    r -> set_clipping (x1,y1,x2,y2);
    wid->handle_repaint (x1,y1,x2,y2);
		r->end();
    if (r->interrupted())
      aqua_update_flag= true;
	}
//	cout << "END DRAWING" << "\n";
 
  if (aqua_update_flag) {
    if (DEBUG_EVENTS)
      cout << "Postponed redrawing\n"; 
    [self performSelector:@selector(delayedUpdate) withObject: nil afterDelay: 10];
  }
  
}


#if 0
- (void)keyDown:(NSEvent *)theEvent
{
  if (!wid) return;
  
  {
    char str[256];
    string r;
    NSString *nss = [theEvent charactersIgnoringModifiers];
    unsigned int mods = [theEvent modifierFlags];
    
    
    
    if (([nss length]==1)&& (!processingCompose))
      
    {
      int key = [nss characterAtIndex:0];
      if (nskeymap->contains(key)) {
        r = nskeymap[key];
        r = ((mods & NSShiftKeyMask)? "S-" * r: r);
      }
      else
      {
        [nss getCString:str maxLength:256 encoding:NSUTF8StringEncoding];
        string rr (str, strlen(str));
        r= utf8_to_cork (rr);          
      } 
      
      
      string s (r);
      if (! contains_unicode_char (s))     
      {
        //      string s= ((mods & NSShiftKeyMask)? "S-" * r: r);
        /* other keyboard modifiers */
        if (N(s)!=0) {
          if (mods & NSControlKeyMask ) s= "C-" * s;
          if (mods & NSAlternateKeyMask) s= "Mod1-" * s;
          if (mods & NSCommandKeyMask) s= "Mod2-" * s;
          //   if (mods & NSNumericPadKeyMask) s= "Mod3-" * s;
          if (mods & NSHelpKeyMask) s= "Mod4-" * s;
          //    if (mods & NSFunctionKeyMask) s= "Mod5-" * s;
        }
        cout << "key press: " << s << LF;
        wid -> handle_keypress (s, texmacs_time());    
      }
    }
    else {
      processingCompose = YES;
      static NSMutableArray *nsEvArray = nil;
      if (nsEvArray == nil)
        nsEvArray = [[NSMutableArray alloc] initWithCapacity: 1];
      
      [nsEvArray addObject: theEvent];
      [self interpretKeyEvents: nsEvArray];
      [nsEvArray removeObject: theEvent];
    }
  }	
  
  
}
#else
- (void)keyDown:(NSEvent *)theEvent
{
  if (!wid) return;

  time_credit= 25;
  timeout_time= texmacs_time () + time_credit;
  static bool fInit = false;
  if (!fInit) {
    if (DEBUG_EVENTS)
      cout << "Initializing keymap\n";
    initkeymap();
    fInit= true;
  }
  
  {
    // char str[256];
    string r;
    NSString *nss = [theEvent charactersIgnoringModifiers];
    unsigned int mods = [theEvent modifierFlags];
    
    string modstr;
    
    if (mods & NSControlKeyMask ) modstr= "C-" * modstr;
    //    if (mods & NSAlternateKeyMask) modstr= "Mod1-" * modstr;
    if (mods & NSCommandKeyMask) modstr= "Mod1-" * modstr;
    //   if (mods & NSNumericPadKeyMask) s= "Mod3-" * s;
    if (mods & NSHelpKeyMask) modstr= "Mod4-" * modstr;
    
    //    if (!processingCompose)
    {
      if ([nss length]>0) {
        int key = [nss characterAtIndex:0];
        if (nskeymap->contains(key)) {
          r = nskeymap[key];
          r = ((mods & NSShiftKeyMask)? "S-" * modstr: modstr) * r;          
          cout << "function key press: " << r << LF;
          [self deleteWorkingText];
          wid -> handle_keypress (r, texmacs_time());    
          return;
        } else if (mods & (NSControlKeyMask  | NSCommandKeyMask | NSHelpKeyMask))
        {
          static char str[256];
          [nss getCString:str maxLength:256 encoding:NSUTF8StringEncoding];
          string rr (str, strlen(str));
          r= utf8_to_cork (rr);          
          
          string s ( modstr * r);
          cout << "modified  key press: " << s << LF;
          [self deleteWorkingText];
          wid -> handle_keypress (s, texmacs_time());    
          the_gui->update (); // FIXME: remove this line when
          // edit_typeset_rep::get_env_value will be faster

          return;
        }
      }
    }
    
    processingCompose = YES;
    static NSMutableArray *nsEvArray = nil;
    if (nsEvArray == nil)
      nsEvArray = [[NSMutableArray alloc] initWithCapacity: 1];
    
    [nsEvArray addObject: theEvent];
    [self interpretKeyEvents: nsEvArray];
    [nsEvArray removeObject: theEvent];
  }
}

#endif

static unsigned int
mouse_state (NSEvent* event, bool flag) {
  unsigned int i= 0;
  i += 1 << min([event buttonNumber],4);
  unsigned int mods = [event modifierFlags];
  if (mods & NSAlternateKeyMask) i = 2;  
  if (mods & NSCommandKeyMask) i = 4;  
  if (mods & NSShiftKeyMask) i += 256;  
  if (mods & NSControlKeyMask) i += 2048;  
  return i;
}

static string
mouse_decode (unsigned int mstate) {
  if      (mstate & 1 ) return "left";
  else if (mstate & 2 ) return "middle";
  else if (mstate & 4 ) return "right";
  else if (mstate & 8 ) return "up";
  else if (mstate & 16) return "down";
  return "unknown";
}



- (void)mouseDown:(NSEvent *)theEvent
{
  if (wid) {
    NSPoint point = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	scale(point);
    unsigned int mstate= mouse_state (theEvent, false);
    string s= "press-" * mouse_decode (mstate);
    wid -> handle_mouse (s, point.x , point.y , mstate, texmacs_time ());
    if (DEBUG_EVENTS)
      cout << "mouse event: " << s << " at "
      << point.x << ", " << point.y  << LF;
  }
}

- (void)mouseUp:(NSEvent *)theEvent
{
  if (wid) {
    NSPoint point = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	scale(point);
    unsigned int mstate= mouse_state (theEvent, true);
    string s= "release-" * mouse_decode (mstate);
    wid -> handle_mouse (s, point.x , point.y , mstate, texmacs_time ());
    if (DEBUG_EVENTS)
      cout << "mouse event: " << s << " at "
      << point.x  << ", " << point.y  << LF;
  }
}

- (void)mouseDragged:(NSEvent *)theEvent
{
  if (wid) {
    NSPoint point = [self convertPoint:[theEvent locationInWindow] fromView:nil];
		scale(point);
    unsigned int mstate= mouse_state (theEvent, false);
    string s= "move";
    wid -> handle_mouse (s, point.x , point.y , mstate, texmacs_time ());
    if (DEBUG_EVENTS)
      cout << "mouse event: " << s << " at "
      << point.x  << ", " << point.y  << LF;
  }  
}

- (void)mouseMoved:(NSEvent *)theEvent
{
  if (wid) {
    NSPoint point = [self convertPoint:[theEvent locationInWindow] fromView:nil];
		scale(point);
    unsigned int mstate= mouse_state (theEvent, false);
    string s= "move";
    wid -> handle_mouse (s, point.x , point.y , mstate, texmacs_time ());
    if (DEBUG_EVENTS)
      cout << "mouse event: " << s << " at "
      << point.x  << ", " << point.y  << LF;
  }  
}

- (BOOL)isFlipped
{
  return YES;
}

- (BOOL)isOpaque
{
  return YES;
}

- (void)resizeWithOldSuperviewSize:(NSSize)oldBoundsSize
{
  [super resizeWithOldSuperviewSize:oldBoundsSize];
  if (wid)  {
    NSSize size = [self bounds].size;
		scaleSize(size);
		wid-> handle_notify_resize (size.width, size.height);
  }
	
}

- (BOOL)acceptsFirstResponder
{
	return YES;
}

- (void) deleteWorkingText
{ 
  if (workingText == nil)
    return;
  [workingText release];
  workingText = nil;
  processingCompose = NO;
  
  
}

#pragma mark NSTextInput protocol implementation


- (void) insertText:(id)aString
// instead of keyDown: aString can be NSString or NSAttributedString
{
  processingCompose = NO;
  NSLog(@"insertText: <%@>",aString);
  
  NSString *str = [aString respondsToSelector: @selector(string)] ?
  [aString string] : aString;
  
  static char buf[256];
  for(unsigned int i=0; i<[str length]; i++) {
    [[str substringWithRange:NSMakeRange(i, 1)] getCString:buf maxLength:256 encoding:NSUTF8StringEncoding];
    string rr (buf, strlen(buf));
    string s= utf8_to_cork (rr);          
    cout << "key press: " << s << LF;
    wid -> handle_keypress (s, texmacs_time());        
  }
}

- (void) doCommandBySelector:(SEL)aSelector
{
}

// setMarkedText: cannot take a nil first argument. aString can be NSString or NSAttributedString
- (void) setMarkedText:(id)aString selectedRange:(NSRange)selRange
{
  NSString *str = [aString respondsToSelector: @selector(string)] ?
  [aString string] : aString;
  
  if (workingText != nil)
    [self deleteWorkingText];
  if ([str length] == 0)
    return;
  workingText = [str copy];
  processingCompose = YES;
  NSLog(@"setMarkedText: <%@>",workingText);
  
}

- (void) unmarkText
{
  [self deleteWorkingText];  
}
- (BOOL) hasMarkedText
{
  return workingText != nil;
  
}
- (NSInteger) conversationIdentifier
{
  return (NSInteger)self;
}

/* Returns attributed string at the range.  This allows input mangers to query any range in backing-store.  May return nil.
 */
- (NSAttributedString *) attributedSubstringFromRange:(NSRange)theRange
{
  static NSAttributedString *str = nil;
  if (str == nil) str = [NSAttributedString new];
  return str;
}

/* This method returns the range for marked region.  If hasMarkedText == false, it'll return NSNotFound location & 0 length range.
 */
- (NSRange) markedRange
{
  NSRange rng = workingText != nil
  ? NSMakeRange(0, [workingText length]) : NSMakeRange(NSNotFound, 0);
  return rng;
  
}

/* This method returns the range for selected region.  Just like markedRange method, its location field contains char index from the text beginning.
 */
- (NSRange) selectedRange
{
  return NSMakeRange(NSNotFound, 0);
}
/* This method returns the first frame of rects for theRange in screen coordindate system.
 */
- (NSRect) firstRectForCharacterRange:(NSRange)theRange
{
  return NSMakeRect(0,0,50,50);
}

/* This method returns the index for character that is nearest to thePoint.  thePoint is in screen coordinate system.
 */
- (NSUInteger)characterIndexForPoint:(NSPoint)thePoint
{
  return 0;
}

/* This method is the key to attribute extension.  We could add new attributes through this method. NSInputServer examines the return value of this method & constructs appropriate attributed string.
 */
- (NSArray*) validAttributesForMarkedText
{
  static NSArray *arr = nil;
  if (arr == nil) arr = [NSArray new];
  return arr;
}


@end
