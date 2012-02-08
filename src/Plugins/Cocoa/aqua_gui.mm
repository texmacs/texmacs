
/******************************************************************************
* MODULE     : aqua_gui.mm
* DESCRIPTION: Cocoa display class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/


#include "iterator.hpp"
#include "dictionary.hpp"
#include "aqua_gui.h"
#include "analyze.hpp"
#include <locale.h>
#include "language.hpp"
#include "message.hpp"
#include "aqua_renderer.h" // for the_aqua_renderer

//extern hashmap<id, pointer> NSWindow_to_window;
//extern window (*get_current_window) (void);

aqua_gui_rep* the_gui= NULL;

int nr_windows = 0; // FIXME: fake variable, referenced in tm_server

bool aqua_update_flag= false;

int time_credit;
int timeout_time;

/******************************************************************************
 * Constructor and geometry
 ******************************************************************************/


aqua_gui_rep::aqua_gui_rep(int& argc, char** argv): 
  interrupted(false), selection(NULL)
{
  (void) argc; (void) argv;
//  argc               = argc2;
//  argv               = argv2;
  interrupted        = false;
  interrupt_time     = texmacs_time ();
  
  set_output_language (get_locale_language ());
  // out_lan= get_locale_language ();
//  (void) default_font ();
}


/* important routines */
void
aqua_gui_rep::get_extents (SI& width, SI& height) {
  NSRect bounds = [[NSScreen mainScreen] visibleFrame];
  
  width = ((SI) bounds.size.width)  * PIXEL;
  height= ((SI) bounds.size.height) * PIXEL;
}

void
aqua_gui_rep::get_max_size (SI& width, SI& height) {
  width = 8000 * PIXEL;
  height= 6000 * PIXEL;
}

/******************************************************************************
 * interclient communication
 ******************************************************************************/

bool
aqua_gui_rep::get_selection (string key, tree& t, string& s) {
  t= "none";
  s= "";
  if (selection_t->contains (key)) {
    t= copy (selection_t [key]);
    s= copy (selection_s [key]);
    return true;
  }
  if (key != "primary") return false;
  
	NSPasteboard *pb = [NSPasteboard generalPasteboard];
	NSArray *types = [NSArray arrayWithObject:NSStringPboardType];
	NSString *bestType = [pb availableTypeFromArray:types];

	if (bestType != nil) {
		NSString* data = [pb stringForType:bestType];
		if (data) {
		char *buf = (char*)[data UTF8String];
			unsigned size = strlen(buf);
		s << string(buf, size);
		}
	}


  t= tuple ("extern", s);
  return true;
}

bool
aqua_gui_rep::set_selection (string key, tree t, string s) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  if (key == "primary") {
    //if (is_nil (windows_l)) return false;
    //Window win= windows_l->item;
    if (selection!=NULL) tm_delete_array (selection);
    //XSetSelectionOwner (dpy, XA_PRIMARY, win, CurrentTime);
    //if (XGetSelectionOwner(dpy, XA_PRIMARY)==None) return false;
    selection= as_charp (s);
	
	NSPasteboard *pb = [NSPasteboard generalPasteboard];
	NSArray *types = [NSArray arrayWithObjects:
		NSStringPboardType, nil];
	[pb declareTypes:types owner:nil];
	[pb setString:[NSString stringWithCString:selection] forType:NSStringPboardType];
	
	
  }
  return true;
}

void
aqua_gui_rep::clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  if ((key == "primary") && (selection != NULL)) {
    tm_delete_array (selection);
	// FIXME: should we do something with the pasteboard?
    selection= NULL;
  }
}


/******************************************************************************
 * Miscellaneous
 ******************************************************************************/

void aqua_gui_rep::image_gc (string name) { (void) name; }
// FIXME: remove this unused function
void aqua_gui_rep::set_mouse_pointer (string name) { (void) name; }
// FIXME: implement this function
void aqua_gui_rep::set_mouse_pointer (string curs_name, string mask_name)  { (void) curs_name; (void) mask_name; } ;

/******************************************************************************
 * Main loop
 ******************************************************************************/

static bool check_mask(int mask)
{
  NSEvent * event = [NSApp nextEventMatchingMask:mask
                             untilDate:nil
                                inMode:NSDefaultRunLoopMode 
                               dequeue:NO];
 // if (event != nil) NSLog(@"%@",event);
  return (event != nil);
  
}

#if 0
bool
aqua_gui_rep::check_event (int type) {
  switch (type) {
    case INTERRUPT_EVENT:
      if (interrupted) return true;
      else  {
        time_t now= texmacs_time ();
        if (now - interrupt_time < 0) return false;
//        else interrupt_time= now + (100 / (XPending (dpy) + 1));
        else interrupt_time= now + 100;
        interrupted= check_mask(NSKeyDownMask |
                               // NSKeyUpMask |
                                NSLeftMouseDownMask |
                                NSLeftMouseUpMask |
                                NSRightMouseDownMask |
                                NSRightMouseUpMask );
        return interrupted;
      }
      case INTERRUPTED_EVENT:
        return interrupted;
      case ANY_EVENT:
        return check_mask(NSAnyEventMask);
      case MOTION_EVENT:
        return check_mask(NSMouseMovedMask);
      case DRAG_EVENT:
        return check_mask(NSLeftMouseDraggedMask|NSRightMouseDraggedMask);
      case MENU_EVENT:
        return check_mask(NSLeftMouseDownMask |
                          NSLeftMouseUpMask |
                          NSRightMouseDownMask |
                          NSRightMouseUpMask );
  }
  return interrupted;
}
#else
bool
aqua_gui_rep::check_event (int type) {
  return false;
}
#endif

void
aqua_gui_rep::show_wait_indicator (widget w, string message, string arg) {
}


void (*the_interpose_handler) (void) = NULL;
//void set_interpose_handler (void (*r) (void)) { the_interpose_handler= r; }
void gui_interpose (void (*r) (void)) { the_interpose_handler= r; }

void update()
{
	//NSBeep();
	if (the_interpose_handler) the_interpose_handler();
}

void aqua_gui_rep::update ()
{
//  NSLog(@"UPDATE----------------------------");
  ::update();
}




@interface TMHelper : NSObject
{
}
@end
@implementation TMHelper
- init
{
  if (self = [super init])
  {
		[NSApp setDelegate:self];
  }
  return self;
}

- (void)applicationWillUpdate:(NSNotification *)aNotification
{
//	NSBeep();
	update();
}
- (void)dealloc
{
	[NSApp setDelegate:nil];
  [super dealloc];
}
@end


@interface TMInterposer : NSObject
{  
	NSNotification *n;
}
- (void)interposeNow;
-(void)waitIdle;
@end

@implementation TMInterposer
- init
{
  if (self = [super init])
  {
	//	n = [[NSNotification notificationWithName:@"TMInterposeNotification" object:self] retain];
   // [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(interposeNow) name:@"TMInterposeNotification" object:nil];
		 [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(interposeNow) name:NSApplicationWillUpdateNotification object:nil];
	//	[self waitIdle];
  }
  return self;
}
- (void)dealloc
{
	//[n release];
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}

- (void)interposeNow
{
//	NSBeep();
	update();
	//[self performSelector:@selector(waitIdle) withObject:nil afterDelay:0.25 inModes:[NSArray arrayWithObjects:NSDefaultRunLoopMode, nil]];
}
-(void)waitIdle
{
	[[NSNotificationQueue defaultQueue] enqueueNotification:n 
																						 postingStyle:NSPostWhenIdle
																						 coalesceMask:NSNotificationCoalescingOnName 
																								 forModes:nil];
}

@end



//@class FScriptMenuItem;

void aqua_gui_rep::event_loop ()
#if 0
{
//	TMInterposer* i = [[TMInterposer alloc ] init];
	//[[NSApp mainMenu] addItem:[[[FScriptMenuItem alloc] init] autorelease]];
//	update();
	[[[TMHelper alloc] init] autorelease];
	[NSApp run];
//	[i release];
}
#else
{
//	[[NSApp mainMenu] addItem:[[[FScriptMenuItem alloc] init] autorelease]];
	[NSApp finishLaunching];
	{
	NSEvent *event = nil;
    time_credit= 1000000;

  while (1) {
    timeout_time= texmacs_time () + time_credit;

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
			NSDate *dateSlow = [NSDate dateWithTimeIntervalSinceNow:0.5];
		event= [NSApp nextEventMatchingMask:NSAnyEventMask untilDate: dateSlow //[NSDate distantFuture] 
																 inMode:NSDefaultRunLoopMode dequeue:YES];		
		while (event)
		{
			[NSApp sendEvent:event];
		//	update();
			//NSDate *dateFast = [NSDate dateWithTimeIntervalSinceNow:0.001];
			event= [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantPast] // dateFast 
																	 inMode:NSDefaultRunLoopMode dequeue:YES];
		}
		interrupted = false;
    if (!event)  {
       update();
      time_credit= min (1000000, 2 * time_credit);
      aqua_update_flag= false;
    }
    [pool release];
  }  
	}
}
#endif




aqua_gui_rep::~aqua_gui_rep() 
{ 
} 



/* interface ******************************************************************/
#pragma mark GUI interface

//static display cur_display= NULL;

static NSAutoreleasePool *pool = nil;
//static NSApplication *app = nil;



/******************************************************************************
* Main routines
******************************************************************************/

void gui_open (int& argc2, char** argv2)
  // start the gui
{
  if (!NSApp) {
    // initialize app
    [NSApplication sharedApplication];
    [NSBundle loadNibNamed:@"MainMenu" owner:NSApp];
  }
  if (!pool) {
    // create autorelease pool 
    pool = [[NSAutoreleasePool alloc] init];
  } else [pool retain];
  
  the_gui = tm_new <aqua_gui_rep> (argc2, argv2);
}

void gui_start_loop ()
  // start the main loop
{
  the_gui->event_loop ();
}

void gui_close ()
  // cleanly close the gui
{
  ASSERT (the_gui != NULL, "gui not yet open");
  [pool release];
  tm_delete (the_gui);
  the_gui=NULL;
}
void
gui_root_extents (SI& width, SI& height) {   
	// get the screen size
  the_gui->get_extents (width, height);
}

void
gui_maximal_extents (SI& width, SI& height) {
  // get the maximal size of a window (can be larger than the screen size)
  the_gui->get_max_size (width, height);
}

void gui_refresh ()
{
  // update and redraw all windows (e.g. on change of output language)
  // FIXME: add suitable code
}



/******************************************************************************
* Font support
******************************************************************************/

void
set_default_font (string name) {
	(void) name;
  // set the name of the default font
  // this is ignored since Qt handles fonts for the widgets
}

font
get_default_font (bool tt, bool mini, bool bold) {
  (void) tt; (void) mini;
  // get the default font or monospaced font (if tt is true)
	
  // return a null font since this function is not called in the Qt port.
  if (DEBUG_EVENTS) cout << "get_default_font(): SHOULD NOT BE CALLED\n";
  return NULL;
  //return tex_font (this, "ecrm", 10, 300, 0);
}

// load the metric and glyphs of a system font
// you are not obliged to provide any system fonts

void
load_system_font (string family, int size, int dpi,
                  font_metric& fnm, font_glyphs& fng)
{
	(void) family; (void) size; (void) dpi; (void) fnm; (void) fng;
	if (DEBUG_EVENTS) cout << "load_system_font(): SHOULD NOT BE CALLED\n";
}

/******************************************************************************
* Clipboard support
******************************************************************************/

  // Copy a selection 't' with string equivalent 's' to the clipboard 'cb'
  // Returns true on success
bool
set_selection (string key, tree t, string s, string format) {
  (void) format;
  return the_gui->set_selection (key, t, s);
}

  // Retrieve the selection 't' with string equivalent 's' from clipboard 'cb'
  // Returns true on success; sets t to (extern s) for external selections
bool
get_selection (string key, tree& t, string& s, string format) { 
  (void) format;
  return the_gui->get_selection (key, t, s);
}

  // Clear the selection on clipboard 'cb'
void
clear_selection (string key) {
  the_gui->clear_selection (key);
}


/******************************************************************************
* Miscellaneous
******************************************************************************/
int char_clip=0;

void 
beep () {
  // Issue a beep
  NSBeep();
}

void 
needs_update () {
  aqua_update_flag= true;
}

bool check_event (int type)
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
{ return the_gui->check_event(type); }

void image_gc (string name) {
  // Garbage collect images of a given name (may use wildcards)
  // This routine only needs to be implemented if you use your own image cache
  the_aqua_renderer()->image_gc(name); 
}

void
show_help_balloon (widget balloon, SI x, SI y) { 
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
  (void) balloon; (void) x; (void) y;
}

void
show_wait_indicator (widget base, string message, string argument) {
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty
  the_gui->show_wait_indicator(base,message,argument); 
}

void
external_event (string type, time_t t) {
  // External events, such as pushing a button of a remote infrared commander
#if 0
  QTMWidget *tm_focus = qobject_cast<QTMWidget*>(qApp->focusWidget());
  if (tm_focus) {
    simple_widget_rep *wid = tm_focus->tm_widget();
    if (wid) the_gui -> process_keypress (wid, type, t);
  }
#endif
}

font x_font (string family, int size, int dpi)
{
  (void) family; (void) size; (void) dpi;
  if (DEBUG_EVENTS) cout << "x_font(): SHOULD NOT BE CALLED\n";
  return NULL;
}

