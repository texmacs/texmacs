
/******************************************************************************
* MODULE     : aqua_gui.mm
* DESCRIPTION: Cocoa display class
* COPYRIGHT  : (C) 2006 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/


#include "iterator.hpp"
#include "dictionary.hpp"
#include "aqua_gui.h"
#include "analyze.hpp"
#include <locale.h>
#include "language.hpp"
#include "message.hpp"

extern hashmap<id, pointer> NSWindow_to_window;
extern window (*get_current_window) (void);

aqua_gui_rep* the_gui= NULL;

int nr_windows = 0; // FIXME: fake variable, referenced in tm_server

/******************************************************************************
* Aqua images
******************************************************************************/


aqua_image::aqua_image (NSImage *img2, SI xo2, SI yo2, int w2, int h2) :
rep (new aqua_image_rep(img2,xo2,yo2,w2,h2)) {}
//aqua_image::aqua_image () : rep(NULL) {}

aqua_image_rep::aqua_image_rep (NSImage *img2, SI xo2, SI yo2, int w2, int h2) :
img(img2), xo(xo2), yo(yo2), w(w2), h(h2) { [img retain]; }

aqua_image_rep::~aqua_image_rep() { [img release]; }

/******************************************************************************/





aqua_gui_rep::aqua_gui_rep(int argc2, char **argv2)
: color_scale ((void*) NULL), selection(NULL),
character_image (aqua_image()), interrupted(false), images (aqua_image())
{
//  argc               = argc2;
//  argv               = argv2;
  interrupted        = false;
  interrupt_time     = texmacs_time ();
  
  //get_xmodmap ();
  initialize_colors ();
  set_output_language (get_locale_language ());
  // out_lan= get_locale_language ();
  (void) default_font ();
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
* Set up colors
******************************************************************************/

bool reverse_colors= false;

color black, white, red, green, blue;
color yellow, magenta, orange, brown, pink;
color light_grey, grey, dark_grey;

static int CSCALES= 4;
static int CFACTOR= 5;
static int GREYS  = 16;
static int CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);



static NSColor* alloc_color (int r, int g, int b) {
  if (reverse_colors) {
    int m= min (r, min (g, b));
    int M= max (r, max (g, b));
    int t= (r + g + b) / 3;
    int tt= 65535 - t;
    double mu= 1.0;
    tt= 6 * tt / 7;
    if (M != m) {
      double lambda1= max (((double) (t - m)) / t,
                           ((double) (M - t)) / (65535 - t));
      double lambda2= max (((double) (t - m)) / tt,
                           ((double) (M - t)) / (65535 - tt));
      mu= lambda1 / lambda2;
    }
    r= (int) (tt + mu * (r - t) + 0.5);
    g= (int) (tt + mu * (g - t) + 0.5);
    b= (int) (tt + mu * (b - t) + 0.5);
  }
  
  return [[NSColor colorWithDeviceRed:(r/65535.0) green:(g/65535.0) blue:(b/65535.0) alpha:1.0] retain];
}

void
aqua_gui_rep::init_color_map () {
  int i, r, g, b;
  
  cmap= new NSColor* [CTOTAL];
  
  for (i=0; i<=GREYS; i++)
    cmap[i]= alloc_color ((i*65535)/GREYS, (i*65535)/GREYS, (i*65535)/GREYS);
  
  for (r=0; r<=CSCALES; r++)
    for (g=0; g<=CSCALES; g++)
      for (b=0; b<=CSCALES; b++) {
        i= r*CFACTOR*CFACTOR+ g*CFACTOR+ b+ GREYS+ 1;
        cmap[i]= alloc_color ((r*65535)/CSCALES,
                              (g*65535)/CSCALES,
                              (b*65535)/CSCALES);
      }
}

color rgb_color (int r, int g, int b) {
  if ((r==g) && (g==b)) return (r*GREYS+ 128)/255;
  else {
    r= (r*CSCALES+ 128)/255;
    g= (g*CSCALES+ 128)/255;
    b= (b*CSCALES+ 128)/255;
    return r*CFACTOR*CFACTOR+ g*CFACTOR+ b+ GREYS+ 1;
  }
}

void get_rgb_color (color col, int& r, int& g, int& b) {
  if (col <= GREYS) {
    r= (col*255)/GREYS;
    g= (col*255)/GREYS;
    b= (col*255)/GREYS;
  }
  else {
    int rr, gg, bb;
    col-= (GREYS+1);
    bb  = col % CFACTOR;
    gg  = (col/CFACTOR) % CFACTOR;
    rr  = (col/(CFACTOR*CFACTOR)) % CFACTOR;
    r   = (rr*255)/CSCALES;
    g   = (gg*255)/CSCALES;
    b   = (bb*255)/CSCALES;
  }
}

color named_color (string s) {
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return rgb_color (r, g, b);
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return rgb_color (r, g, b);
  }
  unsigned int depth = 65535;
  int pastel= (depth>=16? 223: 191);
  
  if ((N(s) > 4) && (s (1,4) == "gray") && (is_numeric (s (5,N(s)) ))) {
	int level, i=5;
	if (read_int(s,i,level)) {
		level = (level*255) /100;
		return rgb_color(level,level,level);
	}
  }
  
  
  if (s == "black")          return black;
  if (s == "white")          return white;
  if (s == "grey")           return grey;
  if (s == "red")            return red;
  if (s == "blue")           return blue;
  if (s == "yellow")         return yellow;
  if (s == "green")          return green;
  if (s == "magenta")        return magenta;
  if (s == "cyan")           return rgb_color (0, 255, 255);
  if (s == "orange")         return orange;
  if (s == "brown")          return brown;
  if (s == "pink")           return pink;
  if (s == "broken white")   return rgb_color (255, 255, pastel);
  if (s == "light grey")     return light_grey;
  if (s == "dark grey")      return dark_grey;
  if (s == "dark red")       return rgb_color (128, 0, 0);
  if (s == "dark blue")      return rgb_color (0, 0, 128);
  if (s == "dark yellow")    return rgb_color (128, 128, 0);
  if (s == "dark green")     return rgb_color (0, 128, 0);
  if (s == "dark magenta")   return rgb_color (128, 0, 128);
  if (s == "dark cyan")      return rgb_color (0, 128, 128);
  if (s == "dark orange")    return rgb_color (128, 64, 0);
  if (s == "dark brown")     return rgb_color (64, 16, 0);
  if (s == "pastel grey")    return rgb_color (pastel, pastel, pastel);
  if (s == "pastel red")     return rgb_color (255, pastel, pastel);
  if (s == "pastel blue")    return rgb_color (pastel, pastel, 255);
  if (s == "pastel yellow")  return rgb_color (255, 255, pastel);
  if (s == "pastel green")   return rgb_color (pastel, 255, pastel);
  if (s == "pastel magenta") return rgb_color (255, pastel, 255);
  if (s == "pastel cyan")    return rgb_color (pastel, 255, 255);
  if (s == "pastel orange")  return rgb_color (255, pastel, 2*pastel-255);
  if (s == "pastel brown")   return rgb_color (pastel, 2*pastel-255, 2*pastel-255);
  return black;
}

string
get_named_color (color c) {
  SI r, g, b;
  get_rgb_color (c, r, g, b);
  return "#" *
    as_hexadecimal (r, 2) *
    as_hexadecimal (g, 2) *
    as_hexadecimal (b, 2);
}


void
aqua_gui_rep::initialize_colors () {
  unsigned int depth = 65535;
  if (depth >= 16) {
    CSCALES= 8;
    CFACTOR= 9;
    GREYS  = 256;
    CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);
  }
  
  init_color_map ();
  
  black   = rgb_color (0, 0, 0);
  white   = rgb_color (255, 255, 255);
  red     = rgb_color (255, 0, 0);
  blue    = rgb_color (0, 0, 255);
  yellow  = rgb_color (255, 255, 0);
  green   = rgb_color (0, 255, 0);
  magenta = rgb_color (255, 0, 255);
  orange  = rgb_color (255, 128, 0);
  brown   = rgb_color (128, 32, 0);
  pink    = rgb_color (255, 128, 128);
  
  light_grey = rgb_color (208, 208, 208);
  grey       = rgb_color (184, 184, 184);
  dark_grey  = rgb_color (112, 112, 112);
}


/******************************************************************************
* Server fonts
******************************************************************************/

static string the_default_display_font ("");
font the_default_wait_font;

void aqua_gui_rep::set_default_font (string name) {
  the_default_display_font= name;
}

font aqua_gui_rep::default_font_sub (bool tt) {
  string s= the_default_display_font;
  if (s == "") s= "ecrm11@300";
  int i, j, n= N(s);
  for (j=0; j<n; j++) if ((s[j] >= '0') && (s[j] <= '9')) break;
  string fam= s (0, j);
  for (i=j; j<n; j++) if (s[j] == '@') break;
  int sz= (j<n? as_int (s (i, j)): 10);
  if (j<n) j++;
  int dpi= (j<n? as_int (s (j, n)): 300);
  if (N(fam) >= 2) {
    string ff= fam (0, 2);
    string out_lan= get_output_language ();

    if (((out_lan == "bulgarian") || (out_lan == "russian") ||
         (out_lan == "ukrainian")) &&
        ((ff == "cm") || (ff == "ec"))) {
      fam= "la" * fam (2, N(fam)); ff= "la"; if (sz<100) sz *= 100; }
    if (out_lan == "japanese" || out_lan == "korean") {
      tree modern_fn= tuple ("modern", "ss", "medium", "right");
      modern_fn << as_string (sz) << as_string (dpi);
      return find_font (modern_fn);
    }
    if (out_lan == "chinese" || out_lan == "taiwanese")
      return unicode_font ( "fireflysung", sz, dpi);
    //if (out_lan == "japanese")
    //return unicode_font (this, "ipagui", sz, dpi);
    //if (out_lan == "korean")
    //return unicode_font (this, "UnDotum", sz, dpi);
    if (ff == "ec")
      return tex_ec_font ( tt? ff * "tt": fam, sz, dpi);
    if (ff == "la")
      return tex_la_font ( tt? ff * "tt": fam, sz, dpi, 1000);
    if (ff == "pu") tt= false;
    if ((ff == "cm") || (ff == "pn") || (ff == "pu"))
      return tex_cm_font (tt? ff * "tt": fam, sz, dpi);
  }
  return tex_font ( fam, sz, dpi);
  // if (out_lan == "german") return tex_font (this, "ygoth", 14, 300, 0);
  // return tex_font (this, "rpagk", 10, 300, 0);
  // return tex_font (this, "rphvr", 10, 300, 0);
  // return ps_font (this, "b&h-lucidabright-medium-r-normal", 11, 300);
}

font aqua_gui_rep::default_font (bool tt) {
  font fn= default_font_sub (tt);
  the_default_wait_font= fn;
  return fn;
}


void aqua_gui_rep::load_system_font (string family, int size, int dpi,
                                        font_metric& fnm, font_glyphs& fng) {} ;


/* interclient communication */

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
    if (selection!=NULL) delete[] selection;
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
    delete[] selection;
	// FIXME: should we do something with the pasteboard?
    selection= NULL;
  }
}





/* miscellaneous */
void aqua_gui_rep::image_gc (string name) {} ;
void aqua_gui_rep::set_mouse_pointer (string name) {} ;
void aqua_gui_rep::set_mouse_pointer (string curs_name, string mask_name) {} ;

static bool check_mask(int mask)
{
  NSEvent * event = [NSApp nextEventMatchingMask:mask
                             untilDate:nil
                                inMode:NSDefaultRunLoopMode 
                               dequeue:NO];
 // if (event != nil) NSLog(@"%@",event);
  return (event != nil);
  
}

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
  while (1) {
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
    if (!event)  update();
    [pool release];
  }  
	}
}
#endif



/******************************************************************************
* Making color scales for anti alised fonts
******************************************************************************/

x_character_rep::x_character_rep (
                                  int c2, font_glyphs fng2, int sf2, color fg2, color bg2):
c (c2), fng (fng2), sf (sf2), fg (fg2), bg (bg2) {}

x_character::x_character (int c, font_glyphs fng, int sf, color fg, color bg):
rep (new x_character_rep (c, fng, sf, fg, bg)) {}

x_character::operator tree () {
  tree t (TUPLE,  as_string (rep->c), rep->fng->res_name);
  t << as_string (rep->sf) << as_string (rep->fg) << as_string (rep->bg);
  return t; }

bool operator == (x_character xc1, x_character xc2) {
  return
  (xc1->c==xc2->c) && (xc1->fng.rep==xc2->fng.rep) &&
  (xc1->sf==xc2->sf) && (xc1->fg==xc2->fg) && (xc1->bg==xc2->bg); }

bool operator != (x_character xc1, x_character xc2) {
  return
  (xc1->c!=xc2->c) || (xc1->fng.rep!=xc2->fng.rep) ||
  (xc1->sf!=xc2->sf) || (xc1->fg!=xc2->fg) || (xc1->bg!=xc2->bg); }

int hash (x_character xc) {
  return xc->c ^ ((intptr_t) xc->fng.rep) ^ xc->fg ^ xc->bg ^ xc->sf; }

void
aqua_gui_rep::prepare_color (int sf, color fg, color bg) {
  int nr_cols= sf*sf;
  if (nr_cols >= 64) nr_cols= 64;
  x_character col_entry (0, font_glyphs (), sf, fg, bg);
  color* cols= (color*) color_scale [col_entry];
  if (cols == NULL) {
    int fR, fG, fB, bR, bG, bB, j;
    get_rgb_color (fg, fR, fG, fB);
    get_rgb_color (bg, bR, bG, bB);
    cols= new color [nr_cols+1];
    for (j=0; j<=nr_cols; j++)
      cols [nr_cols-j]= rgb_color ((bR*j + fR*(nr_cols-j)) / nr_cols,
                             (bG*j + fG*(nr_cols-j)) / nr_cols,
                             (bB*j + fB*(nr_cols-j)) / nr_cols);
    color_scale (col_entry)= (void*) cols;
  }
}



aqua_gui_rep::~aqua_gui_rep() 
{ 
  /* release colormap */
  for(int i = 0; i< CTOTAL; i++) [this->cmap[i] release];
  delete [] this->cmap;
  
} 



/* interface ******************************************************************/
#pragma mark GUI interface

//static display cur_display= NULL;

static NSAutoreleasePool *pool = nil;
//static NSApplication *app = nil;



/******************************************************************************
* Main routines
******************************************************************************/

void gui_open (int argc2, char** argv2)
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
  
  the_gui = new aqua_gui_rep (argc2, argv2);
}

void gui_start_loop ()
  // start the main loop
{
  the_gui->event_loop ();
}

void gui_close ()
  // cleanly close the gui
{
  if (the_gui == NULL)
    fatal_error ("gui not yet open", "gui_close");
  [pool release];
  delete the_gui;
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
  // update and redraw all windows (e.g. on change of output language)
{
}



/******************************************************************************
* Font support
******************************************************************************/

void
set_default_font (string name) {
  // set the name of the default font
  the_gui->set_default_font (name);
}

font
get_default_font (bool tt) {
  // get the default font or monospaced font (if tt is true)
  return the_gui->default_font (tt);
}

  // load the metric and glyphs of a system font
  // you are not obliged to provide any system fonts

void
load_system_font (string family, int size, int dpi,
		  font_metric& fnm, font_glyphs& fng)
{
  the_gui->load_system_font (family, size, dpi, fnm, fng);
}

/******************************************************************************
* Clipboard support
******************************************************************************/

  // Copy a selection 't' with string equivalent 's' to the clipboard 'cb'
  // Returns true on success
bool
set_selection (string key, tree t, string s) {
  return the_gui->set_selection (key, t, s);
}

  // Retrieve the selection 't' with string equivalent 's' from clipboard 'cb'
  // Returns true on success; sets t to (extern s) for external selections
bool
get_selection (string key, tree& t, string& s) {
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

void beep ()
  // Issue a beep
{    NSBeep(); }

void needs_update () {}

bool check_event (int type)
  // Check whether an event of one of the above types has occurred;
  // we check for keyboard events while repainting windows
{ return the_gui->check_event(type); }

void image_gc (string name)
  // Garbage collect images of a given name (may use wildcards)
  // This routine only needs to be implemented if you use your own image cache
{ the_gui->image_gc(name); }

void show_help_balloon (widget balloon, SI x, SI y)
  // Display a help balloon at position (x, y); the help balloon should
  // disappear as soon as the user presses a key or moves the mouse
{  }

void show_wait_indicator (widget base, string message, string argument)
  // Display a wait indicator with a message and an optional argument
  // The indicator might for instance be displayed at the center of
  // the base widget which triggered the lengthy operation;
  // the indicator should be removed if the message is empty
{ the_gui->show_wait_indicator(base,message,argument);  }
