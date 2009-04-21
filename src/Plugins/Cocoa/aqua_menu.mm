
/******************************************************************************
* MODULE     : aqua_menu.h
* DESCRIPTION: Aqua menu proxies
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
#include "aqua_menu.h"
#include "aqua_utilities.h"
//#include "aqua_renderer.h"
#include "aqua_renderer.h"
#include "aqua_simple_widget.h"
#include "aqua_basic_widgets.h"

#include "widget.hpp" 
#include "message.hpp"

#include "promise.hpp"
#import "TMView.h"

NSMenu *alloc_menu() { return [NSMenu alloc]; }
NSMenuItem *alloc_menuitem() { return [NSMenuItem alloc]; }

class aqua_menu_rep : public aqua_widget_rep  {
public:
	NSMenuItem *item;
	aqua_menu_rep(NSMenuItem* _item) : item(_item) { [item retain]; }
	~aqua_menu_rep()  { [item release]; }

	virtual void send (slot s, blackbox val);
	virtual widget make_popup_widget (); 
	virtual widget popup_window_widget (string s); 

  virtual TMMenuItem *as_menuitem() { return (TMMenuItem *)item; }

};

widget aqua_menu_rep::make_popup_widget ()
{
	return this;
}

widget aqua_menu_rep::popup_window_widget (string s)
{
	[item setTitle:to_nsstring(s)];
	return this;
}


void aqua_menu_rep::send (slot s, blackbox val) {
  switch (s) {
  case SLOT_POSITION:
    {
      ASSERT (type_box (val) == type_helper<coord2>::id, "type mismatch");
    }
    break;
  case SLOT_VISIBILITY:
    {	
      check_type<bool> (val, "SLOT_VISIBILITY");
      bool flag = open_box<bool> (val);
    }	
    break;
  case SLOT_MOUSE_GRAB:
    {	
      check_type<bool> (val, "SLOT_MOUSE_GRAB");
      bool flag = open_box<bool> (val);
      [NSMenu popUpContextMenu:[item submenu] withEvent:[NSApp currentEvent] forView:( (aqua_view_widget_rep*)(the_keyboard_focus.rep))->view ];
    }	
    //			send_mouse_grab (THIS, val);
    break;
    
  default:
    FAILED ("cannot handle slot type");
  }
}

#if 0
class aqua_menu_text_rep : public aqua_basic_widget_rep {
public:
	NSString *text;
	aqua_menu_text_rep(NSString* _text) : text(_text) { [text retain]; }
	~aqua_menu_text_rep()  { [text release]; }
};

#endif


@interface TMMenuItem : NSMenuItem
{
	command_rep *cmd;
	simple_widget_rep* wid;// an eventual box widget (see tm_button.cpp)
}
- (void)setCommand:(command_rep *)_c;
- (void)setWidget:(simple_widget_rep *)_w;
- (void)doit;
@end

@implementation TMMenuItem
- (void)setCommand:(command_rep *)_c 
{  
	if (cmd) { DEC_COUNT_NULL(cmd); } cmd = _c; 
	if (cmd) {
		INC_COUNT_NULL(cmd);
	  [self setAction:@selector(doit)];
	  [self setTarget:self];
	}
}
- (void)setWidget:(simple_widget_rep *)_w
{  
	if (wid) { DEC_COUNT_NULL(wid); } wid = _w; 
	if (wid) {
		INC_COUNT_NULL(wid);
	}
}
- (void)dealloc { [self setCommand:NULL];  [self setWidget:NULL];  [super dealloc]; }
- (void)doit {	if (cmd) cmd->apply(); }


- (NSImage*) image
{
  NSImage *img = [super image];
  if ((!img)&&(wid))
  {
    SI width, height;
    wid->handle_get_size_hint (width,height);
    NSSize s = NSMakeSize(width/PIXEL,height/PIXEL);
    
    img = [[[NSImage alloc] initWithSize:s] autorelease];
    [img lockFocus];
    
    basic_renderer r = the_aqua_renderer();
    int x1 = 0;
    int y1 = s.height;
    int x2 = s.width;
    int y2 = 0;
    
    r -> begin([NSGraphicsContext currentContext]);
    
    r -> encode (x1,y1);
    r -> encode (x2,y2);
    r -> set_clipping (x1,y1,x2,y2);
    wid -> handle_repaint (x1,y1,x2,y2);
    r->end();
    [img unlockFocus];
    //[img setFlipped:YES];
    [super setImage:img];			
    [self setWidget:NULL];
  }
  return img;
}
@end



@interface TMLazyMenu : NSMenu
{
	promise_rep<widget> *pm;
	BOOL forced;
}
- (void)setPromise:(promise_rep<widget> *)p;
@end

@implementation TMLazyMenu
- (void)setPromise:(promise_rep<widget> *)p 
{ 
	if (pm) { DEC_COUNT_NULL(pm); }  pm = p;  INC_COUNT_NULL(pm); 
	forced = NO;
	[self setDelegate:self];
}
- (void)dealloc { [self setPromise:NULL]; [super dealloc]; }

- (void)menuNeedsUpdate:(NSMenu *)menu
{
	if (!forced) {
		widget w = pm->eval();
		aqua_menu_rep *wid = (aqua_menu_rep*)(w.rep); 
		NSMenu *menu2 = [wid->item submenu];
		unsigned count = [menu2 numberOfItems];
		for (unsigned j=0; j<count; j++)
		{
			NSMenuItem *itm = [[[menu2 itemAtIndex:0] retain] autorelease];
			[menu2 removeItem:itm];
			[menu insertItem:itm atIndex:j];
		}
		DEC_COUNT_NULL(pm); pm = NULL;
		forced = YES;
	}
}

- (BOOL)menuHasKeyEquivalent:(NSMenu *)menu forEvent:(NSEvent *)event target:(id *)target action:(SEL *)action
{
	return NO;
	// disable keyboard handling for lazy menus
}
@end



/******************************************************************************
 * Widgets for the construction of menus
 ******************************************************************************/

widget horizontal_menu (array<widget> a) 
// a horizontal menu made up of the widgets in a
{
	NSMenuItem* mi = [[alloc_menuitem() initWithTitle:@"Menu" action:NULL keyEquivalent:@""] autorelease];
	NSMenu *menu = [[alloc_menu() init] autorelease];
	for(int i = 0; i < N(a); i++) {
		if (is_nil(a[i])) 
			break;
      [menu addItem: concrete(a[i])->as_menuitem()];
	};
	[mi setSubmenu:menu];
	return tm_new <aqua_menu_rep> (mi);	
}

widget vertical_menu (array<widget> a) { return horizontal_menu(a); }
// a vertical menu made up of the widgets in a


@interface TMTileView : NSMatrix
{
	int cols;
}
- (id) initWithObjects:(NSArray*)objs cols:(int)_cols;
- (void) click:(TMTileView*)tile;
@end


@implementation TMTileView
- (void) dealloc
{
	[super dealloc];
}
- (id) initWithObjects:(NSArray*)objs cols:(int)_cols
{
	self = [super init];
	if (self != nil) {
		int current_col;
		int current_row;
		cols = _cols;
		current_col = cols;
		current_row = -1;
		[self setCellSize:NSMakeSize(20,20)];
		[self renewRows:0 columns:cols];
    NSEnumerator *en = [objs objectEnumerator];
		NSMenuItem *mi;
		while ((mi = [en nextObject])) {
			if (current_col == cols) {
				current_col=0; current_row++;
				[self addRow];
			}
			NSImageCell *cell = [[[NSImageCell alloc] initImageCell:[mi image]] autorelease];
	//		[cell setImage:[mi image]];
			[cell setRepresentedObject:mi];
			[self putCell:cell atRow:current_row column:current_col];
			current_col++;			
		}
		
		[self setTarget:self];
		[self setAction:@selector(click:)];
		[self sizeToCells];
	}
	return self;
}

- (void) click:(TMTileView*)tile
{
	// on mouse up, we want to dismiss the menu being tracked
	NSMenuItem* mi = [self enclosingMenuItem];
	//[[mi menu] performSelector:@selector(cancelTracking) withObject:nil afterDelay:0.0];
	[[mi menu] cancelTracking];
  TMMenuItem* item =  [(NSCell*)[self selectedCell]  representedObject];
//	[item performSelector:@selector(doit) withObject:nil afterDelay:0.0];
	[item doit];
}

#if 0
- (void)mouseDown:(NSEvent*)event
{
	[super mouseDown:event];	
	// on mouse up, we want to dismiss the menu being tracked
	NSMenu* menu = [[self enclosingMenuItem] menu];
	[menu cancelTracking];
	
}
#endif
@end





widget tile_menu (array<widget> a, int cols) 
// a menu rendered as a table of cols columns wide & made up of widgets in a
{ 
//	return horizontal_menu(a); 
	NSMutableArray *tiles = [NSMutableArray arrayWithCapacity:N(a)];
	for(int i = 0; i < N(a); i++) {
		if (is_nil(a[i]))  break;
//		[tiles addObject:( (aqua_menu_rep*)(a[i].rep))->item];
      [tiles addObject:concrete(a[i])->as_menuitem()];
	};
	TMTileView* tv = [[[TMTileView alloc] initWithObjects:tiles cols:cols] autorelease];
	
	NSMenuItem* mi = [[[NSMenuItem alloc] initWithTitle:@"Tile" action:NULL keyEquivalent:@""] autorelease];

	
	[mi setView:tv];
	return tm_new <aqua_menu_rep> (mi);
	
}



widget menu_separator (bool vertical) { return tm_new <aqua_menu_rep> ([NSMenuItem separatorItem]); }
// a horizontal or vertical menu separator
widget menu_group (string name, string lan) 
// a menu group; the name should be greyed and centered
{
	NSMenuItem* mi = [[alloc_menuitem() initWithTitle:to_nsstring_utf8(name) action:NULL keyEquivalent:@""] autorelease];

	//	NSAttributedString *str = [mi attributedTitle];
	NSMutableParagraphStyle *style = [[[NSParagraphStyle defaultParagraphStyle] mutableCopy] autorelease];
//	NSMutableParagraphStyle *style = [(NSParagraphStyle*)[str attribute:NSParagraphStyleAttributeName atIndex:0 effectiveRange:NULL] mutableCopy];
	[style setAlignment: NSCenterTextAlignment];
	[mi setAttributedTitle:[[[NSAttributedString alloc] 
                               initWithString: [mi title]
                                   attributes: [NSDictionary 
                                      dictionaryWithObjectsAndKeys:style, NSParagraphStyleAttributeName, nil]]
                                autorelease]];
	return tm_new <aqua_menu_rep> (mi);
}

widget pulldown_button (widget w, promise<widget> pw) 
// a button w with a lazy pulldown menu pw
{
//	NSString *title = (is_nil(w)? @"":((aqua_menu_text_rep*)w.rep)->text);
//	NSMenuItem *mi = [[alloc_menuitem() initWithTitle:title action:NULL keyEquivalent:@""] autorelease];
//	TMMenuItem* mi =  (TMMenuItem*)((aqua_menu_rep*)w.rep) -> item;
  TMMenuItem* mi = concrete(w) -> as_menuitem();
   
  TMLazyMenu *lm = [[[TMLazyMenu alloc] init] autorelease];
	[lm setPromise:pw.rep];
	[mi setSubmenu: lm];
	return tm_new <aqua_menu_rep> (mi);
}

widget pullright_button (widget w, promise<widget> pw)
// a button w with a lazy pullright menu pw
{
	return pulldown_button(w, pw);
}


TMMenuItem * aqua_text_widget_rep::as_menuitem()
{
  return [[[TMMenuItem alloc] initWithTitle:to_nsstring_utf8(str) action:NULL keyEquivalent:@""] autorelease];
}

TMMenuItem * aqua_image_widget_rep::as_menuitem()
{
#if 0
  CGImageRef cgi = the_aqua_renderer()->xpm_image(image);
  NSImage *img = [[[NSImage alloc] init] autorelease];
  [img addRepresentation:[[NSBitmapImageRep alloc ] initWithCGImage: cgi]];
#else
  NSImage *img = the_aqua_renderer()->xpm_image(image);
#endif
  //	TMMenuItem *mi = [[[TMMenuItem alloc] initWithTitle:to_nsstring(as_string(file_name)) action:NULL keyEquivalent:@""] autorelease];
  TMMenuItem *mi = [[[TMMenuItem alloc] initWithTitle:@"" action:NULL keyEquivalent:@""] autorelease];
  [mi setRepresentedObject:img];
  [mi setImage:img];
  
  return  mi;
}

TMMenuItem * aqua_balloon_widget_rep::as_menuitem()
{
  TMMenuItem *mi = ((aqua_widget_rep*)text.rep)->as_menuitem();
  [mi setToolTip:to_nsstring(((aqua_text_widget_rep*)hint.rep)->str)];
  return mi;
}


widget menu_button (widget w, command cmd, string pre, string ks, bool ok) 
// a command button with an optional prefix (o, * or v) and
// keyboard shortcut; if ok does not hold, then the button is greyed
{
  TMMenuItem *mi = nil;
  
  if (typeid(*(w.rep)) == typeid(simple_widget_rep)) {
    mi = [[[TMMenuItem alloc] init] autorelease];
    [mi setWidget:(simple_widget_rep*)w.rep];
  } else  {
    mi = ((aqua_widget_rep*)w.rep)->as_menuitem();
  }

  [mi setCommand: cmd.rep];
  [mi setEnabled:(ok ? YES : NO)];
	// FIXME: implement complete prefix handling and keyboard shortcuts
	// cout << "ks: "<< ks << "\n";
	[mi setState:(pre!="" ? NSOnState: NSOffState)];
	if (pre == "v") {
	} else if (pre == "*") {
//		[mi setOnStateImage:[NSImage imageNamed:@"TMStarMenuBullet"]];
	} else if (pre == "o") {
	}
	return tm_new <aqua_menu_rep> (mi);
}

widget balloon_widget (widget w, widget help) 
// given a button widget w, specify a help balloon which should be displayed
// when the user leaves the mouse pointer on the button for a small while
{ 
  return tm_new <aqua_balloon_widget_rep> (w,help);
}

widget text_widget (string s, color col, bool tsp, string lan) 
// a text widget with a given color, transparency and language
{
  string t= aqua_translate (s);
  return tm_new <aqua_text_widget_rep> (t,col,tsp,lan);
}
widget xpm_widget (url file_name)// { return widget(); }
// a widget with an X pixmap icon
{
  return tm_new <aqua_image_widget_rep> (file_name);
#if 0  
	NSImage *image = the_aqua_renderer()->xpm_image(file_name);
//	TMMenuItem *mi = [[[TMMenuItem alloc] initWithTitle:to_nsstring(as_string(file_name)) action:NULL keyEquivalent:@""] autorelease];
	TMMenuItem *mi = [[[TMMenuItem alloc] initWithTitle:@"" action:NULL keyEquivalent:@""] autorelease];
	[mi setRepresentedObject:image];
	[mi setImage:image];
  return tm_new <aqua_menu_rep> (mi);
//	return new aqua_menu_text_rep(to_nsstring(as_string(file_name)));
#endif
}

NSMenu* to_nsmenu(widget w)
{
  if (typeid(*w.rep) == typeid(aqua_menu_rep)) {
    aqua_menu_rep *ww = ((aqua_menu_rep*)w.rep);
	NSMenu *m =[[[ww->item submenu] retain] autorelease];
	[ww->item setSubmenu:nil];
	return m;
  }
  else return nil;
}

NSMenuItem* to_nsmenuitem(widget w)
{
	return ((aqua_menu_rep*)w.rep)->item;
}

 TMMenuItem *simple_widget_rep::as_menuitem()
{
  TMMenuItem *mi = [[[TMMenuItem alloc] init] autorelease];
  [mi setWidget:this];
  return mi;
}

