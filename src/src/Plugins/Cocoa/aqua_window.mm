
/******************************************************************************
* MODULE     : aqua_window.mm
* DESCRIPTION: Aqua window class
* COPYRIGHT  : (C) 2007 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "aqua_window.h"

aqua_window_rep::aqua_window_rep (widget wid2) : window_rep(), aqua_renderer_rep(the_gui), wc(nil), wid(wid2) 
{
  NSRect screen_frame = [[NSScreen mainScreen] visibleFrame];

  NSWindow *nsw = [[[NSWindow alloc] initWithContentRect:NSMakeRect(0,0,100,100) 
                                              styleMask:NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask | NSResizableWindowMask
                                                backing:NSBackingStoreBuffered
                                                  defer:NO] autorelease];
  wc = [[NSWindowController alloc] initWithWindow:nsw];
  NSView *view = ((aqua_widget_rep*)wid.rep)->get_nsview();
  [nsw setContentView:view];
}

aqua_window_rep::~aqua_window_rep () { wid = NULL; [wc release]; }

widget aqua_window_rep::get_widget () { return wid; }
    // Get the top widget associated to the window
  
void aqua_window_rep::set_name (string name) 
    // Set the window title
{
  char *p = as_charp(name);
  [[wc window] setTitle:[NSString stringWithCString:p encoding:NSASCIIStringEncoding]];
  delete[] p;
} 

void aqua_window_rep::set_visibility (bool flag) 
    // Map or unmap the window
{
 if (flag)
   [[wc window] makeKeyAndOrderFront:nil] ;
 else 
   [[wc window] orderOut:nil]  ;
}

void aqua_window_rep::set_full_screen (bool flag) {}
    // Set or reset full screen mode

void aqua_window_rep::set_size (SI w, SI h) 
    // Resize the window
{
//  h=-h; decode (w, h);
 // [[controller window] setContentSize:NSMakeSize(w/PIXEL,h/PIXEL)];
  [[wc window] setContentSize:NSMakeSize(w/PIXEL,h/PIXEL)];
}

void aqua_window_rep::get_size (SI& w, SI& h) 
    // Get the current size of the window
{
  NSRect bounds = [[[wc window] contentView] frame];
  w = bounds.size.width*PIXEL;
  h = bounds.size.height*PIXEL;
}

void aqua_window_rep::set_position (SI x, SI y) 
    // Move the window
{
  NSRect screen_frame = [[NSScreen mainScreen] visibleFrame];
  NSRect cont_bounds = [[[wc window] contentView] frame];
  
  x= x/PIXEL;
  y= -y/PIXEL;
//  if ((x+ win_w) > dw) x= dw- win_w;
//  if (x<0) x=0;
//  if ((y+ win_h) > dh) y= dh - win_h;
//  if (y<0) y=0;
  [[wc window] setFrameOrigin:NSMakePoint(x,screen_frame.size.height-(y+cont_bounds.size.height))];
}

void aqua_window_rep::get_position (SI& x, SI& y) 
    // Get the current position of the window on the screen
{
  NSRect screen_frame = [[NSScreen mainScreen] visibleFrame];
  NSRect bounds = [[wc window] frame];
  NSRect cont_bounds = [[[wc window] contentView] frame];
  x = bounds.origin.x*PIXEL;
  y = -(screen_frame.size.height-(bounds.origin.y+cont_bounds.size.height))*PIXEL;
} 

void aqua_window_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {}
    // Explicit request for redrawing a region in the window

void aqua_window_rep::translate (SI x1, SI y1, SI x2, SI y2, SI dx, SI dy) {}
    // Fast translation of a region in the window (used for scrolling)

void aqua_window_rep::set_keyboard_focus (widget wid, bool get_focus) {}
    // Obtain or release the keyboard focus

bool aqua_window_rep::get_keyboard_focus (widget wid) { fatal_error("not implemented","aqua_window_rep::get_keyboard_focus"); return false; }
    // Does this widget have the keyboard focus

void aqua_window_rep::set_mouse_grab (widget wid, bool get_grab) {}
    // Obtain or release the mouse grab. Recursive grabs are stored on
    // a stack: if w1 and w2 successively grab the mouse and w2 releases
    // the mouse grab, then w1 reobtains the mouse grab. Enter and leave
    // mouse events are also issued when obtaining or loosing the grab.

bool aqua_window_rep::get_mouse_grab (widget wid) { fatal_error("not implemented","aqua_window_rep::get_mouse_grab"); return false; }
    // Does this widget have the mouse grab?

void aqua_window_rep::set_mouse_pointer (widget wid, string name, string mask) {}
    // Set the shape of the mouse pointer with a mask ("" means no mask)

void aqua_window_rep::delayed_message (widget wid, string message, time_t delay) {}
    // Send message to wid for reception after delay (used for scrolling)

void aqua_window_rep::begin_draw()  {}
void aqua_window_rep::end_draw()  {}
