
/******************************************************************************
* MODULE     : TMButtonsController.h
* DESCRIPTION: Controller for the widget bar
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#import <Cocoa/Cocoa.h>

@interface TMButtonsController : NSObject {
	NSMutableArray *menuArray;
	NSMutableArray *barArray;
	NSView *view;
	IBOutlet  NSSegmentedControl *prototype;
}
- (void) setMenu:(NSMenu *)menu forRow:(unsigned) idx;
- (void) layout;
- (NSView*) bar;
//- (void) buttonsAction:(NSMatrix*) mat;
- (NSSegmentedControl*) newSegment;

@end
