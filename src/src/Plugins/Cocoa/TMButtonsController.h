
/******************************************************************************
* MODULE     : TMButtonsController.h
* DESCRIPTION: Controller for the widget bar
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
