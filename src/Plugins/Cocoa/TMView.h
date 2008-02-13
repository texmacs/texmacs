
/******************************************************************************
* MODULE     : TMView.h
* DESCRIPTION: Main TeXmacs view
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "mac_cocoa.h"
#include "aqua_simple_widget.h"

@interface TMSimpleView : NSView {
	simple_widget_rep *wid;
}
- (void) setWidget:(widget_rep*) w;
- (widget_rep*) widget;
@end

@interface TMView : TMSimpleView  <NSTextInput>
{
  NSString *workingText;
  BOOL processingCompose;
}
- (void) deleteWorkingText;
@end
