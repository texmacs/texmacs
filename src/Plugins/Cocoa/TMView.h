
/******************************************************************************
* MODULE     : TMView.h
* DESCRIPTION: Main TeXmacs view
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
#include "aqua_simple_widget.h"

@interface TMView : NSView  <NSTextInput>
{
	simple_widget_rep *wid;
  NSString *workingText;
  BOOL processingCompose;
  NSMutableArray *delayed_rects;
}
- (void) setWidget:(widget_rep*) w;
- (widget_rep*) widget;
- (void) deleteWorkingText;
@end
