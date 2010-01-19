
/******************************************************************************
* MODULE     : TMButtonsController.m
* DESCRIPTION: Controller for the widget bar
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#import "TMButtonsController.h"

@implementation TMButtonsController

- (id) init
{
	self = [super init];
	if (self != nil) {
		menuArray = [[NSMutableArray alloc] initWithCapacity:10];
		barArray = [[NSMutableArray alloc] initWithCapacity:10];
		view = [[NSView alloc] init];
	}
	return self;
}
- (void) dealloc
{
	[menuArray release];
	[barArray release];
	[view release];
	[super dealloc];
}

@protocol TMMenuItemDoit
- (void) doit;
@end

- (void) buttonsAction:(id) sc
{
  unsigned idx = [sc selectedSegment];
  NSArray *arr = [[sc cell] representedObject];
  NSMenuItem *mi = [arr objectAtIndex:idx];
  NSMenu *sm = [mi submenu];
  if (sm)
    [NSMenu popUpContextMenu:sm withEvent:[NSApp currentEvent] forView:sc];
  else if ([mi respondsToSelector:@selector(doit)]) [(id)mi doit];
}

- (NSSegmentedControl*) newSegment
{
  NSSegmentedControl *ret =  [[[NSSegmentedControl alloc] init] autorelease];
  [ret setSegmentStyle: NSSegmentStyleTexturedSquare];
  return ret; 
}

- (void)setMenu:(NSMenu *)menu forRow:(unsigned) idx
{
	unsigned count = [menuArray count];
	while (count <= idx) { 
		[menuArray addObject:[[[NSMenu alloc] init] autorelease]];  
      NSMutableArray *arr =  [[NSMutableArray alloc] initWithCapacity:10];
      [barArray addObject:arr];  
		count++; 
	}
	[menuArray replaceObjectAtIndex: idx withObject: menu];
	{
      NSMutableArray *arr = [barArray objectAtIndex:idx];
//      [arr removeAllObjects];
      unsigned nn = [arr count], mm = 0;
      unsigned  i,j;
      [menu addItem:[NSMenuItem separatorItem]];
      unsigned c = [menu numberOfItems];
      NSMutableArray *segs =  [[[NSMutableArray alloc] initWithCapacity:10] autorelease];
      for(i = 0; i < c; i++)
      {
        NSMenuItem *mi = [menu itemAtIndex:i];
        if (![mi isSeparatorItem]) [segs addObject:mi];
        else  if ([segs count]>0) {
          while (mm >= nn) {
            [arr addObject:[self newSegment]];
            nn++;
          }
          NSSegmentedControl *sc = [arr objectAtIndex:mm];
          mm++;
          [sc setSegmentCount:[segs count]];
          for(j=0; j<[segs count]; j++)
          {
            mi = [segs objectAtIndex:j];
            [sc setEnabled:YES forSegment:j];
            [sc setImage:[mi representedObject] forSegment:j];
            [sc setMenu:[mi submenu] forSegment:j];
            [mi setMenu:nil];
            [sc setLabel:nil forSegment:j];
            [sc setWidth:25.0 forSegment:j];
            [(NSSegmentedCell*)[sc cell] setToolTip:[mi toolTip] forSegment:j];
            
          }
          [(NSCell*)[sc cell] setRepresentedObject:segs];
          [[sc cell] setTrackingMode: NSSegmentSwitchTrackingMomentary];
          [sc sizeToFit];
          [sc setTarget: self];
          [sc setAction:@selector(buttonsAction:)];
          segs =  [[[NSMutableArray alloc] initWithCapacity:10] autorelease];
        }
        
      }
      while (mm < nn) {
        [(NSSegmentedControl*)[arr objectAtIndex:mm] removeFromSuperview];
        mm++;
      }
	}
  [self layout];
}

- (void) layout
{
  NSRect r = [view bounds];
  float totalHeight = 0.0;
  float totalWidth = 0.0;
  float baseX = r.origin.x;
  int i,j;
  //	r.origin.y += r.size.height;
  for (i=[barArray count]-1; i>= 0; i--) {
    r.origin.x = baseX;
    NSArray *arr = [barArray objectAtIndex:i];
    float currHeight = 0.0;
    for(j=0; j<((int) [arr count]); j++) {
      NSSegmentedControl *sc = [arr objectAtIndex:j];
      if (![sc superview]) [view addSubview:sc];
      r.size = [sc frame].size;
	  [sc setFrame:r];
      r = NSOffsetRect(r, r.size.width + 5.0, 0);            
      if (NSMaxX(r) > totalWidth) totalWidth = NSMaxX(r);
      if (r.size.height > currHeight) currHeight = r.size.height; 
    }
    r = NSOffsetRect(r, 0, currHeight);
    totalHeight += currHeight;    
  }
  r = [view frame];
  r.size.height = totalHeight;
  r.size.width = totalWidth;
  
  [view setFrame:r];
  [view setNeedsDisplay:YES];
}

- (NSView*) bar
{
	return view;
}

@end
