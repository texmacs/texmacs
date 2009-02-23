
/******************************************************************************
* MODULE     : aqua_utilities.mm
* DESCRIPTION: Utilities for Aqua
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "aqua_utilities.h"
#include "dictionary.hpp"
#include "converter.hpp"

NSRect to_nsrect(coord4 p)
{
	float c = 1.0/PIXEL;
	return NSMakeRect(p.x1*c, -p.x4*c, (p.x3-p.x1)*c, (p.x4-p.x2)*c);
}

NSPoint to_nspoint(coord2 p)
{
	float c = 1.0/PIXEL;
	return NSMakePoint(p.x1*c,-p.x2*c);
}

NSSize to_nssize(coord2 p)
{
	float c = 1.0/PIXEL;
	return NSMakeSize(p.x1*c,p.x2*c);
}

coord4 from_nsrect(NSRect rect)
{
	SI c1, c2, c3, c4;
	
	c1 = rect.origin.x*PIXEL;
	c2 = rect.origin.y*PIXEL;
	c3 = (rect.origin.x+rect.size.width)*PIXEL;
	c4 = (rect.origin.y+rect.size.height)*PIXEL;	
	return coord4 (c1, c2, c3, c4);
}

coord2 from_nspoint(NSPoint pt)
{
	SI c1, c2;
	c1 = pt.x*PIXEL;
	c2 = -pt.y*PIXEL;
	return coord2 (c1,c2)	;
}

coord2 from_nssize(NSSize s)
{
	SI c1, c2;
	c1 = s.width*PIXEL;
	c2 = s.height*PIXEL;
	return coord2 (c1,c2)	;
}

NSString *to_nsstring(string s)
{
	char *p = as_charp(s);
	NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
	tm_delete_array (p);	
	return nss;
}

string from_nsstring(NSString *s)
{
	const char *cstr = [s cStringUsingEncoding:NSUTF8StringEncoding];
	return utf8_to_cork(string((char*)cstr));
}


NSString *to_nsstring_utf8(string s)
{
  s= cork_to_utf8 (s);
	char *p = as_charp(s);
	NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
	tm_delete_array (p);	
	return nss;
}

string
aqua_translate (string s) {
  string out_lan= get_output_language ();
  return tm_var_encode (translate (s, "english", out_lan));
}
