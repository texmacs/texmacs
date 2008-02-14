
/******************************************************************************
* MODULE     : mac_utilities.mm
* DESCRIPTION: Utilities for Mac OS X
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include <Cocoa/Cocoa.h>

void mac_fix_paths()
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  /* add appropriate TEXMACS_PATH to the current environment */
#if 0
  setenv("TEXMACS_PATH",
         [[[[NSBundle mainBundle] resourcePath] 
            stringByAppendingPathComponent:@"share/TeXmacs"] 
            cStringUsingEncoding:NSUTF8StringEncoding],
         1);
#endif
  /* add TeX directory */
  /* FIXME: make this user-defined */
  setenv("PATH",
         [[[NSString stringWithCString:getenv("PATH")] 
            stringByAppendingString:@":/usr/texbin"]
            cStringUsingEncoding:NSUTF8StringEncoding],
         1); 
  setenv("GUILE_LOAD_PATH","/opt/local/share/guile/1.8",1);
  system("printenv");
  [pool release];  
}
