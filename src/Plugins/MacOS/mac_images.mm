
/******************************************************************************
* MODULE     : mac_images.mm
* DESCRIPTION: interface with the MacOSX image conversion facilities
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifdef OS_MACOS_DISACTIVATED
#include "MacOS/mac_images.h"
#include "converter.hpp" // hack: remove as soon as possible

#include "Cocoa/mac_cocoa.h"

static NSString *
to_nsstring_utf8 (string s) {
  s= cork_to_utf8 (s);
  char *p = as_charp(s);
  NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
  tm_delete_array (p);	
  return nss;
}

void mac_image_to_png (url img_file, url png_file) {
  // we need to be sure that the Cocoa application infrastructure is initialized 
  // (apparently Qt does not do this properly and the NSSpellChecker instance returns null
  //  without the following instruction)
  NSApplication *NSApp=[NSApplication sharedApplication]; (void) NSApp;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSImage *image = [[NSImage alloc] initWithContentsOfFile: to_nsstring_utf8 ( concretize (img_file) )];
  NSSize size = [image size];
  [image lockFocus];
  NSBitmapImageRep *bmp = [[NSBitmapImageRep alloc] initWithFocusedViewRect:
                          NSMakeRect(0,0,size.width,size.height)];
  [image unlockFocus];
  [image release];
  NSData *png_data = [bmp representationUsingType: NSPNGFileType properties: nil ];
  [png_data writeToURL:[NSURL fileURLWithPath: to_nsstring_utf8 ( concretize (png_file))] atomically: YES];
  [bmp release];
  [pool release];
}

#endif // OS_MACOS_DISACTIVATED
