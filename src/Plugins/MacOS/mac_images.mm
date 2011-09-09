
/******************************************************************************
* MODULE     : mac_images.mm
* DESCRIPTION: interface with the MacOSX image conversion facilities
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "MacOS/mac_images.h"
#include "converter.hpp" // hack: remove as soon as possible

#include "Cocoa/mac_cocoa.h"
#include "ApplicationServices/ApplicationServices.h"

static NSString *
to_nsstring_utf8 (string s) {
  s= cork_to_utf8 (s);
  char *p = as_charp(s);
  NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
  tm_delete_array (p);	
  return nss;
}

void mac_image_to_png (url img_file, url png_file, int w, int h) {
  // we need to be sure that the Cocoa application infrastructure is initialized 
  // (apparently Qt does not do this properly)
  NSApplication *NSApp=[NSApplication sharedApplication]; (void) NSApp;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSImage *image = [[NSImage alloc] initWithContentsOfFile: to_nsstring_utf8 ( concretize (img_file) )];
  NSSize size = NSMakeSize(w,h);
  [image setSize: size];
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

bool mac_image_size (url img_file, int& w, int& h) 
{
  string suf= suffix (img_file);
  if (suf == "ps" || suf == "eps" || suf == "pdf") return false;

  bool res = false; 
  // we need to be sure that the Cocoa application infrastructure is initialized 
  // (apparently Qt does not do this properly)
  NSApplication *NSApp=[NSApplication sharedApplication]; (void) NSApp;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  
  NSImage *image = [[NSImage alloc] initWithContentsOfFile: to_nsstring_utf8 ( concretize (img_file) )];
  if (image) {
    NSSize size = [image size];
    [image release];
    //NSLog(@"Probing  image size %f %f.\n", size.width, size.height);
    w = size.width;
    h = size.height;
    res = true;
  }
  [pool release];
  return res;
}

bool mac_supports (url img_file) {
  int w, h;
  return mac_image_size (img_file, w, h);
}

void mac_ps_to_pdf (url ps_file, url pdf_file) 
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSString *inpath = to_nsstring_utf8 ( concretize (ps_file) );
  NSString *outpath = to_nsstring_utf8 ( concretize (pdf_file) );
  NSURL *inurl = [NSURL fileURLWithPath:inpath];
  NSURL *outurl = [NSURL fileURLWithPath: outpath];
  
  CGPSConverterCallbacks callbacks = {
    0, // unsigned int version;
    nil, // CGPSConverterBeginDocumentCallback beginDocument;
    nil, // CGPSConverterEndDocumentCallback endDocument;
    nil, // CGPSConverterBeginPageCallback beginPage;
    nil, // CGPSConverterEndPageCallback endPage;
    nil, // CGPSConverterProgressCallback noteProgress;
    nil, // CGPSConverterMessageCallback noteMessage;
    nil  // CGPSConverterReleaseInfoCallback releaseInfo;
  };
  
  CGPSConverterRef converter = CGPSConverterCreate (NULL,&callbacks,NULL);  
  CGDataProviderRef provider = CGDataProviderCreateWithURL ((CFURLRef)inurl);
  CGDataConsumerRef consumer = CGDataConsumerCreateWithURL ((CFURLRef)outurl);
  
  BOOL converted = CGPSConverterConvert (converter,provider,consumer,NULL);
  
  if (converted) {
    NSLog(@"Postscript file converted.\n");
    //CGDataConsumerRetain(consumer);
  } else {
    NSLog(@"Converting postscript failed.\n");
  }
  
  CGDataProviderRelease (provider);
  CGDataConsumerRelease (consumer);
  CFRelease (converter);
  
  [pool release];
}

