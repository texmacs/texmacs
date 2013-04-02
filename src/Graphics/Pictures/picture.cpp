
/******************************************************************************
* MODULE     : picture.cpp
* DESCRIPTION: Abstract graphical pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "picture.hpp"
#include "analyze.hpp"
#include "gui.hpp"

void
picture_rep::translate_origin (int dx, int dy) {
  set_origin (get_origin_x () + dx, get_origin_y () + dy);
}

void
picture_rep::internal_copy_from (int x, int y, picture src,
                                 int x1, int y1, int x2, int y2)
{
  for (int yy= y1; yy < y2; yy++)
    for (int xx= x1; xx < x2; xx++)
      internal_set_pixel (x + xx, y + yy, src->internal_get_pixel (xx, yy));
}

void
picture_rep::internal_copy_to (int x, int y, picture dest,
                               int x1, int y1, int x2, int y2)
{
  for (int yy= y1; yy < y2; yy++)
    for (int xx= x1; xx < x2; xx++)
      dest->internal_set_pixel (x + xx, y + yy, internal_get_pixel (xx, yy));
}

string
picture_as_eps (picture pic, int dpi) {
  static const char* d= "0123456789ABCDEF";
  int w_pt= pic->get_width (), h_pt= pic->get_height ();
  int ox= pic->get_origin_x (), oy= pic->get_origin_y ();
  string r;

  string sw= as_string (w_pt);
  string sh= as_string (h_pt);
  r << "%!PS-Adobe-3.0 EPSF-3.0\n%%Creator: TeXmacs\n%%BoundingBox: 0 0 "
    << sw << " " << sh
    << "\n\n% Created by picture_as_eps ()\n\n%%BeginProlog\nsave\n"
    << "countdictstack\nmark\nnewpath\n/showpage {} def\n/setpagedevice "
    << "{pop} def\n%%EndProlog\n%%Page 1 1\n"
    << "/max { 2 copy lt { exch } if pop } bind def\n"
    << "/ImageWidth " << sw
    << " def\n/ImageHeight " << sh << " def\nImageWidth ImageHeight max "
    << "ImageWidth ImageHeight max scale\n\n/ImageDatas\n\tcurrentfile\n\t"
    << "<< /Filter /ASCIIHexDecode >>\n\t/ReusableStreamDecode\n\tfilter\n";
  
  int v, i= 0, j= 0, k= 0, l= 0;
  bool alpha= false;
  for (j=0; j < h_pt; j++)
    for (i=0; i < w_pt; i++) {
      color col= pic->get_pixel (i - ox, j - oy);
      int rr, gg, bb, aa;
      get_rgb_color (col, rr, gg, bb, aa);
      if (aa != 255) alpha= true;
    }

  string mask;
  for (j= 0; j < h_pt; j++) {
    for (i=0; i < w_pt; i++) {
      l++;
      color col= pic->get_pixel (i - ox, j - oy);
      int rr, gg, bb, aa;
      get_rgb_color (col, rr, gg, bb, aa);
      r << as_hexadecimal (rr, 2);
      r << as_hexadecimal (gg, 2);
      r << as_hexadecimal (bb, 2);
      if (l > 12) {
        r << "\n";
        l= 0;
      }
    }
    if (alpha) {
      v = 0;
      for (i=0; i < w_pt; i++) {
        color col= pic->get_pixel (i - ox, j - oy);
        int rr, gg, bb, aa;
        get_rgb_color (col, rr, gg, bb, aa);
        v += (aa == 0) << (3 - i % 4);
        if (i % 4 == 3 || i + 1 == w_pt) {
          mask << d[v];
          v= 0;
          k++;
          // Padding of the image data mask
          if (i + 1 == w_pt && k % 2 == 1) {
            mask << d[0];
            k++;
          }
          // Code layout
          if (k >= 78) {
            mask << "\n";
            k= 0;
          }
        }
      }
    }
  }
  r << ">\ndef\n\n";
  
  if (alpha) {
    r << "/MaskDatas\n\tcurrentfile\n\t<< /Filter /ASCIIHexDecode >>\n"
      << "\t/ReusableStreamDecode\n\tfilter\n"
      << mask
      << ">\ndef\n\n"
      << "/TheMask\n<<\n\t/ImageType\t1\n\t/Width\t\tImageWidth\n\t/Height\t"
      << "\tImageHeight\n\t/BitsPerComponent 1\n\t/Decode [ 0 1 ]\n\t"
      << "/ImageMatrix [ ImageWidth 0 0 ImageWidth neg 0 ImageHeight ]\n\t"
      << "/DataSource MaskDatas\n>> def\n\n";
  }
  r << "/TheImage\n<<\n\t/ImageType\t1\n\t/Width\t\tImageWidth\n\t/Height\t"
    << "\tImageHeight\n\t/BitsPerComponent 8\n\t/Decode [ 0 1 0 1 0 1 ]\n\t"
    << "/ImageMatrix [ ImageWidth 0 0 ImageWidth neg 0 ImageHeight ]\n\t"
    << "/DataSource ImageDatas\n>> def\n\n"
    << "/DeviceRGB setcolorspace\n";
  if (alpha) {
    r << "<<\n\t/ImageType 3\n\t/InterleaveType 3\n\t/DataDict TheImage\n"
      << "\t/MaskDict TheMask\n>>";
  }
  else {
    r << "\tTheImage";
  }
  r << "\nimage\nshowpage\n%%Trailer\ncleartomark\ncountdictstack\n"
    << "exch sub { end } repeat\nrestore\n%%EOF\n";

  return r;
}
