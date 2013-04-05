
/******************************************************************************
* MODULE     : picture.cpp
* DESCRIPTION: Abstract graphical pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "renderer.hpp"
#include "analyze.hpp"
#include "gui.hpp"
#include "image_files.hpp"

/******************************************************************************
* Default implementations of some virtual routines
******************************************************************************/

url
picture_rep::get_name () {
  return url_none ();
}

picture
picture_rep::non_lazy () {
  return this;
}

void
picture_rep::translate_origin (int dx, int dy) {
  set_origin (get_origin_x () + dx, get_origin_y () + dy);
}

color
picture_rep::internal_smooth_pixel (double x, double y) {
  int x1= (int) floor (x);
  int y1= (int) floor (y);
  int x2= x1 + 1;
  int y2= y1 + 1;
  double ix1= x2 - x;
  double ix2= x - x1;
  double iy1= y2 - y;
  double iy2= y - y1;
  double ix1y1= ix1 * iy1;
  double ix1y2= ix1 * iy2;
  double ix2y1= ix2 * iy1;
  double ix2y2= ix2 * iy2;
  int rx1y1, gx1y1, bx1y1, ax1y1;
  int rx1y2, gx1y2, bx1y2, ax1y2;
  int rx2y1, gx2y1, bx2y1, ax2y1;
  int rx2y2, gx2y2, bx2y2, ax2y2;
  get_rgb_color (internal_get_pixel (x1, y1), rx1y1, gx1y1, bx1y1, ax1y1);
  get_rgb_color (internal_get_pixel (x1, y2), rx1y2, gx1y2, bx1y2, ax1y2);
  get_rgb_color (internal_get_pixel (x2, y1), rx2y1, gx2y1, bx2y1, ax2y1);
  get_rgb_color (internal_get_pixel (x2, y2), rx2y2, gx2y2, bx2y2, ax2y2);
  int Rx1y1= ax1y1 * rx1y1, Gx1y1= ax1y1 * gx1y1, Bx1y1= ax1y1 * bx1y1;
  int Rx1y2= ax1y2 * rx1y2, Gx1y2= ax1y2 * gx1y2, Bx1y2= ax1y2 * bx1y2;
  int Rx2y1= ax2y1 * rx2y1, Gx2y1= ax2y1 * gx2y1, Bx2y1= ax2y1 * bx2y1;
  int Rx2y2= ax2y2 * rx2y2, Gx2y2= ax2y2 * gx2y2, Bx2y2= ax2y2 * bx2y2;
  double tR= ix1y1 * Rx1y1 + ix2y1 * Rx2y1 + ix1y2 * Rx1y2 + ix2y2 * Rx2y2;
  double tG= ix1y1 * Gx1y1 + ix2y1 * Gx2y1 + ix1y2 * Gx1y2 + ix2y2 * Gx2y2;
  double tB= ix1y1 * Bx1y1 + ix2y1 * Bx2y1 + ix1y2 * Bx1y2 + ix2y2 * Bx2y2;
  double ta= ix1y1 * ax1y1 + ix2y1 * ax2y1 + ix1y2 * ax1y2 + ix2y2 * ax2y2;
  if (ta <= 1.0e-10) return rgb_color (0, 0, 0, 0);
  int r= as_int (tR / ta);
  int g= as_int (tG / ta);
  int b= as_int (tB / ta);
  int a= as_int (ta);
  return rgb_color (min (r, 255), min (g, 255), min (b, 255), a);
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

picture
error_picture (int w, int h) {
  picture pic= raster_picture (w, h);
  draw_on (pic, 0x20ff0000, compose_source);
}

/******************************************************************************
* xpm pictures
******************************************************************************/

color xpm_to_color (string s);
picture qt_load_xpm (url file_name);

picture
load_xpm (url file_name) {
  static hashmap<string,picture> cache;
  string name= as_string (file_name);
  if (cache->contains (name)) return cache[name];

#ifdef QTTEXMACS

  picture pict= qt_load_xpm (file_name);

#else

  tree t= xpm_load (file_name);

  // get main info
  int ok, i=0, j, k, w, h, c, b, x, y;
  string s= as_string (t[0]);
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  if ((!ok) || (N(t)<(c+1)) || (c<=0)) {
    cerr << "file_name= " << file_name << "\n";
    FAILED ("invalid xpm");
  }

  // setup colors
  string first_name;
  hashmap<string,color> pmcs(0);
  for (k=0; k<c; k++) {
    string s   = as_string (t[k+1]);
    string name= "";
    string def = "none";
    if (N(s)<b) i=N(s);
    else { name= s(0,b); i=b; }
    if (k==0) first_name= name;

    skip_spaces (s, i);
    if ((i<N(s)) && (s[i]=='s')) {
      i++;
      skip_spaces (s, i);
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      skip_spaces (s, i);
    }
    if ((i<N(s)) && (s[i]=='c')) {
      i++;
      skip_spaces (s, i);
      j=i;
      while ((i<N(s)) && (s[i]!=' ') && (s[i]!='\t')) i++;
      def= locase_all (s (j, i));
    }
    pmcs(name)= xpm_to_color (def);
  }

  // setup pixmap
  picture pict= raster_picture (w, h);
  draw_on (pict, 0x00646464, compose_source);
  for (y=0; y<h; y++) {
    if (N(t) < (y+c+1)) s= "";
    else s= as_string (t[y+c+1]);
    for (x=0; x<w; x++) {
      string name;
      if (N(s)<(b*(x+1))) name= first_name;
      else name= s (b*x, b*(x+1));
      color pmc= pmcs[name];
      if (!pmcs->contains (name)) pmc= pmcs[first_name];
      pict->set_pixel (x, h-1-y, pmc);
    }
  }
  pict= as_native_picture (pict);

#endif

  cache (name)= pict;
  return pict;
}

/******************************************************************************
* Generation of encapsulated postscript
******************************************************************************/

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
      color col= pic->get_pixel (i - ox, h_pt - 1 - j - oy);
      int rr, gg, bb, aa;
      get_rgb_color (col, rr, gg, bb, aa);
      if (aa != 255) alpha= true;
    }

  string mask;
  for (j= 0; j < h_pt; j++) {
    for (i=0; i < w_pt; i++) {
      l++;
      color col= pic->get_pixel (i - ox, h_pt - 1 - j - oy);
      int rr, gg, bb, aa;
      get_rgb_color (col, rr, gg, bb, aa);
      rr= (rr * aa + 255 * (255 - aa)) / 255;
      gg= (gg * aa + 255 * (255 - aa)) / 255;
      bb= (bb * aa + 255 * (255 - aa)) / 255;
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
        color col= pic->get_pixel (i - ox, h_pt - 1 - j - oy);
        int rr, gg, bb, aa;
        get_rgb_color (col, rr, gg, bb, aa);
        v += (aa <= 32) << (3 - i % 4);
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
