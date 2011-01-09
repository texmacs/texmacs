
/******************************************************************************
* MODULE     : image_files.cpp
* DESCRIPTION: image file handling
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "image_files.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "scheme.hpp"
#include "Imlib2/imlib2.hpp"

#ifdef MACOSX_EXTENSIONS
#include "MacOS/mac_images.h"
#endif

#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#endif

#ifdef OS_WIN32
#include <x11/xlib.h>
#endif

#ifdef USE_GS
#include "Ghostscript/gs_utilities.hpp"
#endif

hashmap<tree,string> ps_bbox ("");

/******************************************************************************
* Loading xpm pixmaps
******************************************************************************/

tree
xpm_load (url u) {
  string s;
  load_string ("$TEXMACS_PIXMAP_PATH" * u, s, false);
  if (s == "") load_string ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm", s, true);

  int i, j;
  tree t (TUPLE);
  for (i=0; i<N(s); i++)
    if (s[i]=='\x22') {
      i++;
      j=i;
      while ((i<N(s)) && (s[i]!='\x22')) i++;
      t << s (j, i);
    }
  if (N(t)==0) return xpm_load ("$TEXMACS_PATH/misc/pixmaps/TeXmacs.xpm");
  return t;
}

void
xpm_size (url u, int& w, int& h) {
  static hashmap<string,string> xpm_size_table ("");
  string file_name= as_string (u);
  if (!xpm_size_table->contains (file_name)) {
    tree t= xpm_load (u);
    xpm_size_table (file_name)= t[0]->label;
  }

  int i= 0;
  bool ok;
  string s= xpm_size_table[file_name];
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  if (!ok) {
    cerr << "File name= " << file_name << "\n";
    FAILED ("invalid xpm");
  }
}

array<string>
xpm_colors (tree t) {
  array<string> res(0);
  string s= t[0]->label;
  int ok, i=0, j, k, w, h, c, b;
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  ASSERT (ok && N(t)>c && c>0, "invalid xpm tree");

  for (k=0; k<c; k++) {
    string s   = as_string (t[k+1]);
    string def = "none";
    if (N(s)<b) i=N(s); else i=b;

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
    res<<def;
  }
  return res;
}

array<SI>
xpm_hotspot (tree t) {
  array<SI> res(0);
  string s= t[0]->label;
  int ok, i=0, w, h, c, b, x, y;
  skip_spaces (s, i);
  ok= read_int (s, i, w);
  skip_spaces (s, i);
  ok= read_int (s, i, h) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, c) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, b) && ok;
  ASSERT (ok && N(t)>c && c>0, "invalid xpm tree");

  skip_spaces (s, i);
  ok= read_int (s, i, x) && ok;
  skip_spaces (s, i);
  ok= read_int (s, i, y) && ok;
  if (ok) {
    res<<x;
    res<<y;
  }
  return res;
}

/******************************************************************************
* Loading postscript files or conversion to postscript
******************************************************************************/

string
ps_load (url image) {
  url name= resolve (image);
  if (is_none (name))
    name= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";

#ifdef OS_WIN32
  if (is_ramdisc (name)) name= get_from_ramdisc (name);
#endif

  string s, suf= suffix (name);
  if (suf == "ps" || suf == "eps") load_string (name, s, false);
  else s= as_string (call ("image->postscript", object (name)));

#ifdef OS_WIN32
  if (s == "") {
    char *data;
    char *path= as_charp (as_string (name));
    data= XLoadImageAsPS (path);
    tm_delete_array (path);
    if (!data) s= "";
    else {
      s= string (data);
      free (data);
    }
  }
#endif

  if (s == "") load_string ("$TEXMACS_PATH/misc/pixmaps/unknown.ps", s, true);
  return s;
}

void
ps_bounding_box (url image, int& x1, int& y1, int& x2, int& y2) {
  tree lookup= image->t;
  if (!ps_bbox->contains (image->t)) {
    int i;
    string s= ps_load (image);
    string r= "0 0 32 32";
    for (i=0; i<N(s); i++)
      if (read (s, i, "%%BoundingBox: ")) {
	double tmp;
	int j = i;
	read_line (s, i, r);
	// Check whether we really have a bounding box line with numbers
	skip_spaces (s, j);
	if(read_double (s, j, tmp))
	  break;
      }
    ps_bbox (image->t)= r;
  }

  int i= 0;
  bool ok;
  string s= ps_bbox [image->t];
  double X1=0.0, Y1=0.0, X2=0.0, Y2=0.0;
  skip_spaces (s, i);
  ok= read_double (s, i, X1);
  skip_spaces (s, i);
  ok= read_double (s, i, Y1) && ok;
  skip_spaces (s, i);
  ok= read_double (s, i, X2) && ok;
  skip_spaces (s, i);
  ok= read_double (s, i, Y2) && ok;
  x1= (int) X1; y1= (int) Y1;
  x2= (int) X2; y2= (int) Y2;
  if (ok) return;
  x1= y1= 0; x2= 596; y2= 842;
}

/******************************************************************************
* Getting the size of an image, using internal plug-ins if possible
******************************************************************************/

void
image_size (url image, int& w, int& h) {
#ifdef QTTEXMACS
  if (qt_supports (image)) {
    cout << "qt " << image << "\n";
    qt_image_size (image, w, h); // default to 72 dpi
    return;
  }
#endif
#ifdef MACOSX_EXTENSIONS 
  if ( mac_image_size (image, w, h) ) {
    cout << "mac " << image << "\n";
    return;
  }
#endif
#ifdef USE_IMLIB2
  if (imlib2_supports (image)) {
    cout << "imlib2 " << image << "\n";
    imlib2_image_size (image, w, h);
    return;
  }
#endif
#ifdef USE_GS
  if (gs_supports (image)) {
    cout << "gs " << image << "\n";
    gs_image_size (image, w, h);
    return;
  }
#endif
  cout << "default " << image << "\n";
  int x1, y1, x2, y2;
  ps_bounding_box (image, x1, y1, x2, y2);
  w= x2 - x1;
  h= y2 - y1;
}

/******************************************************************************
* Converting image formats
******************************************************************************/

void
image_to_eps (url image, url eps, int w_pt, int h_pt, int dpi) {
/*  if ((suffix (eps) != "eps") && (suffix (eps) != "ps")) {
    cerr << "TeXmacs] warning: " << concretize (eps) << " has no .eps or .ps suffix\n";
  }*/
#ifdef QTTEXMACS
  if (qt_supports (image)) {
    qt_image_to_eps (image, eps, w_pt, h_pt, dpi);
    return;
  }
#endif
#ifdef USE_GS
  if (gs_supports (image)) {
    gs_to_eps (image, eps);
    return;
  }
#endif
  string s= suffix (image);
  string cmd= "convert";
  if (s != "pdf" && s != "ps" && s != "eps" && dpi > 0 && w_pt > 0 && h_pt > 0) {
    int ww= w_pt * dpi / 72;
    int hh= h_pt * dpi / 72;
    string sz= eval_system ("identify -format \"%[fx:w] %[fx:h]\"", image);
    int w_px, h_px, ok= true, pos= 0;
    ok= read_int (sz, pos, w_px);
    skip_spaces (sz, pos);  
    ok= ok && read_int (sz, pos, h_px);
    if (ok && (ww < w_px || hh < h_px)) {
      cmd << " -resize " * as_string (ww) * "x" * as_string (hh) * "!";
    }
  }
  system (cmd, image, eps);
}

string
image_to_psdoc (url image) {
  url psfile= url_temp (".eps");
  image_to_eps (image, psfile);
  string psdoc;
  load_string (psfile, psdoc, false);
  remove (psfile);
  return psdoc;
}

void
image_to_png (url image, url png, int w, int h) {
/*  if (suffix (png) != "png") {
    cerr << "TeXmacs] warning: " << concretize (png) << " has no .png suffix\n";
  }*/
#ifdef MACOSX_EXTENSIONS
  cout << "mac convert " << image << ", " << png << "\n";
  mac_image_to_png (image, png, w, h);
#else
#ifdef QTTEXMACS
  if (qt_supports (image)) {
    cout << "qt convert " << image << ", " << png << "\n";
    qt_convert_image (image, png, w, h);
    return;
  }
#endif
#ifdef USE_GS
  if (gs_supports (image)) {
    cout << "gs convert " << image << ", " << png << "\n";
    gs_to_png (image, png, w, h);
    return;
  }
#endif
  cout << "default convert " << image << ", " << png << "\n";
  string cmd= "convert";
  if (w > 0 && h > 0) cmd << " -resize " * as_string(w) * "x" * as_string(h) * "!";
  system (cmd, image, png);
#endif
}

