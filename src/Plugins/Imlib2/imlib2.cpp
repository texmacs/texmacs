
/******************************************************************************
* MODULE     : imlib2.cpp
* DESCRIPTION: interface with Imlib2
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Imlib2/imlib2.hpp"
#include "hashmap.hpp"

#include <X11/Xlib.h>
#include <Imlib2.h>
#include <stdio.h>
#include <string.h>

hashmap<tree,tree> imlib2_size_cache ("");

bool
supports_imlib2 () {
  return true;
}

bool
imlib2_supports (url u) {
  if (!supports_imlib2 ()) return false;
  string s= suffix (u);
  return s != "ps" && s != "eps" && s != "pdf" && s != "xpm";
}

Imlib_Image
imlib2_load_image (url u) {
  url name= resolve (u);
  if (is_none (name))
    name= resolve ("$TEXMACS_PIXMAP_PATH/TeXmacs-gnu.xpm");
  char *_name= as_charp (as_string (name));
  Imlib_Image image;
  image = imlib_load_image (_name);
  delete[] _name;
  return image;
}

void
imlib2_image_size (url u, int& w, int& h) {
  if (imlib2_size_cache->contains (u->t)) {
    w= as_int (imlib2_size_cache[u->t][0]);
    h= as_int (imlib2_size_cache[u->t][1]);
  }
  else {
    Imlib_Image image= imlib2_load_image (u);
    if (image) {
      imlib_context_set_image (image);
      w= imlib_image_get_width ();
      h= imlib_image_get_height ();
      imlib_free_image ();
      imlib2_size_cache (u->t)= tuple (as_string (w), as_string (h));
    }
  }
}

void
imlib2_display (Display* dpy, Pixmap pm, url u, SI w, SI h,
		double cx1, double cy1, double cx2, double cy2)
{
  Imlib_Image image= imlib2_load_image (u);
  if (image) {
    Visual *vis= DefaultVisual (dpy, DefaultScreen (dpy));
    Colormap cm= DefaultColormap (dpy, DefaultScreen (dpy));
    imlib_context_set_display (dpy);
    imlib_context_set_visual (vis);
    imlib_context_set_colormap (cm);
    imlib_context_set_drawable (pm);

    imlib_context_set_image (image);
    int iw= imlib_image_get_width ();
    int ih= imlib_image_get_height ();
    int x1= (int) (cx1 * iw + 0.5);
    int y1= (int) (cy1 * ih + 0.5);
    int x2= (int) (cx2 * iw + 0.5);
    int y2= (int) (cy2 * ih + 0.5);
    int ww= x2 - x1;
    int hh= y2 - y1;
    imlib_render_image_part_on_drawable_at_size
      (x1, hh-y2, ww, hh, 0, 0, w, h);
    imlib_free_image ();
  }
}
