
/******************************************************************************
* MODULE     : imlib2.cpp
* DESCRIPTION: interface with Imlib2
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifdef X11TEXMACS
#define IMLIB2_X11TEXMACS
#endif

#include "config.h"
#include "Imlib2/imlib2.hpp"
#include "dyn_link.hpp"
#include "hashmap.hpp"

#ifdef USE_IMLIB2

#include <Imlib2.h>
#include <stdio.h>
#include <string.h>

/******************************************************************************
* Routines used from Imlib2
******************************************************************************/

Imlib_Image (*IMLIB2_load_image) (const char *file);
int (*IMLIB2_image_get_width) (void);
int (*IMLIB2_image_get_height) (void);
void (*IMLIB2_context_set_image) (Imlib_Image image);
void (*IMLIB2_render_image_part_on_drawable_at_size)
  (int source_x, int source_y, int source_width, int source_height,
   int x, int y, int width, int height);
void (*IMLIB2_free_image) (void);
void (*IMLIB2_context_set_display) (Display * display);
void (*IMLIB2_context_set_visual) (Visual * visual);
void (*IMLIB2_context_set_colormap) (Colormap colormap);
void (*IMLIB2_context_set_drawable) (Drawable drawable);

/******************************************************************************
* Initialization
******************************************************************************/

static bool imlib2_initialized= false;
static bool imlib2_error      = false;

#ifdef LINKED_IMLIB2
#define imlib2_bind(orig,tm) \
  tm= orig;
#else
#define imlib2_bind(orig,tm) \
  (void) symbol_install ("libImlib2.so", #orig, (pointer&) tm); \
  if (tm == NULL) return;
#endif

void
imlib2_initialize () {
  imlib2_initialized= true;
  imlib2_error      = true;

  int status= debug_off ();
  imlib2_bind (imlib_load_image, IMLIB2_load_image);
  imlib2_bind (imlib_image_get_width, IMLIB2_image_get_width);
  imlib2_bind (imlib_image_get_height, IMLIB2_image_get_height);
  imlib2_bind (imlib_context_set_image, IMLIB2_context_set_image);
  imlib2_bind (imlib_render_image_part_on_drawable_at_size,
	       IMLIB2_render_image_part_on_drawable_at_size);
  imlib2_bind (imlib_free_image, IMLIB2_free_image);
  imlib2_bind (imlib_context_set_display, IMLIB2_context_set_display);
  imlib2_bind (imlib_context_set_visual, IMLIB2_context_set_visual);
  imlib2_bind (imlib_context_set_colormap, IMLIB2_context_set_colormap);
  imlib2_bind (imlib_context_set_drawable, IMLIB2_context_set_drawable);
  debug_on (status);

#ifdef LINKED_IMLIB2
  if (DEBUG_AUTO) debug_automatic << "With linked Imlib2 support\n";
#else
  if (DEBUG_AUTO) debug_automatic << "Installed Imlib2 support\n";
#endif

  imlib2_error= false;
}

/******************************************************************************
* Functionality provided by the plug-in
******************************************************************************/

hashmap<tree,tree> imlib2_size_cache ("");

bool
imlib2_present () {
  if (!imlib2_initialized)
    imlib2_initialize ();
  return !imlib2_error;
}

bool
imlib2_supports (url u) {
  string s= suffix (u);
  if (s == "ps" || s == "eps" || s == "pdf" ||
      s == "xpm" || s == "tif") return false;
  return imlib2_present ();
}

static Imlib_Image
imlib2_load_image (url u) {
  url name= resolve (u);
  if (is_none (name))
    name= resolve ("$TEXMACS_PIXMAP_PATH/TeXmacs-gnu.xpm");
  Imlib_Image image;
  {
    c_string _name (as_string (name));
    image = IMLIB2_load_image (_name);
  }
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
      IMLIB2_context_set_image (image);
      w= IMLIB2_image_get_width ();
      h= IMLIB2_image_get_height ();
      IMLIB2_free_image ();
      imlib2_size_cache (u->t)= tuple (as_string (w), as_string (h));
    }
  }
}

void
imlib2_display (Display* dpy, Pixmap pm, url u, SI w, SI h) {
  Imlib_Image image= imlib2_load_image (u);
  if (image) {
    Visual *vis= DefaultVisual (dpy, DefaultScreen (dpy));
    Colormap cm= DefaultColormap (dpy, DefaultScreen (dpy));
    IMLIB2_context_set_display (dpy);
    IMLIB2_context_set_visual (vis);
    IMLIB2_context_set_colormap (cm);
    IMLIB2_context_set_drawable (pm);

    IMLIB2_context_set_image (image);
    int iw= IMLIB2_image_get_width ();
    int ih= IMLIB2_image_get_height ();
    IMLIB2_render_image_part_on_drawable_at_size (0, 0, iw, ih, 0, 0, w, h);
    IMLIB2_free_image ();
  }
}

#else // USE_IMLIB2

/******************************************************************************
* If Imlib2 is not present...
******************************************************************************/

bool imlib2_present () { return false; }
bool imlib2_supports (url u) { (void) u; return false; }

void
imlib2_image_size (url u, int& w, int& h) {
  (void) u; (void) w; (void) h;
  FAILED ("imlib2 is not present");
}

#ifdef X11TEXMACS
void
imlib2_display (Display* dpy, Pixmap pm, url u, SI w, SI h) {
  (void) dpy; (void) pm;
  (void) u; (void) w; (void) h;
  FAILED ("imlib2 is not present");
}
#endif

#endif // USE_IMLIB2
