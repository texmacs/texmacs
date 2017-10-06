
/******************************************************************************
* MODULE     : tm_cairo.cpp
* DESCRIPTION: Interface with Cairo
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "dyn_link.hpp"
#include "tm_cairo.hpp"

#ifdef USE_CAIRO

static bool tm_cairo_initialized= false;
static bool tm_cairo_error      = true;

 void (*tm_cairo_move_to) (cairo_t *cr, double x, double y);
 void (*tm_cairo_show_glyphs) (cairo_t *cr, const cairo_glyph_t *glyphs, int num_glyphs);
 cairo_status_t (*tm_cairo_font_face_status) (cairo_font_face_t *font_face);
 void (*tm_cairo_set_source_surface) (cairo_t *cr, cairo_surface_t *surface, double x, double y);
 void (*tm_cairo_new_path) (cairo_t *cr);
 void (*tm_cairo_stroke) (cairo_t *cr);
 cairo_status_t (*tm_cairo_font_face_set_user_data) (cairo_font_face_t *font_face, const cairo_user_data_key_t *key, void *user_data, cairo_destroy_func_t destroy);
 void (*tm_cairo_set_antialias) (cairo_t *cr, cairo_antialias_t antialias);
 void (*tm_cairo_set_font_size) (cairo_t *cr, double size);
 void (*tm_cairo_fill) (cairo_t *cr);
 void (*tm_cairo_set_source_rgba) (cairo_t *cr, double red, double green, double blue, double alpha);
 const char * (*tm_cairo_status_to_string) (cairo_status_t status);
 void (*tm_cairo_set_source_rgb) (cairo_t *cr, double red, double green, double blue);
 void (*tm_cairo_close_path) (cairo_t *cr);
 void (*tm_cairo_restore) (cairo_t *cr);
 void (*tm_cairo_translate) (cairo_t *cr, double tx, double ty);
 void (*tm_cairo_set_font_face) (cairo_t *cr, cairo_font_face_t *font_face);
 void (*tm_cairo_font_face_destroy) (cairo_font_face_t *font_face);
 void (*tm_cairo_set_line_width) (cairo_t *cr, double width);
 cairo_surface_t * (*tm_cairo_image_surface_create_from_png) (const char *filename);
 int (*tm_cairo_image_surface_get_width) (cairo_surface_t *surface);
 void (*tm_cairo_scale) (cairo_t *cr, double sx, double sy);
 void (*tm_cairo_mask) (cairo_t *cr, cairo_pattern_t *pattern);
 void (*tm_cairo_set_operator) (cairo_t *cr, cairo_operator_t op);
 cairo_surface_t * (*tm_cairo_image_surface_create) (cairo_format_t format, int width, int height);
 void (*tm_cairo_paint) (cairo_t *cr);
 void (*tm_cairo_rectangle) (cairo_t *cr, double x, double y, double width, double height);
 void (*tm_cairo_set_line_cap) (cairo_t *cr, cairo_line_cap_t line_cap);
 void (*tm_cairo_set_line_join) (cairo_t *cr, cairo_line_join_t line_join);
 cairo_surface_t * (*tm_cairo_surface_reference) (cairo_surface_t *surface);
 void (*tm_cairo_line_to) (cairo_t *cr, double x, double y);
 cairo_status_t (*tm_cairo_status) (cairo_t *cr);
 void (*tm_cairo_show_page) (cairo_t *cr);
 cairo_t * (*tm_cairo_reference) (cairo_t *cr);
 cairo_t * (*tm_cairo_create) (cairo_surface_t *target);
 void (*tm_cairo_surface_destroy) (cairo_surface_t *surface);
 void (*tm_cairo_set_fill_rule) (cairo_t *cr, cairo_fill_rule_t fill_rule);
 void (*tm_cairo_destroy) (cairo_t *cr);
 void (*tm_cairo_save) (cairo_t *cr);
 void (*tm_cairo_set_source) (cairo_t *cr, cairo_pattern_t *source);
 int (*tm_cairo_image_surface_get_height) (cairo_surface_t *surface);
 void (*tm_cairo_mask_surface) (cairo_t *cr, cairo_surface_t *surface, double surface_x, double surface_y);


#ifdef CAIRO_HAS_FT_FONT
  cairo_font_face_t * (*tm_cairo_ft_font_face_create_for_ft_face) (FT_Face face, int load_flags);
#endif

#ifdef CAIRO_HAS_PS_SURFACE
  cairo_surface_t * (*tm_cairo_ps_surface_create) (const char *filename, double width_in_points, double height_in_points);
#endif

#ifdef CAIRO_HAS_XLIB_SURFACE
 cairo_surface_t * (*tm_cairo_xlib_surface_create) (Display *dpy, Drawable	drawable, Visual *visual, int width, int height);
#endif

#ifdef CAIRO_HAS_QUARTZ_SURFACE
  cairo_surface_t * (*tm_cairo_quartz_surface_create_for_cg_context) (CGContextRef cgContext, unsigned int width, unsigned int height);
#endif

bool
tm_cairo_initialize () {
  if (tm_cairo_initialized) return tm_cairo_error;
  tm_cairo_initialized= true;

#ifdef LINKED_CAIRO
#define CAIRO_LINK(a,b) b = a
#else
#define CAIRO_LINK(LIBFUNC,LOCALFUNC) \
  (void) symbol_install ("/usr/lib/cairo.so", #LIBFUNC , (pointer&) LOCALFUNC); \
  if (LOCALFUNC == NULL) return true; 
  
  int status= debug_off ();
#endif
  
  CAIRO_LINK(cairo_move_to, tm_cairo_move_to);
  CAIRO_LINK(cairo_show_glyphs, tm_cairo_show_glyphs);
  CAIRO_LINK(cairo_font_face_status, tm_cairo_font_face_status);
  CAIRO_LINK(cairo_set_source_surface, tm_cairo_set_source_surface);
  CAIRO_LINK(cairo_new_path, tm_cairo_new_path);
  CAIRO_LINK(cairo_stroke, tm_cairo_stroke);
  CAIRO_LINK(cairo_font_face_set_user_data, tm_cairo_font_face_set_user_data);
  CAIRO_LINK(cairo_set_antialias, tm_cairo_set_antialias);
  CAIRO_LINK(cairo_set_font_size, tm_cairo_set_font_size);
  CAIRO_LINK(cairo_fill, tm_cairo_fill);
  CAIRO_LINK(cairo_set_source_rgba, tm_cairo_set_source_rgba);
  CAIRO_LINK(cairo_status_to_string, tm_cairo_status_to_string);
  CAIRO_LINK(cairo_set_source_rgb, tm_cairo_set_source_rgb);
  CAIRO_LINK(cairo_close_path, tm_cairo_close_path);
  CAIRO_LINK(cairo_restore, tm_cairo_restore);
  CAIRO_LINK(cairo_translate, tm_cairo_translate);
  CAIRO_LINK(cairo_set_font_face, tm_cairo_set_font_face);
  CAIRO_LINK(cairo_font_face_destroy, tm_cairo_font_face_destroy);
  CAIRO_LINK(cairo_set_line_width, tm_cairo_set_line_width);
  CAIRO_LINK(cairo_image_surface_create_from_png, tm_cairo_image_surface_create_from_png);
  CAIRO_LINK(cairo_image_surface_get_width, tm_cairo_image_surface_get_width);
  CAIRO_LINK(cairo_scale, tm_cairo_scale);
  CAIRO_LINK(cairo_mask, tm_cairo_mask);
  CAIRO_LINK(cairo_set_operator, tm_cairo_set_operator);
  CAIRO_LINK(cairo_image_surface_create, tm_cairo_image_surface_create);
  CAIRO_LINK(cairo_paint, tm_cairo_paint);
  CAIRO_LINK(cairo_rectangle, tm_cairo_rectangle);
  CAIRO_LINK(cairo_set_line_cap, tm_cairo_set_line_cap);
  CAIRO_LINK(cairo_set_line_join, tm_cairo_set_line_join);
  CAIRO_LINK(cairo_surface_reference, tm_cairo_surface_reference);
  CAIRO_LINK(cairo_line_to, tm_cairo_line_to);
  CAIRO_LINK(cairo_status, tm_cairo_status);
  CAIRO_LINK(cairo_show_page, tm_cairo_show_page);
  CAIRO_LINK(cairo_reference, tm_cairo_reference);
  CAIRO_LINK(cairo_create, tm_cairo_create);
  CAIRO_LINK(cairo_surface_destroy, tm_cairo_surface_destroy);
  CAIRO_LINK(cairo_set_fill_rule, tm_cairo_set_fill_rule);
  CAIRO_LINK(cairo_destroy, tm_cairo_destroy);
  CAIRO_LINK(cairo_save, tm_cairo_save);
  CAIRO_LINK(cairo_set_source, tm_cairo_set_source);
  CAIRO_LINK(cairo_image_surface_get_height, tm_cairo_image_surface_get_height);
  CAIRO_LINK(cairo_mask_surface, tm_cairo_mask_surface);

#ifdef CAIRO_HAS_FT_FONT
  CAIRO_LINK(cairo_ft_font_face_create_for_ft_face, tm_cairo_ft_font_face_create_for_ft_face);
#endif
#ifdef CAIRO_HAS_PS_SURFACE
  CAIRO_LINK(cairo_ps_surface_create, tm_cairo_ps_surface_create);
#endif
#ifdef CAIRO_HAS_XLIB_SURFACE
  CAIRO_LINK(cairo_xlib_surface_create, tm_cairo_xlib_surface_create);
#endif  
#ifdef CAIRO_HAS_QUARTZ_SURFACE
  CAIRO_LINK(cairo_quartz_surface_create_for_cg_context, tm_cairo_quartz_surface_create_for_cg_context) ;
#endif
  
    
#undef CAIRO_LINK
#ifdef LINKED_CAIRO
  if (DEBUG_AUTO) debug_automatic << "With linked Cairo support\n";
#else
  if (DEBUG_AUTO) debug_automatic << "Installed Cairo support\n";
  debug_on (status);
#endif
  
  
  tm_cairo_error= false;
  return false;
}

bool
tm_cairo_present () {
  return !tm_cairo_initialize ();
}

#else

bool tm_cairo_initialize () { return true; }
bool tm_cairo_present () { return false; }

#endif
