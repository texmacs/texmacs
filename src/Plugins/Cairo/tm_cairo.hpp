
/******************************************************************************
* MODULE     : tm_cairo.hpp
* DESCRIPTION: Interface with Cairo
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_CAIRO_H
#define TM_CAIRO_H
//#include "tree.hpp"

bool tm_cairo_initialize ();
bool tm_cairo_present ();


#ifdef USE_CAIRO
#include <cairo.h>
#ifdef CAIRO_HAS_FT_FONT
#include <cairo-ft.h>
#endif
#ifdef CAIRO_HAS_PS_SURFACE
#include <cairo-ps.h>
#endif
#ifdef CAIRO_HAS_XLIB_SURFACE
#include <cairo-xlib.h>
#undef KeyPress  // conflict between QEvent::KeyPrees and X11 defintion
#endif
#ifdef CAIRO_HAS_QUARTZ_SURFACE
#define ID OTHER_ID
// conflicts with X11 headers
#undef Status
#define Cursor OTHER_Cursor
#include <cairo-quartz.h>
#undef ID
#undef Cursor
#endif

extern void (*tm_cairo_move_to) (cairo_t *cr, double x, double y);
extern void (*tm_cairo_show_glyphs) (cairo_t *cr, const cairo_glyph_t *glyphs, int num_glyphs);
extern cairo_status_t (*tm_cairo_font_face_status) (cairo_font_face_t *font_face);
extern void (*tm_cairo_set_source_surface) (cairo_t *cr, cairo_surface_t *surface, double x, double y);
extern void (*tm_cairo_new_path) (cairo_t *cr);
extern void (*tm_cairo_stroke) (cairo_t *cr);
extern cairo_status_t (*tm_cairo_font_face_set_user_data) (cairo_font_face_t *font_face, const cairo_user_data_key_t *key, void *user_data, cairo_destroy_func_t destroy);
extern void (*tm_cairo_set_antialias) (cairo_t *cr, cairo_antialias_t antialias);
extern void (*tm_cairo_set_font_size) (cairo_t *cr, double size);
extern void (*tm_cairo_fill) (cairo_t *cr);
extern void (*tm_cairo_set_source_rgba) (cairo_t *cr, double red, double green, double blue, double alpha);
extern const char * (*tm_cairo_status_to_string) (cairo_status_t status);
extern void (*tm_cairo_set_source_rgb) (cairo_t *cr, double red, double green, double blue);
extern void (*tm_cairo_close_path) (cairo_t *cr);
extern void (*tm_cairo_restore) (cairo_t *cr);
extern void (*tm_cairo_translate) (cairo_t *cr, double tx, double ty);
extern void (*tm_cairo_set_font_face) (cairo_t *cr, cairo_font_face_t *font_face);
extern void (*tm_cairo_font_face_destroy) (cairo_font_face_t *font_face);
extern void (*tm_cairo_set_line_width) (cairo_t *cr, double width);
extern cairo_surface_t * (*tm_cairo_image_surface_create_from_png) (const char *filename);
extern int (*tm_cairo_image_surface_get_width) (cairo_surface_t *surface);
extern void (*tm_cairo_scale) (cairo_t *cr, double sx, double sy);
extern void (*tm_cairo_mask) (cairo_t *cr, cairo_pattern_t *pattern);
extern void (*tm_cairo_set_operator) (cairo_t *cr, cairo_operator_t op);
extern cairo_surface_t * (*tm_cairo_image_surface_create) (cairo_format_t format, int width, int height);
extern void (*tm_cairo_paint) (cairo_t *cr);
extern void (*tm_cairo_rectangle) (cairo_t *cr, double x, double y, double width, double height);
extern void (*tm_cairo_set_line_cap) (cairo_t *cr, cairo_line_cap_t line_cap);
extern void (*tm_cairo_set_line_join) (cairo_t *cr, cairo_line_join_t line_join);
extern cairo_surface_t * (*tm_cairo_surface_reference) (cairo_surface_t *surface);
extern void (*tm_cairo_line_to) (cairo_t *cr, double x, double y);
extern cairo_status_t (*tm_cairo_status) (cairo_t *cr);
extern void (*tm_cairo_show_page) (cairo_t *cr);
extern cairo_t * (*tm_cairo_reference) (cairo_t *cr);
extern cairo_t * (*tm_cairo_create) (cairo_surface_t *target);
extern void (*tm_cairo_surface_destroy) (cairo_surface_t *surface);
extern void (*tm_cairo_set_fill_rule) (cairo_t *cr, cairo_fill_rule_t fill_rule);
extern void (*tm_cairo_destroy) (cairo_t *cr);
extern void (*tm_cairo_save) (cairo_t *cr);
extern void (*tm_cairo_set_source) (cairo_t *cr, cairo_pattern_t *source);
extern int (*tm_cairo_image_surface_get_height) (cairo_surface_t *surface);
extern void (*tm_cairo_mask_surface) (cairo_t *cr, cairo_surface_t *surface, double surface_x, double surface_y);

#ifdef CAIRO_HAS_FT_FONT
extern cairo_font_face_t * (*tm_cairo_ft_font_face_create_for_ft_face) (FT_Face face, int load_flags);
#endif

#ifdef CAIRO_HAS_PS_SURFACE
extern cairo_surface_t * (*tm_cairo_ps_surface_create) (const char *filename, double width_in_points, double height_in_points);
#endif

#ifdef CAIRO_HAS_XLIB_SURFACE
extern cairo_surface_t * (*tm_cairo_xlib_surface_create) (Display *dpy, Drawable	drawable, Visual *visual, int width, int height);
#endif

#ifdef CAIRO_HAS_QUARTZ_SURFACE
extern  cairo_surface_t * (*tm_cairo_quartz_surface_create_for_cg_context) (CGContextRef cgContext, unsigned int width, unsigned int height);
#endif

#endif

#endif // TM_CAIRO_H
