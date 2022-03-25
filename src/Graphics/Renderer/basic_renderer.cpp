
/******************************************************************************
* MODULE     : basic_renderer.cpp
* DESCRIPTION: common drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#if (defined(QTTEXMACS) || defined(AQUATEXMACS) || defined(SDLTEXMACS))

#include "basic_renderer.hpp"
#include "analyze.hpp"
#include "gui.hpp" // for INTERRUPT_EVENT, INTERRUPTED_EVENT
#include "font.hpp" // for the definition of font
#include "colors.hpp"
#include "iterator.hpp"
#include "convert.hpp"
#include "file.hpp"
#include <string.h>

/******************************************************************************
* structure for caching font pixmaps
******************************************************************************/

basic_character::operator tree () {
  tree t (TUPLE,  as_string (rep->c), rep->fng->res_name);
  t << as_string (rep->sf) << as_string (rep->fg) << as_string (rep->bg);
  return t;
}

bool
operator == (basic_character xc1, basic_character xc2) {
  return
    (xc1->c==xc2->c) && (xc1->fng.rep==xc2->fng.rep) &&
    (xc1->sf==xc2->sf) && (xc1->fg==xc2->fg) && (xc1->bg==xc2->bg);
}

bool
operator != (basic_character xc1, basic_character xc2) {
  return
    (xc1->c!=xc2->c) || (xc1->fng.rep!=xc2->fng.rep) ||
    (xc1->sf!=xc2->sf) || (xc1->fg!=xc2->fg) || (xc1->bg!=xc2->bg);
}

int
hash (basic_character xc) {
  return xc->c ^ ((intptr_t) xc->fng.rep) ^ xc->fg ^ xc->bg ^ xc->sf;
}

/******************************************************************************
* Conversion between window and postscript coordinates
******************************************************************************/

void
basic_renderer_rep::get_extents (int& w2, int& h2) {
  w2 = w; h2 = h;
}

void basic_renderer_rep::begin (void* handle) { 
  (void) handle; 
}

void basic_renderer_rep::end () {  }

/******************************************************************************
* Drawing into drawables
******************************************************************************/

color
basic_renderer_rep::rgb (int r, int g, int b, int a) {
  return rgb_color (r, g, b, a);
}

void
basic_renderer_rep::get_rgb (color col, int& r, int& g, int& b, int& a) {
  get_rgb_color (col, r, g, b, a);
}

pencil
basic_renderer_rep::get_pencil () {
  return pen;
}

brush
basic_renderer_rep::get_brush () {
  return fg_brush;
}

brush
basic_renderer_rep::get_background () {
  return bg_brush;
}

void
basic_renderer_rep::set_pencil (pencil p) {
  ASSERT (!is_nil (p), "concrete pencil expected");
  pen= p;
}

void
basic_renderer_rep::set_brush (brush b) {
  ASSERT (!is_nil (b), "concrete brush expected");
  fg_brush= b;
  pen= pencil (b);
}

void
basic_renderer_rep::set_background (brush b) {
  ASSERT (!is_nil (b), "concrete brush expected");
  bg_brush= b;
}


void
basic_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
//  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
}

/* shadowing and copying rectangular regions across devices defaults to nothing */

void
basic_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2,
			   renderer dev, SI x, SI y)
{
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; (void) x; (void) y; 
  if (DEBUG_EVENTS)
    debug_events << "REN fetch (" << x1 << "," << x2 << "," << y1 << "," << y2
                 << ", dev ," << x << "," << y << ")\n";
}

void
basic_renderer_rep::new_shadow (renderer& dev) {
  dev = this; 
  if (DEBUG_EVENTS) debug_events << "REN new_shadow\n";
}

void
basic_renderer_rep::delete_shadow (renderer& dev) { dev= NULL; 
  if (DEBUG_EVENTS) debug_events << "REN delete_shadow\n";
}

void
basic_renderer_rep::get_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; 
  if (DEBUG_EVENTS)
    debug_events << "REN get_shadow (" << x1 << "," << x2
                 << "," << y1 << "," << y2 << ", dev )\n";
}

void
basic_renderer_rep::put_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; 
  if (DEBUG_EVENTS)
    debug_events << "REN put_shadow (dev, " << x1 << "," << x2
                 << "," << y1 << "," << y2 << ")\n";
}

void
basic_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; 
  if (DEBUG_EVENTS)
    debug_events << "REN apply_shadow (" << x1 << "," << x2
                 << "," << y1 << "," << y2 << ")\n";
}

bool
gui_interrupted (bool check) {
  return check_event (check? INTERRUPT_EVENT: INTERRUPTED_EVENT);
}

#endif
