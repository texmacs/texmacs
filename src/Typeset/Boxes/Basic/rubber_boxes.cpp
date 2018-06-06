
/******************************************************************************
* MODULE     : rubber.cpp
* DESCRIPTION: boxes whose dimensions are (partially) set by the user.
*                - empty and plain boxes
*                - parenthesis boxes
*                - overline and underline like boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"

/*****************************************************************************/
// Bracket types from  ../math-mode/math-macros.hpp
/*****************************************************************************/

#define Lbracket        1011
#define Lcrochet        1012
#define Langular        1013
#define Laccolade       1014
#define Rbracket        1021
#define Rcrochet        1022
#define Rangular        1023
#define Raccolade       1024
#define Voidbr          1030
#define Absolute        1031

/*****************************************************************************/
// Empty boxes
/*****************************************************************************/

struct empty_box_rep: public box_rep {
  empty_box_rep (path ip, int x1b, int y1b, int x2b, int y2b): box_rep (ip) {
    x3=x4=y3=y4=0; x1= x1b; y1=y1b; x2=x2b; y2=y2b; }
  operator tree () { return "empty"; }
  void display (renderer ren) { (void) ren; }
};

struct dummy_box_rep: public box_rep {
  dummy_box_rep (path ip, int x1b, int y1b, int x2b, int y2b): box_rep (ip) {
    x3=x4=y3=y4=0; x1= x1b; y1=y1b; x2=x2b; y2=y2b; }
  operator tree () { return "dummy"; }
  void display (renderer ren) { (void) ren; }
  path find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
    bool dummy; found= false;
    return box_rep::find_box_path (x, y, delta, force, dummy); }
};

struct marker_box_rep: public box_rep {
  int pos;
  box ref;
  marker_box_rep (path ip2, int x1b, int y1b, int x2b, int y2b, box ref2):
    box_rep (is_accessible (ip2)? ip2->next: ip2),
    pos (is_accessible (ip2)? ip2->item: 0), ref (ref2) {
      x3= x4= y3= y4= 0; x1= x1b; y1= y1b; x2= x2b; y2= y2b; }
  operator tree () { return "marker"; }
  void display (renderer ren) { (void) ren; }
  path find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
    (void) x; (void) y; (void) delta; (void) force;
    found= true;
    return path (0); }
  path find_lip () {
    return is_accessible (ip)? descend (ip, pos): ip; }
  path find_rip () {
    return is_accessible (ip)? descend (ip, pos): ip; }
  path find_right_box_path () {
    return path (0); }
  path find_box_path (path p, bool& found) {
    found= !is_nil (p) && is_accessible (ip);
    return path (0); }
  path find_tree_path (path bp) {
    (void) bp;
    if (is_accessible (ip)) return reverse (descend (ip, pos));
    else return reverse (descend_decode (ip, 0)); }
  cursor find_cursor (path bp) {
    (void) bp; return cursor (0, 0, 0, y1, y2); }
  selection find_selection (path lbp, path rbp) {
    return selection (rectangles (),
		      find_tree_path (lbp), find_tree_path (rbp)); }
  SI sub_lo_base (int level) { return ref->sub_lo_base (level); }
  //SI sub_lo_base (int level) { return min (y1, ref->sub_lo_base (level)); }
  SI sub_hi_lim (int level) { return ref->sub_hi_lim (level); }
  SI sup_lo_lim (int level) { return ref->sup_lo_lim (level); }
  SI sup_lo_base (int level) { return ref->sup_lo_base (level); }
  SI sup_hi_lim (int level) { return ref->sup_hi_lim (level); }
  //SI sup_hi_lim (int level) { return max (y2, ref->sup_hi_lim (level)); }
};

/*****************************************************************************/
// Brackets
/*****************************************************************************/

struct bracket_box_rep: public box_rep {
  int br_type;
  pencil pen;

  bracket_box_rep (path ip, int br_type2, pencil pen, SI y1b, SI y2b);
  operator tree () { return "bracket"; }
  void display (renderer ren);
};

SI
bracket_width (int br_type, SI height, SI penw) {
  switch (br_type) {
  case Lbracket:
  case Rbracket:
  case Lcrochet:
  case Rcrochet:
  case Laccolade:
  case Raccolade:
  case Langular:
  case Rangular:
    {
      SI ref_size  = penw/2;
      double factor= sqrt (((double) height) / ((double) ref_size));
      if (factor<2) factor=2;
      factor=factor*1.412;
      return (2*penw) + ((SI) (((double) height)/factor));
    }
  case Absolute:
    return 2*penw;
  case Voidbr:
  default:
    return 0;
  }
}

bracket_box_rep::bracket_box_rep (path ip, int br_type2, pencil pen2,
				  SI y1b, SI y2b): box_rep (ip) {
  br_type = br_type2;
  pen     = pen2;
  x1 = x3 = 0;
  x2 = x4 = bracket_width (br_type, y2b- y1b, pen->get_width ());
  y1 = y3 = y1b;
  y2 = y4 = y2b;
}

void
draw_bracket (renderer ren, int br_type, SI x, SI y, SI w, SI h, pencil pen) {
  SI lw= pen->get_width ();
  x+=lw; w-=2*lw;
  y+=lw; h-=2*lw;
  ren->set_pencil (pen);
  switch (br_type) {
  case Lbracket:
    {
      int i;
      SI ww= (SI) (((double) w) / (1.0- sqrt (0.5)));
      SI hh= (SI) (((double) h) / sqrt (2.0));
      SI ox= x+ ww;
      SI oy= y+ (h>>1);
      ren->set_pencil (pen->set_width (ren->pixel));
      for (i=0; i<lw; i+=ren->pixel)
	ren->arc (ox-ww+i, oy-hh, ox+ww-i, oy+hh, 135<<6, 90<<6);
    }
    break;
  case Rbracket:
    {
      int i;
      SI ww= (SI) (((double) w) / (1.0- sqrt (0.5)));
      SI hh= (SI) (((double) h) / sqrt (2.0));
      SI ox= x+ w- ww;
      SI oy= y+ (h>>1);
      ren->set_pencil (pen->set_width (ren->pixel));
      for (i=0; i<lw; i+=ren->pixel)
	ren->arc (ox-ww+i, oy-hh, ox+ww-i, oy+hh, -(45<<6), 90<<6);
    }
    break;
  case Lcrochet:
    ren->line (x, y, x, y+h);
    ren->line (x, y, x+w, y);
    ren->line (x, y+h, x+w, y+h);
    break;
  case Rcrochet:
    ren->line (x+w, y, x+w, y+h);
    ren->line (x, y, x+w, y);
    ren->line (x, y+h, x+w, y+h);
    break;
  case Laccolade:
  case Raccolade:
    {
      SI d = w>>1;
      SI ox= x+ (w>>1);
      SI oy= y+ (h>>1);
      // SI xx= x+ w;
      SI yy= y+ h;
      ren->line (ox, y+d-PIXEL, ox, oy-d);
      ren->line (ox, oy+d-PIXEL, ox, yy-d);
      if (br_type==Laccolade) {
	ren->arc (ox, yy-w, ox+w, yy, 90<<6, 90<<6);
	ren->arc (ox-w, oy, ox, oy+w, 270<<6, 90<<6);
	ren->arc (ox-w, oy-w, ox, oy, 0, 90<<6);
	ren->arc (ox, y, ox+w, y+w, 180<<6, 90<<6);
      }
      else {
	ren->arc (ox-w, yy-w, ox, yy, 0, 90<<6);
	ren->arc (ox, oy, ox+w, oy+w, 180<<6, 90<<6);
	ren->arc (ox, oy-w, ox+w, oy, 90<<6, 90<<6);
	ren->arc (ox-w, y, ox, y+w, 270<<6, 90<<6);
      }
    }
    break;
  case Langular:
    ren->line (x, y+(h>>1), x+w, y);
    ren->line (x, y+(h>>1), x+w, y+h);
    break;
  case Rangular:
    ren->line (x+w, y+(h>>1), x, y);
    ren->line (x+w, y+(h>>1), x, y+h);
    break;
  case Absolute:
    ren->line (x, y, x, y+h);
    break;
  }
}

void
bracket_box_rep::display (renderer ren) {
  draw_bracket (ren, br_type, 0, y1, x2, y2-y1, pen);
}

/*****************************************************************************/
// box construction routines
/*****************************************************************************/

box
empty_box (path ip, int x1, int y1, int x2, int y2) {
  return tm_new<empty_box_rep> (ip, x1, y1, x2, y2);
}

box
dummy_box (path ip, int x1, int y1, int x2, int y2) {
  return tm_new<dummy_box_rep> (ip, x1, y1, x2, y2);
}

box
marker_box (path ip, int x1, int y1, int x2, int y2, box ref) {
  return tm_new<marker_box_rep> (ip, x1, y1, x2, y2, ref);
}

box
bracket_box (path ip, int br_type, pencil pen, SI y1, SI y2) {
  return tm_new<bracket_box_rep> (ip, br_type, pen, y1, y2);
}
