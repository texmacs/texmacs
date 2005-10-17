
/******************************************************************************
* MODULE     : rubber.cpp
* DESCRIPTION: boxes whose dimensions are (partially) set by the user.
*                - empty and plain boxes
*                - parenthesis boxes
*                - overline and underline like boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
// The empty box
/*****************************************************************************/

struct empty_box_rep: public box_rep {
  empty_box_rep (path ip, int x1b, int y1b, int x2b, int y2b): box_rep (ip) {
    x3=x4=y3=y4=0; x1= x1b; y1=y1b; x2=x2b; y2=y2b; }
  operator tree () { return "empty"; }
  void display (ps_device dev) { (void) dev; }
};

/*****************************************************************************/
// Brackets
/*****************************************************************************/

struct bracket_box_rep: public box_rep {
  int br_type;
  color col;
  SI penw;

  bracket_box_rep (path ip, int br_type2, SI penw, color c, SI y1b, SI y2b);
  operator tree () { return "bracket"; }
  void display (ps_device dev);
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

bracket_box_rep::bracket_box_rep (path ip, int br_type2, SI penw2, color c,
				  SI y1b, SI y2b): box_rep (ip) {
  br_type= br_type2;
  penw   = penw2;
  col    = c;
  x1 = x3 = 0;
  x2 = x4 = bracket_width (br_type, y2b- y1b, penw);
  y1 = y3 = y1b;
  y2 = y4 = y2b;
}

void
draw_bracket (ps_device dev, int br_type, SI x, SI y, SI w, SI h, SI lw) {
  x+=lw; w-=2*lw;
  y+=lw; h-=2*lw;
  switch (br_type) {
  case Lbracket:
    {
      int i;
      SI ww= (SI) (((double) w) / (1.0- sqrt (0.5)));
      SI hh= (SI) (((double) h) / sqrt (2.0));
      SI ox= x+ ww;
      SI oy= y+ (h>>1);
      dev->set_line_style (PIXEL);
      for (i=0; i<lw; i+=PIXEL)
	dev->arc (ox-ww+i, oy-hh, ox+ww-i, oy+hh, 135<<6, 90<<6);
    }
    break;
  case Rbracket:
    {
      int i;
      SI ww= (SI) (((double) w) / (1.0- sqrt (0.5)));
      SI hh= (SI) (((double) h) / sqrt (2.0));
      SI ox= x+ w- ww;
      SI oy= y+ (h>>1);
      dev->set_line_style (PIXEL);
      for (i=0; i<lw; i+=PIXEL)
	dev->arc (ox-ww+i, oy-hh, ox+ww-i, oy+hh, -(45<<6), 90<<6);
    }
    break;
  case Lcrochet:
    dev->line (x, y, x, y+h);
    dev->line (x, y, x+w, y);
    dev->line (x, y+h, x+w, y+h);
    break;
  case Rcrochet:
    dev->line (x+w, y, x+w, y+h);
    dev->line (x, y, x+w, y);
    dev->line (x, y+h, x+w, y+h);
    break;
  case Laccolade:
  case Raccolade:
    {
      SI d = w>>1;
      SI ox= x+ (w>>1);
      SI oy= y+ (h>>1);
      // SI xx= x+ w;
      SI yy= y+ h;
      dev->line (ox, y+d-PIXEL, ox, oy-d);
      dev->line (ox, oy+d-PIXEL, ox, yy-d);
      if (br_type==Laccolade) {
	dev->arc (ox, yy-w, ox+w, yy, 90<<6, 90<<6);
	dev->arc (ox-w, oy, ox, oy+w, 270<<6, 90<<6);
	dev->arc (ox-w, oy-w, ox, oy, 0, 90<<6);
	dev->arc (ox, y, ox+w, y+w, 180<<6, 90<<6);
      }
      else {
	dev->arc (ox-w, yy-w, ox, yy, 0, 90<<6);
	dev->arc (ox, oy, ox+w, oy+w, 180<<6, 90<<6);
	dev->arc (ox, oy-w, ox+w, oy, 90<<6, 90<<6);
	dev->arc (ox-w, y, ox, y+w, 270<<6, 90<<6);
      }
    }
    break;
  case Langular:
    dev->line (x, y+(h>>1), x+w, y);
    dev->line (x, y+(h>>1), x+w, y+h);
    break;
  case Rangular:
    dev->line (x+w, y+(h>>1), x, y);
    dev->line (x+w, y+(h>>1), x, y+h);
    break;
  case Absolute:
    dev->line (x, y, x, y+h);
    break;
  }
}

void
bracket_box_rep::display (ps_device dev) {
  dev->set_line_style (penw);
  dev->set_color (col);
  draw_bracket (dev, br_type, 0, y1, x2, y2-y1, penw);
}

/*****************************************************************************/
// box construction routines
/*****************************************************************************/

box
empty_box (path ip, int x1, int y1, int x2, int y2) {
  return new empty_box_rep (ip, x1, y1, x2, y2);
}

box
bracket_box (path ip, int br_type, SI penw, color col, SI y1, SI y2) {
  return new bracket_box_rep (ip, br_type, penw, col, y1, y2);
}
