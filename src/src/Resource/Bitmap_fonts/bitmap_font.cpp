
/******************************************************************************
* MODULE     : bitmap_font.cpp
* DESCRIPTION: bitmap fonts
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "bitmap_font.hpp"

RESOURCE_CODE(bitmap_metric);
RESOURCE_CODE(bitmap_font);

/******************************************************************************
* bitmap_metrics
******************************************************************************/

bitmap_metric_rep::bitmap_metric_rep (string name):
  rep<bitmap_metric> (name) {}

bitmap_metric_rep::~bitmap_metric_rep () {
  fatal_error ("not yet implemented",
	       "bitmap_metric_rep::~bitmap_metric_rep",
	       "bitmap_metric.cpp");
}

/******************************************************************************
* Standard bitmap metrics
******************************************************************************/

static text_extents on_error;

struct std_bitmap_metric_rep: public bitmap_metric_rep {
  int bc, ec;
  text_extents* bmm;

  std_bitmap_metric_rep (string name, text_extents* bmm, int bc, int ec);
  text_extents& get (int char_code);
};

std_bitmap_metric_rep::std_bitmap_metric_rep (
  string name, text_extents* bmm2, int bc2, int ec2):
    bitmap_metric_rep (name), bc (bc2), ec (ec2), bmm (bmm2)
{
  on_error->x1= on_error->y1= 0;
  on_error->x2= on_error->y2= 0;
  on_error->x3= on_error->y3= 0;
  on_error->x4= on_error->y4= 0;
}

text_extents&
std_bitmap_metric_rep::get (int c) {
  if ((c<bc) || (c>ec)) return on_error;
  return bmm [c-bc];
}

bitmap_metric
std_bitmap_metric (string name, text_extents* bmm, int bc, int ec) {
  return make (bitmap_metric, name,
	       new std_bitmap_metric_rep (name, bmm, bc, ec));
}

/******************************************************************************
* bitmap_fonts
******************************************************************************/

bitmap_font_rep::bitmap_font_rep (string name):
  rep<bitmap_font> (name) {}

bitmap_font_rep::~bitmap_font_rep () {
  fatal_error ("not yet implemented",
	       "bitmap_font_rep::~bitmap_font_rep",
	       "bitmap_font.cpp");
}

/******************************************************************************
* Standard bitmap fonts
******************************************************************************/

struct std_bitmap_font_rep: public bitmap_font_rep {
  int bc, ec;
  bitmap_char* bmf; // definitions of the characters

  std_bitmap_font_rep (string name, bitmap_char* bmf, int bc, int ec);
  bitmap_char& get (int char_code);
};

std_bitmap_font_rep::std_bitmap_font_rep (
  string name, bitmap_char* bmf2, int bc2, int ec2):
    bitmap_font_rep (name), bc (bc2), ec (ec2), bmf (bmf2) {}

static bitmap_char nil_bitmap_char;

bitmap_char&
std_bitmap_font_rep::get (int c) {
  if ((c<bc) || (c>ec)) return nil_bitmap_char;
  return bmf [c-bc];
}

bitmap_font
std_bitmap_font (string name, bitmap_char* bmf, int bc, int ec) {
  return make (bitmap_font, name, new std_bitmap_font_rep (name, bmf, bc, ec));
}
