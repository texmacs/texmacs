
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

RESOURCE_CODE(font_metric);
RESOURCE_CODE(font_gliefs);

/******************************************************************************
* font_metrics
******************************************************************************/

font_metric_rep::font_metric_rep (string name):
  rep<font_metric> (name) {}

font_metric_rep::~font_metric_rep () {
  fatal_error ("not yet implemented",
	       "font_metric_rep::~font_metric_rep",
	       "font_metric.cpp");
}

/******************************************************************************
* Standard bitmap metrics
******************************************************************************/

static metric on_error;

struct std_font_metric_rep: public font_metric_rep {
  int bc, ec;
  metric* bmm;

  std_font_metric_rep (string name, metric* bmm, int bc, int ec);
  metric& get (int char_code);
};

std_font_metric_rep::std_font_metric_rep (
  string name, metric* bmm2, int bc2, int ec2):
    font_metric_rep (name), bc (bc2), ec (ec2), bmm (bmm2)
{
  on_error->x1= on_error->y1= 0;
  on_error->x2= on_error->y2= 0;
  on_error->x3= on_error->y3= 0;
  on_error->x4= on_error->y4= 0;
}

metric&
std_font_metric_rep::get (int c) {
  if ((c<bc) || (c>ec)) return on_error;
  return bmm [c-bc];
}

font_metric
std_font_metric (string name, metric* bmm, int bc, int ec) {
  return make (font_metric, name,
	       new std_font_metric_rep (name, bmm, bc, ec));
}

/******************************************************************************
* font_gliefss
******************************************************************************/

font_gliefs_rep::font_gliefs_rep (string name):
  rep<font_gliefs> (name) {}

font_gliefs_rep::~font_gliefs_rep () {
  fatal_error ("not yet implemented",
	       "font_gliefs_rep::~font_gliefs_rep",
	       "font_gliefs.cpp");
}

/******************************************************************************
* Standard bitmap fonts
******************************************************************************/

struct std_font_gliefs_rep: public font_gliefs_rep {
  int bc, ec;
  glief* bmf; // definitions of the characters

  std_font_gliefs_rep (string name, glief* bmf, int bc, int ec);
  glief& get (int char_code);
};

std_font_gliefs_rep::std_font_gliefs_rep (
  string name, glief* bmf2, int bc2, int ec2):
    font_gliefs_rep (name), bc (bc2), ec (ec2), bmf (bmf2) {}

static glief nil_glief;

glief&
std_font_gliefs_rep::get (int c) {
  if ((c<bc) || (c>ec)) return nil_glief;
  return bmf [c-bc];
}

font_gliefs
std_font_gliefs (string name, glief* bmf, int bc, int ec) {
  return make (font_gliefs, name, new std_font_gliefs_rep (name, bmf, bc, ec));
}
