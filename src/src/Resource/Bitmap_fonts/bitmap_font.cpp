
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

bitmap_metric_rep::bitmap_metric_rep (
  string name, text_extents* bmm2, int bc2, int ec2):
    rep<bitmap_metric> (name), bc (bc2), ec (ec2), bmm (bmm2)
{
  on_error->x1= on_error->y1= 0;
  on_error->x2= on_error->y2= 0;
  on_error->x3= on_error->y3= 0;
  on_error->x4= on_error->y4= 0;
}

bitmap_metric_rep::~bitmap_metric_rep () {
  fatal_error ("not yet implemented",
	       "bitmap_metric_rep::~bitmap_metric_rep",
	       "bitmap_metric.cpp");
}

/******************************************************************************
* bitmap_fonts
******************************************************************************/

bitmap_font_rep::bitmap_font_rep (
  string name, bitmap_char* bmf2, int bc2, int ec2):
    rep<bitmap_font> (name), bc (bc2), ec (ec2), bmf (bmf2) {}

bitmap_font_rep::~bitmap_font_rep () {
  fatal_error ("not yet implemented",
	       "bitmap_font_rep::~bitmap_font_rep",
	       "bitmap_font.cpp");
}
