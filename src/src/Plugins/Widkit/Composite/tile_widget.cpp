
/******************************************************************************
* MODULE     : tile_widget.cpp
* DESCRIPTION: tile widgets
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "renderer.hpp"
#include "Widkit/attribute_widget.hpp"

void abs_round (SI& x, SI& y);

/******************************************************************************
* Tiles
******************************************************************************/

class tile_rep: public attribute_widget_rep {
protected:
  int cols, rows;

public:
  tile_rep (array<wk_widget> a, int cols);
  tile_rep (array<wk_widget> a, int cols, array<string> name);
  operator tree ();

  void handle_get_size (get_size_event ev);
  void handle_position (position_event ev);
  void handle_get_coord2 (get_coord2_event ev);
  void handle_set_coord2 (set_coord2_event ev);
};

tile_rep::tile_rep (array<wk_widget> a, int cols2):
  attribute_widget_rep (a, south_west),
  cols (max (1, cols2)), rows ((N(a)+cols-1)/cols) {}

tile_rep::tile_rep (array<wk_widget> a, int cols2, array<string> n):
  attribute_widget_rep (a, n, south_west),
  cols (max (1, cols2)), rows ((N(a)+cols-1)/cols) {}

tile_rep::operator tree () {
  int i;
  tree t (TUPLE, N(a)+2);
  t[0]= "tile";
  t[1]= as_string (cols);
  for (i=0; i<N(a); i++) t[i+2]= (tree) a[i];
  return t;
}

void
tile_rep::handle_get_size (get_size_event ev) {
  int i;
  SI& w= ev->w;
  SI& h= ev->h;

  SI m1=0, m2=0, c1=0, c2=0;
  for (i=0; i<N(a); i++) {
    a[i] << get_coord2 ("extra width", c1, c2);
    m1= max (m1, c1); m2= max (m2, c2);
  }
  for (i=0; i<N(a); i++)
    a[i] << set_coord2 ("extra width", m1, m2);

  if (ev->mode==0) {
    SI ww= w, hh= h;
    this << get_size (ww, hh, 1);
    w= min (w, ww);
    h= min (h, hh);
    ww= w; hh= h;
    this << get_size (ww, hh, -1);
    w= max (w, ww);
    h= max (h, hh);
  }
  else {
    int i, ww=0, hh=0;
    for (i=0; i<N(a); i++) {
      SI www= w/cols, hhh= h/rows;
      abs_round (www, hhh);
      a[i] << get_size (www, hhh, ev->mode);
      ww= max (ww, www);
      hh= max (hh, hhh);
    }
    w= ww*cols+2*PIXEL; h= hh*rows+2*PIXEL;
  }
}

void
tile_rep::handle_position (position_event ev) {
  (void) ev;

  int i;
  if (N(a)==0) return;
  for (i=0; i<N(a); i++) {
    int col= i%cols, row= (rows-1)-(i/cols);
    SI X1= (col*w)/cols;
    SI Y1= (row*h)/rows;
    SI X2= ((col+1)*w)/cols;
    SI Y2= ((row+1)*h)/rows;
    SI OX= (X1+X2)>>1;
    SI OY= (Y1+Y2)>>1;
    SI WX= X2-X1-2*PIXEL;
    SI HY= Y2-Y1-2*PIXEL;
    abs_round (OX, OY);
    abs_round (WX, HY);
    a[i] << emit_position (OX, OY, WX, HY, center);
  }
}

void
tile_rep::handle_get_coord2 (get_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_get_coord2 (ev);
  else { ev->c1= 0; ev->c2= 0; }
}

void
tile_rep::handle_set_coord2 (set_coord2_event ev) {
  if (ev->which != "extra width") attribute_widget_rep::handle_set_coord2 (ev);
}

/******************************************************************************
* Interface
******************************************************************************/

wk_widget
tile (array<wk_widget> a, int cols) {
  return new tile_rep (a, cols);
}

wk_widget
tile (array<wk_widget> a, int cols, array<string> name) {
  return new tile_rep (a, cols, name);
}
