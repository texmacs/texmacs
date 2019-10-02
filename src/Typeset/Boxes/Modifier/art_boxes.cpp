
/******************************************************************************
* MODULE     : art_boxes.cpp
* DESCRIPTION: Boxes with artistic ornaments
* COPYRIGHT  : (C) 2019  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/change.hpp"
#include "Boxes/construct.hpp"
#include "scheme.hpp"
#include "gui.hpp"
#include "effect.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "image_files.hpp"

/******************************************************************************
* Art boxes
******************************************************************************/

struct art_box_rep: public composite_box_rep {
  art_box_parameters ps;
  tree data;
  brush old_bg;
  pencil old_pen;
  art_box_rep (path ip, box b, art_box_parameters ps2);
  operator tree () { return tree (TUPLE, "art box", ps->data); }
  void perform_rewritings ();
  void get_image_extents (tree prg, SI& xl, SI& xr, SI& yb, SI& yt);
  void display_image (renderer r, url u, tree e, SI xl, SI yb, SI xr, SI yt);
  void display_one (renderer ren, tree prg);
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
};

/******************************************************************************
* Required routines
******************************************************************************/

art_box_rep::art_box_rep (path ip, box b, art_box_parameters ps2):
  composite_box_rep (ip), ps (ps2), data (ps->data)
{
  insert (b, ps->lpad - b->x1, ps->bpad - b->y1);
  position ();
  x1= 0;
  y1= 0;
  x2= b->w() + ps->lpad + ps->rpad;
  y2= b->h() + ps->bpad + ps->tpad;
  x3= min (x3, 0);
  y3= min (y3, 0);
  x4= max (x4, x2);
  y4= max (y4, y2);
  perform_rewritings ();
  for (int i=0; i<N(data); i++)
    if (data[i][0] == "image" || data[i][0] == "rubber") {
      SI xl= 0, xr= x2, yb= 0, yt= y2;
      get_image_extents (data[i], xl, xr, yb, yt);
      x3= min (x3, xl - 5*PIXEL);
      y3= min (y3, yb - 5*PIXEL);
      x4= max (x4, xr + 5*PIXEL);
      y4= max (y4, yt + 5*PIXEL);
    }
  finalize ();
}

void
art_box_rep::display_image (renderer ren, url u, tree eff,
                            SI xl, SI yb, SI xr, SI yt) {
  //if (ren->is_screen) {
  ren->round (xl, yb);
  ren->round (xr, yt);
  //}
  SI xw= xr - xl, yh= yt - yb;
  if (eff != "") {
    array<url> args;
    args << u;
    u= make_file (CMD_APPLY_EFFECT, eff, args);
  }
  scalable im= load_scalable_image (u, xw, yh, "", ren->pixel);
  ren->draw_scalable (im, xl, yb);
}

void
art_box_rep::display_one (renderer ren, tree prg) {
  if (prg[0] == "image" && is_atomic (prg[1])) {
    SI xl= 0, xr= x2, yb= 0, yt= y2;
    get_image_extents (prg, xl, xr, yb, yt);
    url u= cork_to_utf8 (prg[1]->label);
    tree eff= "";
    for (int i=2; i<N(prg); i+=2)
      if (prg[i] == "effect") eff= prg[i+1];
    display_image (ren, u, eff, xl, yb, xr, yt);
  }
  else if (prg[0] == "rubber" && is_atomic (prg[1])) {
    SI xl= 0, xr= x2, yb= 0, yt= y2;
    get_image_extents (prg, xl, xr, yb, yt);
    if (xl >= xr || yb >= yt) return;
    url u= cork_to_utf8 (prg[1]->label);
    int uw, uh;
    image_size (u, uw, uh);
    tree grid= "";
    for (int i=2; i<N(prg); i+=2)
      if (prg[i] == "grid") grid= prg[i+1];
    if (!is_func (grid, TUPLE, 3) ||
        !is_func (grid[1], TUPLE) ||
        !is_func (grid[2], TUPLE)) return;
    tree eff= grid[0];
    array<double> hor, ver;
    for (int i=0; i<N(grid[1]); i++)
      hor << as_double (grid[1][i]);
    for (int i=0; i<N(grid[2]); i++)
      ver << as_double (grid[2][i]);
    if (hor[N(hor)-1] <= hor[0]) return;
    if (ver[N(ver)-1] <= ver[0]) return;
    int nx= 1, ny= 1;
    double scx= (xr - xl) / ((hor[N(hor)-1] - hor[0]) * uw);
    double scy= (yt - yb) / ((ver[N(ver)-1] - ver[0]) * uh);
    if (N(hor) == 4 && N(ver) == 2) {
      double w1= (hor[1] - hor[0]) * uw;
      double w2= (hor[2] - hor[1]) * uw;
      double w3= (hor[3] - hor[2]) * uw;
      double h1= (ver[1] - ver[0]) * uh;
      double ww= ((xr - xl) * h1) / (yt - yb);
      if (ww < 0.5 * (w1 + w3)) return;
      if (w1 < 0.0 || w2 <= 0.0 || w3 < 0.0) return;
      int nr= (int) round ((ww - w1 - w3) / w2);
      if (nr < 0) nr= 0;
      scx= (xr - xl) / (w1 + nr * w2 + w3);
      nx= nr + 2;
    }
    if (N(ver) == 4 && N(hor) == 2) {
      double w1= (hor[1] - hor[0]) * uw;
      double h1= (ver[1] - ver[0]) * uh;
      double h2= (ver[2] - ver[1]) * uh;
      double h3= (ver[3] - ver[2]) * uh;
      double hh= ((yt - yb) * w1) / (xr - xl);
      if (hh < 0.5 * (h1 + h3)) return;
      if (h1 < 0.0 || h2 <= 0.0 || h3 < 0.0) return;
      int nr= (int) round ((hh - h1 - h3) / h2);
      if (nr < 0) nr= 0;
      scy= (yt - yb) / (h1 + nr * h2 + h3);
      ny= nr + 2;
    }
    double curx= xl;
    for (int i=0; i<nx; i++) {
      int ii= min (i, 1);
      if (nx >= 2 && i == nx-1) ii= 2;
      double nextx= curx + (hor[ii+1] - hor[ii]) * uw * scx;
      double cury= yb;
      for (int j=0; j<ny; j++) {
        int jj= min (j, 1);
        if (ny >= 2 && j == ny-1) jj= 2;
        double nexty= cury + (ver[jj+1] - ver[jj]) * uh * scy;
        if (nextx > curx && nexty > cury) {
          SI xx1= (SI) round (curx) , yy1= (SI) round (cury);
          SI xx2= (SI) round (nextx), yy2= (SI) round (nexty);
          tree creff (EFF_CROP, eff,
                      grid[1][ii], grid[2][jj],
                      grid[1][ii+1], grid[2][jj+1]);
          display_image (ren, u, creff, xx1, yy1, xx2, yy2);
        }
        cury= nexty;
      }
      curx= nextx;
    }
  }
}

void
art_box_rep::pre_display (renderer& ren) {
  old_bg = ren->get_background ();
  old_pen= ren->get_pencil ();
  for (int i=0; i<N(data); i++)
    if (data[0] == "text") break;
    else display_one (ren, data[i]);
}

void
art_box_rep::post_display (renderer &ren) {
  int i;
  for (i=0; i<N(data); i++)
    if (data[0] == "text") break;
  for (i++; i<N(data); i++)
    display_one (ren, data[i]);
  ren->set_background (old_bg);
  ren->set_pencil (old_pen);
}

/******************************************************************************
* Preprocessing
******************************************************************************/

static tree
minus (tree t) {
  if (is_atomic (t) && is_double (t->label))
    return "-" * t->label;
  else if (is_func (t, TMLEN, 1))
    return tree (TMLEN, minus (t[0]));
  else if (is_func (t, TMLEN, 3))
    return tree (TMLEN, minus (t[2]), minus (t[1]), minus (t[0]));
  else
    return t;
}

void
art_box_rep::perform_rewritings () {
  tree new_data (TUPLE);
  for (int i=0; i<N(data); i++)
    if (data[i][0] == "frame" && N(data[i]) >= 2) {
      tree name= data[i][1];
      tree eff = "0";
      tree cx1= "0", cy1= "0", cx2= "1", cy2= "1";
      tree lw= "", rw= "", bh= "", th= "";
      string al= "inner";
      string format= "xxx x.x xxx";
      tree dxl= "", dxr= "", dyb= "", dyt= "";
      tree lrep= "", rrep= "", brep= "", trep= "";
      for (int j=2; j+1<N(data[i]); j+=2) {
        tree var= data[i][j];
        tree val= data[i][j+1];
        if      (var == "effect") eff= val;
        else if (var == "align") al= as_string (val);
        else if (var == "format") format= as_string (val);
        else if (var == "lcrop") cx1= val;
        else if (var == "bcrop") cy1= val;
        else if (var == "rcrop") cx2= val;
        else if (var == "tcrop") cy2= val;
        else if (var == "lwidth" ) lw= val;
        else if (var == "bheight") bh= val;
        else if (var == "rwidth" ) rw= val;
        else if (var == "theight") th= val;
        else if (var == "loffset") dxl= val;
        else if (var == "roffset") dxr= val;
        else if (var == "boffset") dyb= val;
        else if (var == "toffset") dyt= val;
        else if (var == "lrepeat") lrep= val;
        else if (var == "rrepeat") rrep= val;
        else if (var == "brepeat") brep= val;
        else if (var == "trepeat") trep= val;
      }
      for (int row= 0; row <= 2; row++)
        for (int col= 0; col <= 2; col++)
          if (format[(2-row)*4+col] == 'x') {
            tree crx1, cry1, crx2, cry2, ha, va;
            if (col == 0) { crx1= "0"; crx2= cx1; ha= "left"; }
            if (col == 1) { crx1= cx1; crx2= cx2; ha= "stretch"; }
            if (col == 2) { crx1= cx2; crx2= "1"; ha= "right"; }
            if (row == 0) { cry1= "0"; cry2= cy1; va= "bottom"; }
            if (row == 1) { cry1= cy1; cry2= cy2; va= "stretch"; }
            if (row == 2) { cry1= cy2; cry2= "1"; va= "top"; }
            bool outl= al == "outer" || occurs ("west", al);
            bool outr= al == "outer" || occurs ("east", al);
            bool outb= al == "outer" || occurs ("south", al);
            bool outt= al == "outer" || occurs ("north", al);
            if (outl && ha == "left"  ) ha= "outer left";
            if (outr && ha == "right" ) ha= "outer right";
            if (outb && va == "bottom") va= "outer bottom";
            if (outt && va == "top"   ) va= "outer top";
            tree sub_eff= tree (EFF_CROP, eff, crx1, cry1, crx2, cry2);
            tree t= tree (TUPLE, "image", name, "effect", sub_eff);
            if (col != 1 || row != 1)
              if ((col == 1 && (lrep != "" && rrep != "")) ||
                  (row == 1 && (brep != "" && trep != ""))) {
                tree hor (TUPLE, crx1);
                if (col == 1 && (lrep != "" && rrep != ""))
                  hor << lrep << rrep;
                hor << crx2;
                tree ver (TUPLE, cry1);
                if (row == 1 && (brep != "" && trep != ""))
                  ver << brep << trep;
                ver << cry2;
                tree grid (TUPLE, eff, hor, ver);
                t= tree (TUPLE, "rubber", name, "grid", grid);
              }
            t << tree ("halign") << ha << tree ("valign") << va;
            if (col == 0) t << tree ("width") << lw;
            if (col == 1 && !outl) t << tree ("left") << lw;
            if (col == 1 && !outr) t << tree ("right") << minus (rw);
            if (col == 2) t << tree ("width") << rw;
            if (row == 0) t << tree ("height") << bh;
            if (row == 1 && !outb) t << tree ("bottom") << bh;
            if (row == 1 && !outt) t << tree ("top") << minus (th);
            if (row == 2) t << tree ("height") << th;
            if (col == 0 && dxl != "") t << tree ("hoffset") << dxl;
            if (col == 1 && dxl != "") t << tree ("loffset") << dxl;
            if (col == 1 && dxr != "") t << tree ("roffset") << dxr;
            if (col == 2 && dxr != "") t << tree ("hoffset") << dxr;
            if (row == 0 && dyb != "") t << tree ("voffset") << dyb;
            if (row == 1 && dyb != "") t << tree ("boffset") << dyb;
            if (row == 1 && dyt != "") t << tree ("toffset") << dyt;
            if (row == 2 && dyt != "") t << tree ("voffset") << dyt;
            new_data << t;
          }
    }
    else new_data << data[i];
  data= new_data;
}

/******************************************************************************
* Image placement
******************************************************************************/

static SI
get_length (tree t, SI l) {
  if (is_atomic (t) && ends (t->label, "%")) {
    double x= as_double (t->label (0, N(t->label)-1));
    return (SI) floor ((x/100.0) * l + 0.5);
  }
  else if (is_func (t, TMLEN, 1) && is_double (t[0]))
    return (SI) as_double (t[0]);
  else if (is_func (t, TMLEN, 3) && is_double (t[1]))
    return (SI) as_double (t[1]);
  else return l;
}

void
art_box_rep::get_image_extents (tree prg, SI& xl, SI& xr, SI& yb, SI& yt) {
  SI xw= x2, yh= y2, dxl= 0, dxr= 0, dyb= 0, dyt= 0;
  xl= 0; xr= x2; yb= 0; yt= y2;
  SI xl_ref= 0, xr_ref= x2, yb_ref= 0, yt_ref= y2;
  bool xl_done= false, xr_done= false, xw_done= false;
  bool yb_done= false, yt_done= false, yh_done= false;
  for (int i=2; i<N(prg); i+=2) {
    if (prg[i] == "halign") {
      if (prg[i+1] == "stretch") {
        xl_ref= xl= 0; xr_ref= xr= xw; }
      else if (prg[i+1] == "left") {
        xl_ref= xr_ref= xl= 0; xr= xw; xl_done= true; }
      else if (prg[i+1] == "right") {
        xl= x2-xw; xl_ref= xr_ref= xr= x2; xr_done= true; }
      else if (prg[i+1] == "outer left") {
        xl= -xw; xl_ref= xr_ref= xr= 0; xr_done= true; }
      else if (prg[i+1] == "outer right") {
        xl_ref= xr_ref= xl= x2; xr= x2 + xw; xl_done= true; }
    }
    else if (prg[i] == "valign") {
      if (prg[i+1] == "stretch") {
        yb_ref= yb= 0; yt_ref= yt= yh; }
      else if (prg[i+1] == "bottom") {
        yb_ref= yt_ref= yb= 0; yt= yh; yb_done= true; }
      else if (prg[i+1] == "top") {
        yb= y2-yh; yb_ref= yt_ref= yt= y2; yt_done= true; }
      else if (prg[i+1] == "outer bottom") {
        yb= -yh; yb_ref= yt_ref= yt= 0; yt_done= true; }
      else if (prg[i+1] == "outer top") {
        yb_ref= yt_ref= yb= y2; yt= y2 + yh; yb_done= true; }
    }
    else if (prg[i] == "width") {
      xw= get_length (prg[i+1], x2);
      xw_done= true;
    }
    else if (prg[i] == "left") {
      xl= get_length (prg[i+1], x2) + xl_ref;
      xl_done= true;
    }
    else if (prg[i] == "right") {
      xr= get_length (prg[i+1], x2) + xr_ref;
      xr_done= true;
    }
    else if (prg[i] == "height") {
      yh= get_length (prg[i+1], y2);
      yh_done= true;
    }
    else if (prg[i] == "bottom") {
      yb= get_length (prg[i+1], y2) + yb_ref;
      yb_done= true;
    }
    else if (prg[i] == "top") {
      yt= get_length (prg[i+1], y2) + yt_ref;
      yt_done= true;
    }
    else if (prg[i] == "hoffset")
      dxl= dxr= get_length (prg[i+1], x2);
    else if (prg[i] == "voffset")
      dyb= dyt= get_length (prg[i+1], y2);
    else if (prg[i] == "loffset")
      dxl= get_length (prg[i+1], x2);
    else if (prg[i] == "roffset")
      dxr= get_length (prg[i+1], x2);
    else if (prg[i] == "boffset")
      dyb= get_length (prg[i+1], y2);
    else if (prg[i] == "toffset")
      dyt= get_length (prg[i+1], y2);
  }
  if (xw_done && xl_done && !xr_done) { xr= xl + xw; xr_done= true; }
  if (xw_done && xr_done && !xl_done) { xl= xr - xw; xl_done= true; }
  if (yh_done && yb_done && !yt_done) { yt= yb + yh; yt_done= true; }
  if (yh_done && yt_done && !yb_done) { yb= yt - yh; yb_done= true; }
  xl += dxl; xr += dxr;
  yb += dyb; yt += dyt;  
}

/******************************************************************************
* Box construction routines
******************************************************************************/

box
art_box (path ip, box b, art_box_parameters ps) {
  return tm_new<art_box_rep> (ip, b, ps);
}
