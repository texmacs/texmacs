
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
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
  void sub_display (renderer &ren, tree prg);
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
    if (data[i][0] == "image") {
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
art_box_rep::sub_display (renderer &ren, tree prg) {
  if (prg[0] == "image" && is_atomic (prg[1])) {
    SI xl= 0, xr= x2, yb= 0, yt= y2;
    get_image_extents (prg, xl, xr, yb, yt);
    //if (ren->is_screen) {
    ren->round (xl, yb);
    ren->round (xr, yt);
    //}
    SI xw= xr - xl, yh= yt - yb;
    url u= cork_to_utf8 (prg[1]->label);
    tree eff= "";
    for (int i=2; i<N(prg); i+=2)
      if (prg[i] == "effect") eff= prg[i+1];
    if (eff != "") {
      array<url> args;
      args << u;
      u= make_file (CMD_APPLY_EFFECT, eff, args);
    }
    scalable im= load_scalable_image (u, xw, yh, "", ren->pixel);
    ren->draw_scalable (im, xl, yb);
  }
}

void
art_box_rep::pre_display (renderer& ren) {
  old_bg = ren->get_background ();
  old_pen= ren->get_pencil ();
  for (int i=0; i<N(data); i++)
    if (data[0] == "text") break;
    else sub_display (ren, data[i]);
}

void
art_box_rep::post_display (renderer &ren) {
  int i;
  for (i=0; i<N(data); i++)
    if (data[0] == "text") break;
  for (i++; i<N(data); i++)
    sub_display (ren, data[i]);
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
