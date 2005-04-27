
/******************************************************************************
* MODULE     : grid.cpp
* DESCRIPTION: grids for the graphics
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "grid.hpp"
#include "math_util.hpp"

/******************************************************************************
* The empty grid
******************************************************************************/

struct empty_grid_rep: public grid_rep {
  empty_grid_rep ():
    grid_rep () {}
  operator tree () { return "empty_grid"; }
  void display (ps_device dev, SI x1, SI y1, SI x2, SI y2) {
    (void) dev; }
  point find_closest_point (point p) { return p; }
};

grid
empty_grid () {
  return new empty_grid_rep ();
}

/******************************************************************************
* Cartesian grids
******************************************************************************/

struct cartesian_rep: public grid_rep {
  SI step; // Step (on the screen)
  cartesian_rep (array<SI> subd, array<color> col, point o, SI st):
    grid_rep (subd, col, o), step (st) {}
  operator tree () { return "cartesian"; }
  void display (ps_device dev, SI x1, SI y1, SI x2, SI y2);
  point find_closest_point (point p);
};

void
cartesian_rep::display (ps_device dev, SI x1, SI y1, SI x2, SI y2) {
  SI xo,yo;
  xo=(SI)o[0];
  yo=(SI)o[1];
  dev->set_line_style (PIXEL);
  SI i;
  for (i= N(subd)-1; i>=1; i--) {
     SI nsub;
     nsub= subd[i];
     dev->set_color (col[i]);
     if (nsub!=0) {
       SI x, y;
       for (x=xo; x<=x2; x+=step/nsub)
         dev->line (x,y1,x,y2);
       for (x=xo; x>=x1; x-=step/nsub)
         dev->line (x,y1,x,y2);
       for (y=yo; y<=y2; y+=step/nsub)
         dev->line (x1,y,x2,y);
       for (y=yo; y>=y1; y-=step/nsub)
         dev->line (x1,y,x2,y);
     }
  }
  dev->set_color (col[0]);
  dev->line (x1,yo,x2,yo);
  dev->line (xo,y1,xo,y2);
}

point
cartesian_rep::find_closest_point (point p) {
  double x,y,ssubd;
  ssubd= ((double) subd[N(subd)-1]) / step;
  if (ssubd==0) return p;
  p= p-o;
  x= nearest (p[0]*ssubd);
  y= nearest (p[1]*ssubd);
  return o + point (x/ssubd, y/ssubd);
}

grid
cartesian (array<SI> subd, array<color> col, point o, SI step) {
  return new cartesian_rep (subd, col, o, step);
}

/******************************************************************************
* Polar grids
******************************************************************************/

struct polar_rep: public grid_rep {
  SI step;      // Step (on the screen)
  SI astep;     // # angles
  polar_rep (array<SI> subd, array<color> col, point o, SI st, SI ast):
    grid_rep (subd, col, o), step (st), astep (ast) {}
  operator tree () { return "polar"; }
  void display (ps_device dev, SI x1, SI y1, SI x2, SI y2);
  point find_closest_point (point p);
};

void
polar_rep::display (ps_device dev, SI x1, SI y1, SI x2, SI y2) {
  SI ox1,oy1,ox2,oy2;
  dev->get_clipping (ox1,oy1,ox2,oy2);
  dev->set_clipping (x1,y1,x2,y2);
  SI xo,yo;
  xo=(SI)o[0];
  yo=(SI)o[1];
  SI r,R= (SI) norm (point (x2, y2) - point (x1, y1));
  dev->set_line_style (PIXEL);
  SI i;
  for (i= N(subd)-1; i>=1; i--) {
     SI nsub;
     nsub= subd[i];
     dev->set_color (col[i]);
     if (nsub!=0) {
       SI j;
       for (r=0; r<=R; r+=step/nsub)
         dev->arc (xo-r,yo-r,xo+r,yo+r,0,360<<6);
       for (j=0; j<astep*nsub; j++)
         dev->line (xo,yo,(SI)(xo+R*cos((2*PI*j)/(astep*nsub))),
                          (SI)(yo+R*sin((2*PI*j)/(astep*nsub))));
     }
  }
  dev->set_color (col[0]);
  dev->line (x1,yo,x2,yo);
  dev->line (xo,y1,xo,y2);
  dev->set_clipping (ox1,oy1,ox2,oy2);
}

point
polar_rep::find_closest_point (point p) {
  double n,a,ssubd;
  ssubd=(double)subd[N(subd)-1];
  if (ssubd==0) return p;
  p=p-o;
  n=nearest (norm(p)*(ssubd/step));
  a=nearest ((arg(p)/(2*PI))*astep*ssubd);
  n=n*(step/ssubd);
  a=a/(astep*ssubd);
  return o + n * point (cos(2*PI*a), sin(2*PI*a));
}

grid
polar (array<SI> subd, array<color> col, point o, SI step, SI astep) {
  return new polar_rep (subd, col, o, step, astep);
}

/******************************************************************************
* Logarithmic grids
******************************************************************************/

struct logarithmic_rep: public grid_rep {
  SI step; // Step (on the screen)
  SI base;
  logarithmic_rep (array<SI> subd, array<color> col, point o, SI st, SI b):
    grid_rep (subd, col, o), step (st), base (b) {}
  operator tree () { return "logarithmic"; }
  void display (ps_device dev, SI x1, SI y1, SI x2, SI y2);
  point find_closest_point (point p);
};

void
logarithmic_rep::display (ps_device dev, SI x1, SI y1, SI x2, SI y2) {
  SI ox1,oy1,ox2,oy2;
  dev->get_clipping (ox1,oy1,ox2,oy2);
  dev->set_clipping (x1,y1,x2,y2);
  dev->set_line_style (PIXEL);
  SI xo,yo;
  xo=(SI)o[0];
  yo=(SI)o[1];
  dev->set_color (col[2]);
  SI i,x,y;
  for (i=2;i<base;i++) {
    SI dx,dy;
    dx=dy=(SI)(step*log(i)/log(base));
    for (x=xo; x<=x2; x+=step)
      dev->line (x+dx,y1,x+dx,y2);
    for (x=xo-step; x>=x1-step; x-=step)
      dev->line (x+dx,y1,x+dx,y2);
    for (y=yo; y<=y2; y+=step)
      dev->line (x1,y+dy,x2,y+dy);
    for (y=yo-step; y>=y1-step; y-=step)
      dev->line (x1,y+dy,x2,y+dy);
  }
  dev->set_color (col[1]);
  for (x=xo; x<=x2; x+=step)
    dev->line (x,y1,x,y2);
  for (x=xo; x>=x1; x-=step)
    dev->line (x,y1,x,y2);
  for (y=yo; y<=y2; y+=step)
    dev->line (x1,y,x2,y);
  for (y=yo; y>=y1; y-=step)
    dev->line (x1,y,x2,y);
  dev->set_color (col[0]);
  dev->line (x1,yo,x2,yo);
  dev->line (xo,y1,xo,y2);
  dev->set_clipping (ox1,oy1,ox2,oy2);
}

point
logarithmic_rep::find_closest_point (point p) {
  SI xo,yo;
  xo=(SI)o[0];
  yo=(SI)o[1];
  SI x1,y1,x2,y2;
  x1=y1=xo-step;
  x2=y2=xo+step;
  p=p-o;
  SI x0,y0;
  x0=(SI)(p[0]/step);
  y0=(SI)(p[1]/step);
  x0*=step;
  y0*=step;
  p=p-point(x0,y0);
  p=o+p;
  double xm,ym;
  xm=ym=tm_infinity/2;
  SI i;
  for (i=1;i<base;i++) {
    SI x,y,dx,dy;
    dx=dy=(SI)(step*log(i)/log(base));
    for (x=xo; x<=x2; x+=step) 
      if (norm(p[0]-x-dx)<norm(xm-x-dx)) xm=x;
    for (x=xo-step; x>=x1-step; x-=step)
      if (norm(p[0]-x-dx)<norm(xm-x-dx)) xm=x;
    for (y=yo; y<=y2; y+=step)
      if (norm(p[1]-y-dy)<norm(ym-y-dy)) ym=y;
    for (y=yo-step; y>=y1-step; y-=step)
      if (norm(p[1]-y-dy)<norm(ym-y-dy)) ym=y;
  }
  return point (x0+xm, x0+ym);
}

grid
logarithmic (array<SI> subd, array<color> col, point o, SI step, SI base) {
  return new logarithmic_rep (subd, col, o, step, base);
}
