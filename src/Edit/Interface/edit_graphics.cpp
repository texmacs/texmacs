
/******************************************************************************
* MODULE     : edit_graphics.cpp
* DESCRIPTION: graphics between the editor and the window manager
* COPYRIGHT  : (C) 2003  Joris van der Hoeven and Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Interface/edit_graphics.hpp"
#include "server.hpp"
#include "scheme.hpp"
#include "curve.hpp"
#include "Boxes/graphics.hpp"
#include "Bridge/impl_typesetter.hpp"
#include "drd_std.hpp"

extern tree the_et;

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_graphics_rep::edit_graphics_rep () {
  p_x= p_y= 0.0;
  gr_x= gr_y= 0.0;
  graphical_object= tree();
}

edit_graphics_rep::~edit_graphics_rep () {}

/******************************************************************************
* Extra subroutines for graphical selections
******************************************************************************/

static tree snap_mode;
static SI snap_distance;

void
set_snap_mode (tree t) {
  //cout << "Snap mode= " << t << "\n";
  snap_mode= t;
}

void
set_snap_distance (SI d) {
  //cout << "Snap distance= " << d << "\n";
  snap_distance= d;
}

bool
check_snap_mode (string type) {
  if (!is_tuple (snap_mode)) return true;
  for (int i=0; i<N(snap_mode); i++)
    if (snap_mode[i] == "all") return true;
    else if (snap_mode[i] == type) return true;
  return false;
}

bool
can_snap (gr_selection sel) {
  string type= sel->type;
  if (type == "free")
    return true;
  if (type == "box")
    return false;
  if (type == "point")
    return check_snap_mode ("control point");
  if (type == "curve-handle")
    return check_snap_mode ("control point");
  if (type == "curve-point")
    return check_snap_mode ("curve point");
  if (type == "curve-point&curve-point")
    return check_snap_mode ("curve-curve intersection");
  if (type == "grid-point")
    return check_snap_mode ("grid point");
  if (type == "grid-curve-point")
    return check_snap_mode ("grid curve point");
  if (type == "curve-point&grid-curve-point")
    return check_snap_mode ("curve-grid intersection");
  if (type == "grid-curve-point&curve-point")
    return check_snap_mode ("curve-grid intersection");
  if (type == "text" || type == "group")
    return check_snap_mode ("text");
  if (type == "text-handle")
    return check_snap_mode ("control point");
  if (type == "text-border")
    return check_snap_mode ("text border");
  if (type == "text-border-point")
    return check_snap_mode ("text border point");
  if (type == "text-border&grid-curve-point")
    return check_snap_mode ("text border") &&
           check_snap_mode ("curve-curve intersection");
  if (type == "grid-curve-point&text-border")
    return check_snap_mode ("text border") &&
           check_snap_mode ("curve-curve intersection");
  cout << "Uncaptured snap type " << type << "\n";
  return true;
}

gr_selection
snap_to_guide (point p, gr_selections sels, double eps) {
  if (N(sels) == 0) {
    gr_selection snap;
    snap->type= "free";
    snap->p= p;
    snap->dist= 0;
    return snap;
  }

  sort (sels);
  gr_selection best;
  best->type= "none";
  for (int i=0; i<N(sels); i++)
    if (can_snap (sels[i])) {
      if (sels[i]->type == "grid-point")
        best= sels[i];
      else if (is_nil (sels[i]->c))
        return sels[i];
    }

  for (int i=0; i<N(sels); i++)
    for (int j=i+1; j<N(sels); j++) {
      if (!is_nil (sels[i]->c) &&
          !is_nil (sels[j]->c) &&
          (sels[i]->type != "grid-curve-point" ||
           sels[j]->type != "grid-curve-point") &&
          !ends (sels[i]->type, "handle") &&
          !ends (sels[j]->type, "handle"))
        {
          array<point> ins= intersection (sels[i]->c, sels[j]->c, p, eps);
          for (int k=0; k<N(ins); k++)
            if (best->type == "none" || norm (ins[k] - p) < best->dist) {
              gr_selection sel;
              sel->type= sels[i]->type * "&" * sels[j]->type;
              sel->p   = ins[k];
              sel->dist= (SI) norm (ins[k] - p);
              sel->cp  = append (sels[i]->cp, sels[j]->cp);
              sel->pts = append (sels[i]->pts, sels[j]->pts);
              if (can_snap (sel)) best= sel;
            }
        }
    }

  if (best->type != "none") return best;
  if (can_snap (sels[0])) return sels[0];
  else {
    gr_selection snap;
    snap->type= "free";
    snap->p= p;
    snap->dist= 0;
    return snap;
  }
}

/******************************************************************************
* Main edit_graphics routines
******************************************************************************/

path
edit_graphics_rep::graphics_path () {
  path gp= search_upwards (GRAPHICS);
  if (is_nil (gp)) return tp;
  return gp * 0;
}

bool
edit_graphics_rep::inside_graphics (bool b) {
  path p   = path_up (tp);
  bool flag= false;
  tree st  = et;
  while (!is_nil (p)) {
    if (is_func (st, GRAPHICS)) flag= true;
    if (b && is_graphical_text (st)) flag= false;
    if (is_atomic (st) || p->item < 0 || p->item >= N(st)) break;
    st= st[p->item];
    p = p->next;
  }
  return flag || (L(st) == GRAPHICS);
}

bool
edit_graphics_rep::inside_active_graphics (bool b) {
  return inside_graphics (b) && get_env_string (PREAMBLE) == "false";
}

bool
edit_graphics_rep::over_graphics (SI x, SI y) {
  frame f= find_frame ();
  if (!is_nil (f)) {
    point lim1, lim2;
    find_limits (lim1, lim2);
    point p = adjust (f [point (x, y)]);
    // cout << type << " at " << p << " [" << lim1 << ", " << lim2 << "]\n";
    if (N(lim1) == 2)
      if ((p[0]<lim1[0]) || (p[0]>lim2[0]) || (p[1]<lim1[1]) || (p[1]>lim2[1]))
        return as_bool (call ("graphics-busy?"));
    return true;
  }
  return false;
}

tree
edit_graphics_rep::get_graphics () {
  path p   = path_up (tp);
  tree st  = et;
  tree res = tree ();
  while (!is_nil (p)) {
    if (is_func (st, GRAPHICS)) res= st;
    st= st[p->item];
    p = p->next;
  }
  return res;
}

double
edit_graphics_rep::get_x () {
  return gr_x;
}

double
edit_graphics_rep::get_y () {
  return gr_y;
}

frame
edit_graphics_rep::find_frame (bool last) {
  path gp= graphics_path ();
  bool bp_found;
  path bp= eb->find_box_path (gp, bp_found);
  if (bp_found) return eb->find_frame (path_up (bp), last);
  else return frame ();
}

grid
edit_graphics_rep::find_grid () {
  path gp= graphics_path ();
  bool bp_found;
  path bp= eb->find_box_path (gp, bp_found);
  if (bp_found) return eb->find_grid (path_up (bp));
  else return grid ();
}

void
edit_graphics_rep::find_limits (point& lim1, point& lim2) {
  path gp= graphics_path ();
  lim1= point (); lim2= point ();
  bool bp_found;
  path bp= eb->find_box_path (gp, bp_found);
  if (bp_found) eb->find_limits (path_up (bp), lim1, lim2);
  if (N(lim1) >= 2 && fabs (lim1[0]) <= 0.001) lim1[0]= 0.0;
  if (N(lim1) >= 2 && fabs (lim1[1]) <= 0.001) lim1[1]= 0.0;
  if (N(lim2) >= 2 && fabs (lim2[0]) <= 0.001) lim2[0]= 0.0;
  if (N(lim2) >= 2 && fabs (lim2[1]) <= 0.001) lim2[1]= 0.0;
}

bool
edit_graphics_rep::find_graphical_region (SI& x1, SI& y1, SI& x2, SI& y2) {
  point lim1, lim2;
  find_limits (lim1, lim2);
  if (lim1 == point ()) return false;
  frame f= find_frame ();
  if (is_nil (f)) return false;
  point p1= f (point (lim1[0], lim1[1]));
  point p2= f (point (lim2[0], lim2[1]));
  x1= (SI) p1[0]; y1= (SI) p1[1];
  x2= (SI) p2[0]; y2= (SI) p2[1];
  return true;
}

point
edit_graphics_rep::adjust (point p) {
  frame f= find_frame ();
  grid g= find_grid ();
  if (!is_nil (g) && !is_nil (gr0) &&
      (g != gr0 || p[0] != p_x || p[1] != p_y)) {
    graphical_select (p[0], p[1]);
    g= gr0;
    p[0]= p_x;
    p[1]= p_y;
  }
  if (is_nil (g)) return p;
  point res;
  gr_selections sels= copy (gs);
  frame f2= find_frame (true);
  if (is_nil (f2)) return p;
  point fp= f2 (p);
  if ((tree) g != "empty_grid") {
    point q= g->find_point_around (p, snap_distance, f);
    point fq= f2 (q);
    if (norm (fq - fp) < snap_distance) {
      gr_selection sel;
      sel->type= "grid-point";
      sel->p   = fq;
      sel->dist= (SI) norm (fq - fp);
      sels << sel;
    }
    array<grid_curve> gc=
      g->get_curves_around (p, snap_distance, f);
    for (int i=0; i<N(gc); i++) {
      point fc= closest (f2 (gc[i]->c), fp);
      if (norm (fc - fp) < snap_distance) {
        gr_selection sel;
        sel->type= "grid-curve-point";
        sel->p   = fc;
        sel->dist= (SI) norm (fc - fp);
        sel->c   = f2 (gc[i]->c);
        sels << sel;
      }
    }
  }
  double eps= get_pixel_size () / 10.0;
  gr_selection snap= snap_to_guide (fp, sels, eps);
  //cout << "Snap " << fp << " to " << snap << ", " << snap->p << "\n";
  point snapped= f2[snap->p];
  if (N(snapped) == 2) return snapped;
  return p;
  // FIXME: why can snapped be an invalid point?
}

tree
edit_graphics_rep::find_point (point p) {
  return tree (_POINT, as_string (p[0]), as_string (p[1]));
}

bool
admissible_selection (gr_selection sel) {
  if (sel->type != "box" || N(sel->cp) != 1) return true;
  if (last_item (sel->cp[0]) < 0 || N(sel->cp[0]) <= 2) return true;
  path p= path_up (sel->cp[0]);
  if (!has_subtree (the_et, p)) return true;
  tree st= subtree (the_et, p);
  if (is_compound (st, "anim-edit")) return false;
  tree pt= subtree (the_et, path_up (p));
  if (is_func (st, WITH) && is_compound (pt, "anim-edit")) return false;
  return true;
}

tree
edit_graphics_rep::graphical_select (double x, double y) { 
  frame f= find_frame ();
  if (is_nil (f)) return tuple ();
  gr_selections pre_sels, sels;
  point p0 = point (x, y);
  point p = f (p0);
  pre_sels= eb->graphical_select ((SI)p[0], (SI)p[1], snap_distance);
  for (int i=0; i<N(pre_sels); i++)
    if (admissible_selection (pre_sels[i]))
      sels << pre_sels[i];
  //for (int i=0; i<N(sels); i++)
  //  cout << i << ":\t" << sels[i] << "\n";
  gs= sels;
  gr0= empty_grid ();
  grid g= find_grid ();
  frame f2= find_frame (true);
  if (!is_nil (g) && !is_nil (f2)) {
    gr0= g;
    p_x= x;
    p_y= y;
  }
  return as_tree (sels);
}

tree
edit_graphics_rep::graphical_select (
  double x1, double y1, double x2, double y2)
{ 
  frame f= find_frame ();
  if (is_nil (f)) return tuple ();
  gr_selections sels;
  point p1 = f (point (x1, y1)), p2= f (point (x2, y2));
  sels= eb->graphical_select ((SI)p1[0], (SI)p1[1], (SI)p2[0], (SI)p2[1]);
  return as_tree (sels);
}

tree
edit_graphics_rep::get_graphical_object () {
  return graphical_object;
}

void
edit_graphics_rep::set_graphical_object (tree t) {
  go_box= box ();
  graphical_object= t;
  if (N (graphical_object) == 0) return;
  edit_env env= get_typesetter ()->env;
  //tree old_fr= env->local_begin (GR_FRAME, (tree) find_frame ());  
  frame f_env= env->fr;
  env->fr= find_frame ();
  if (!is_nil (env->fr)) {
    int i,n=0;
    go_box= typeset_as_concat (env, t, path (0));
    for (i=0; i<N(go_box); i++)
      if (go_box[i]!="") n++;
    if (n) {
      array<box> bx(n);
      n=0;
      for (i=0; i<N(go_box); i++) if (go_box[i]!="") {
        array<box> bx2(1);
        array<SI> spc2(1);
        bx2[0]= go_box[i];
        spc2[0]=0;
        bx[n]= concat_box (path (0), bx2, spc2);
        n++;
      }
      go_box= composite_box (path (0), bx);
    }
  }
  env->fr= f_env;
  //env->local_end (GR_FRAME, old_fr);
}

void
edit_graphics_rep::invalidate_graphical_object () {
  SI gx1, gy1, gx2, gy2;
  if (!is_nil (eb) &&
      find_graphical_region (gx1, gy1, gx2, gy2) &&
      !is_nil (go_box)) {
    int i;
    rectangles rs;
    rectangle gr (gx1, gy1, gx2, gy2);
    for (i=0; i<go_box->subnr(); i++) {
      box b= go_box->subbox (i);
      rs= rectangles (rectangle (b->x3, b->y3, b->x4, b->y4), rs);
    }
    rs= rs & rectangles (gr);
    invalidate (rs);
  }
}

void
edit_graphics_rep::draw_graphical_object (renderer ren) {
  if (is_nil (go_box)) set_graphical_object (graphical_object);
  if (is_nil (go_box)) return;
  SI ox1, oy1, ox2, oy2;
  ren->get_clipping (ox1, oy1, ox2, oy2);
  SI gx1, gy1, gx2, gy2;
  if (find_graphical_region (gx1, gy1, gx2, gy2))
    ren->extra_clipping (gx1, gy1, gx2, gy2);
  int i;
  for (i=0; i<go_box->subnr(); i++) {
    box b= go_box->subbox (i);
    if ((tree)b=="point" || (tree)b=="curve")
      b->display (ren);
    else {
      rectangles rs;
      b->redraw (ren, path (), rs);
    }
  }
  ren->set_clipping (ox1, oy1, ox2, oy2);
}

void
edit_graphics_rep::back_in_text_at (tree t, path p, bool forward) {
  (void) forward;
  int i= last_item (p);
  if ((i == 0) && is_empty (t[0])) {
    p= path_up (p);
    if (is_func (subtree (et, path_up (p)), WITH)) p= path_up (p);
    tree st= subtree (et, path_up (p));
    if (is_func (st, GRAPHICS)) {
      if (N(st) == 1) assign (p, "");
      else {
        remove (p, 1);
        go_to_border (path_up (p) * 0, true);
      }
    }
  }
}

bool
edit_graphics_rep::mouse_graphics (string type, SI x, SI y, int m, time_t t) {
  //cout << type << ", " << x << ", " << y << ", " << m << ", " << t << "\n";
  //cout << "et= " << et << "\n";
  //cout << "tp= " << tp << "\n";
  //cout << "gp= " << graphics_path () << "\n";
  (void) t;
  // apply_changes (); // FIXME: remove after review of synchronization
  frame f= find_frame ();
  if (!is_nil (f)) {
    if (!over_graphics (x, y))
      return false;
    if (type == "move" || type == "dragging-left")
      if (check_event (MOTION_EVENT))
        return true;
    point p = f [point (x, y)];
    graphical_select (p[0], p[1]); // init the caching for adjust().
    p= adjust (p);
    gr_x= p[0];
    gr_y= p[1];
    string sx= as_string (p[0]);
    string sy= as_string (p[1]);
    invalidate_graphical_object ();
    call ("set-keyboard-modifiers", object (m));
    if (type == "move")
      call ("graphics-move", sx, sy);
    else if (type == "release-left" || type == "double-left")
      call ("graphics-release-left", sx, sy);
    else if (type == "release-middle")
      call ("graphics-release-middle", sx, sy);
    else if (type == "release-right" || type == "double-right")
      call ("graphics-release-right", sx, sy);
    else if (type == "start-drag-left")
      call ("graphics-start-drag-left", sx, sy);
    else if (type == "dragging-left")
      call ("graphics-dragging-left", sx, sy);
    else if (type == "end-drag-left")
      call ("graphics-end-drag-left", sx, sy);
    else if (type == "start-drag-right")
      call ("graphics-start-drag-right", sx, sy);
    else if (type == "dragging-right")
      call ("graphics-dragging-right", sx, sy);
    else if (type == "end-drag-right")
      call ("graphics-end-drag-right", sx, sy);
    invalidate_graphical_object ();
    notify_change (THE_CURSOR);
    return true;
  }
  //cout << "No frame " << tp << ", " << subtree (et, path_up (tp)) << "\n";
  return false;
}
