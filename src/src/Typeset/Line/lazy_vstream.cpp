
/******************************************************************************
* MODULE     : lazy_vstream.cpp
* DESCRIPTION: Last pass for typesetting vstreams;
*              hyphenation and creation of page items
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Line/lazy_vstream.hpp"
#include "Boxes/construct.hpp"
#include "Line/lazy_paragraph.hpp"
array<line_item> typeset_concat_or_table (edit_env env, tree t, path ip);

lazy_vstream_rep::lazy_vstream_rep (
  path ip, tree channel2, array<page_item> l2, stack_border sb2):
    lazy_rep (LAZY_VSTREAM, ip),
    channel (channel2), l (l2), sb (sb2) {}

lazy_vstream_rep::operator tree () {
  return tuple ("vertical stream", channel);
}

lazy
make_lazy_vstream (edit_env env, tree t, path ip, tree channel) {
  SI width, d1, d2, d3, d4, d5, d6, d7;
  env->get_page_pars (width, d1, d2, d3, d4, d5, d6, d7);
  lazy lz= make_lazy (env, t, ip);
  lz= lz->produce (LAZY_VSTREAM, make_format_vstream (width, 0, 0));
  lazy_vstream lwvs= (lazy_vstream) lz;
  lwvs->channel= channel;
  /* Direct modification of y1 and y2 limits should be avoided */
  box& tb= lwvs->l[0]->b;
  if (tb->y2 < env->fn->y2) tb->y2= env->fn->y2;
  box& bb= lwvs->l[N(lwvs->l)-1]->b;
  if (bb->y1 > env->fn->y1) bb->y1= env->fn->y1;
  /*************************************************************/
  return lwvs;
}

box
format_vstream_as_box (
  array<page_item> l, path ip, int vpos, SI depth, SI height)
{
  int i, n= N(l);
  array<box> lines_bx (n);
  array<SI>  lines_ht (n);
  for (i=0; i<n; i++) {
    page_item item= copy (l[i]);
    lines_bx[i]= item->b;
    lines_ht[i]= item->spc->def;
  }

  box b= stack_box (ip, lines_bx, lines_ht);
  SI dy= 0, bot= 0, top= 0;
  if (n>0) {
    if (vpos>0) dy= b->sy (0);
    else if (vpos<0) dy= b->sy (n-1);
    else dy= (b->sy (n>>1) + b->sy ((n-1)>>1)) >> 1;
    if (depth  > 0) bot= max (0, depth + b[n-1]->y1);
    if (height > 0) top= max (0, height- b[0  ]->y2);
  }
  if (dy != 0) b= move_box (ip, b, 0, -dy);
  if ((top != 0) || (bot != 0))
    b= resize_box (ip, b, b->x1, b->y1 - bot, b->x2, b->y2 + top);
  return b;
}

box
typeset_as_paragraph (edit_env env, tree t, path ip) {
  // cout << "Typeset paragraph " << t << "\n";
  lazy_paragraph par (env, ip);
  par->a= typeset_concat_or_table (env, t, ip);
  par->format_paragraph ();
  array<page_item> l= par->sss->l;
  return format_vstream_as_box (l, ip, 1, -env->fn->y1, env->fn->y2);
}

lazy
lazy_vstream_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;
  if (request == LAZY_BOX) {
    int  vpos  = 1;
    SI   depth = 0;
    SI   height= 0;
    if (fm->type == FORMAT_CELL) {
      format_cell fc= (format_cell) fm;
      vpos  = fc->vpos;
      depth = fc->depth;
      height= fc->height;
    }
    box b= format_vstream_as_box (l, ip, vpos, depth, height);
    return make_lazy_box (b);
  }
  return lazy_rep::produce (request, fm);
}
