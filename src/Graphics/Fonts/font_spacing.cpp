
/******************************************************************************
* MODULE     : font_spacing.cpp
* DESCRIPTION: manage font spacing tables
* COPYRIGHT  : (C) 2018  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "language.hpp"

/******************************************************************************
* Manage global profile tables
******************************************************************************/

hashmap<tree,int> spacing_desc_id (0);
hashmap<int,tree> spacing_id_desc ("");

int
get_spacing_id (tree spacing_desc) {
  if (!spacing_desc_id->contains (spacing_desc)) {
    int new_id= N(spacing_desc_id);
    spacing_desc_id (spacing_desc)= new_id;
    spacing_id_desc (new_id)= spacing_desc;
  }
  return spacing_desc_id [spacing_desc];
}

tree
get_spacing_desc (int spacing_id) {
  ASSERT (spacing_id_desc->contains (spacing_id),
          "invalid spacing identifier");
  return spacing_id_desc [spacing_id];
}

/******************************************************************************
* Filling out the spacing tables
******************************************************************************/

array<space>
font_rep::get_spacing_table (int mode, int id, array<array<space> >& tab) {
  while (id >= N(tab)) {
    array<space> new_el;
    tree t= get_spacing_desc (N(tab));
    for (int i=0; i<SPC_END_MARKER; i++)
      new_el << get_spacing_entry (mode, t, i);
    tab << new_el;
  }
  return tab[id];
}

space
font_rep::get_spacing_entry (int mode, tree t, int i) {
  switch (i) {
  case SPC_NONE:
    return space (0);
  case SPC_THIN_SPACE:
    if (t == "default" || t == "old" || t == "wide")
      return (6 * spc) / 10;
    return get_spacing_entry (mode, t, i, "thin-space");
  case SPC_SPACE:
    if (t == "default" || t == "old" || t == "wide")
      return spc;
    return get_spacing_entry (mode, t, i, "space");
  case SPC_DSPACE:
    if (t == "default" || t == "old" || t == "wide")
      return space (spc->min<<1, spc->def<<1, spc->max<<1);
    return get_spacing_entry (mode, t, i, "dspace");
  case SPC_PERIOD:
    if (t == "default" || t == "old" || t == "wide")
      return spc + extra;
    return get_spacing_entry (mode, t, i, "period");
  case SPC_TINY:
    if (t == "default" || t == "old" || t == "wide")
      return space (spc->min>>2, spc->def>>2, spc->max>>2);
    return get_spacing_entry (mode, t, i, "tiny");
  case SPC_CJK_NORMAL:
    if (t == "default" || t == "old" || t == "wide")
      return space (-(spc->min>>5), 0, spc->max>>5);
    return get_spacing_entry (mode, t, i, "cjk-normal");
  case SPC_CJK_PERIOD:
    if (t == "default" || t == "old" || t == "wide")
      return space (-(spc->min>>2), 0, spc->max>>1);
    return get_spacing_entry (mode, t, i, "cjk-period");
  case SPC_CJK_WIDE_PERIOD:
    if (t == "default" || t == "old" || t == "wide")
      return spc + extra;
    return get_spacing_entry (mode, t, i, "cjk-wide-period");
  case SPC_HALF:
    if (t == "default" || t == "wide") {
      if (mode >= 0) return space (spc->min>>2, spc->def>>2, spc->max>>2);
      else return space (spc->min>>4, spc->def>>4, spc->max>>4);
    }
    else if (t == "old") {
      if (mode >= 0) return space (spc->min>>1, spc->def>>2, spc->max>>1);
      else return space (spc->min>>3, spc->def>>4, spc->max>>3);
    }
    return get_spacing_entry (mode, t, i, "half");
  case SPC_OPERATOR:
    if (t == "default" || t == "wide") {
      if (mode >= 0) return space (spc->min>>1, spc->def>>1, spc->max>>1);
      else return space (spc->min>>3, spc->def>>3, spc->max>>3);
    }
    else if (t == "old") {
      if (mode >= 0) return space (spc->min>>1, spc->def>>1, spc->max);
      else return space (spc->min>>3, spc->def>>3, spc->max>>2);
    }
    return get_spacing_entry (mode, t, i, "operator");
  case SPC_WIDEOP:
    if (t == "default") {
      if (mode >= 0) return space (spc->min>>1, spc->def>>1, spc->max>>1);
      else return space (spc->min>>3, spc->def>>3, spc->max>>3);
    }
    else if (t == "wide") {
      if (mode == 0) return spc;
      else if (mode > 0) return space (spc->min<<1, spc->def<<1, spc->max<<1);
      else return space (spc->min>>2, spc->def>>2, spc->max>>2);
    }
    else if (t == "old") {
      if (mode >= 0) return space (spc->min>>1, spc->def>>1, spc->max);
      else return space (spc->min>>3, spc->def>>3, spc->max>>2);
    }
    return get_spacing_entry (mode, t, i, "wideop");
  case SPC_BIGOP:
    if (t == "default" || t == "old" || t == "wide") {
      if (mode >= 0) return spc;
      else return space (spc->min>>2, spc->def>>2, spc->max>>2);;
    }
    return get_spacing_entry (mode, t, i, "bigop");
  case SPC_SHORT_APPLY:
    if (t == "default" || t == "old" || t == "wide") {
      if (mode >= 0) return space (spc->min>>2, spc->def>>2, spc->max>>2);
      else return space (spc->min>>4, spc->def>>4, spc->max>>4);
    }
    return get_spacing_entry (mode, t, i, "short-apply");
  case SPC_APPLY:
    if (t == "default" || t == "wide") {
      if (mode >= 0) return space (mspc->min>>1, mspc->def>>1, mspc->max>>1);
      else return space (mspc->min>>3, mspc->def>>3, mspc->max>>3);
    }
    else if (t == "old") {
      if (mode >= 0) return space (mspc->min>>1, mspc->def>>1, mspc->max);
      else return space (mspc->min>>3, mspc->def>>3, mspc->max>>2);
    }
    return get_spacing_entry (mode, t, i, "apply");
  case SPC_MULTIPLY:
    if (t == "default" || t == "wide") {
      if (mode >= 0) return space (spc->min>>2, spc->def>>2, spc->max>>2);
      else return space (spc->min>>4, spc->def>>4, spc->max>>4);
    }
    else if (t == "old") {
      if (mode >= 0) return space (spc->min>>1, spc->def>>2, spc->max>>1);
      else return space (spc->min>>3, spc->def>>4, spc->max>>3);
    }
    return get_spacing_entry (mode, t, i, "multiply");
  default:
    FAILED ("unimplemented type of space");
  }
}

space
font_rep::get_spacing_entry (int mode, tree t, int i, string kind) {
  if (is_tuple (t)) {
    for (int k=0; k<N(t); k+=2)
      if (k == N(t)-1)
        return get_spacing_entry (mode, t[k], i);
      else if (t[k] == kind)
        return get_spacing_val (mode, t[k+1]);
  }
  return get_spacing_entry (mode, "default", i);
}

space
font_rep::get_spacing_val (int mode, tree t) {
  if (is_atomic (t)) {
    string s= t->label;
    int i;
    for (i=0; i<N(s); i++)
      if ((s[i]>='a') && (s[i]<='z')) break;
    double val = as_double (s (0, i));
    string unit= s (i, N(s));
    if (unit == "spc") return val * spc;
    if (unit == "xspc") return val * extra;
    if (unit == "period") return val * (spc + extra);
    if (unit == "mspc") return val * mspc;
    if (unit == "em") return val * space (wquad);
    if (unit == "ex") return val * space (yx);
  }
  else if (is_func (t, TMLEN, 3)) {
    space s1= get_spacing_val (mode, t[0]);
    space s2= get_spacing_val (mode, t[1]);
    space s3= get_spacing_val (mode, t[2]);
    return space (s1->min, s2->def, s3->max);
  }
  else if (is_func (t, TUPLE, 3)) {
    if (mode <  0) return get_spacing_val (mode, t[0]);
    if (mode == 0) return get_spacing_val (mode, t[1]);
    if (mode >  0) return get_spacing_val (mode, t[2]);
  }
  FAILED ("invalid font spacing");
  return space (0);
}
