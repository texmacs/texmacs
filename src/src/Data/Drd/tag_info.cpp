
/******************************************************************************
* MODULE     : tag_info.cpp
* DESCRIPTION: DRD information about tags
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "tag_info.hpp"

#define get_bits(which,nr) which=i&((1<<nr)-1);i=i>>nr
#define set_bits(which,nr) i+=((int)which)<<offset;offset+=nr

/******************************************************************************
* Properties of the tag
******************************************************************************/

parent_info::parent_info (int a, int x, int am, int cm, bool frozen) {
  arity_mode       = am;
  arity_base       = a;
  arity_extra      = x;
  child_mode       = cm;
  no_border        = false;
  block            = false;
  freeze_arity     = frozen;
  freeze_no_border = frozen;
  freeze_block     = frozen;
}

parent_info::parent_info (string s) {
  int i= as_int (s);
  get_bits (arity_mode      , 2);
  get_bits (arity_base      , 6);
  get_bits (arity_extra     , 4);
  get_bits (child_mode      , 2);
  get_bits (no_border       , 1);
  get_bits (block           , 2);
  get_bits (freeze_arity    , 1);
  get_bits (freeze_no_border, 1);
  get_bits (freeze_block    , 1);
}

parent_info::operator string () {
  int i,offset=0;
  set_bits (arity_mode      , 2);
  set_bits (arity_base      , 6);
  set_bits (arity_extra     , 4);
  set_bits (child_mode      , 2);
  set_bits (no_border       , 1);
  set_bits (block           , 2);
  set_bits (freeze_arity    , 1);
  set_bits (freeze_no_border, 1);
  set_bits (freeze_block    , 1);
  return i;
}

bool
parent_info::operator == (const parent_info& pi) {
  return
    (arity_mode       == pi.arity_mode      ) &&
    (arity_base       == pi.arity_base      ) &&
    (arity_extra      == pi.arity_extra     ) &&
    (child_mode       == pi.child_mode      ) &&
    (no_border        == pi.no_border       ) &&
    (block            == pi.block           ) &&
    (freeze_arity     == pi.freeze_arity    ) &&
    (freeze_no_border == pi.freeze_no_border) &&
    (freeze_block     == pi.freeze_block    );
}

bool
parent_info::operator != (const parent_info& pi) {
  return !(operator == (pi));
}

/******************************************************************************
* Properties of the children of the tag
******************************************************************************/

child_info::child_info (bool frozen) {
  accessible        = 0;
  block             = 0;
  freeze_accessible = frozen;
  freeze_block      = frozen;
}

child_info::child_info (string s) {
  int i= as_int (s);
  get_bits (accessible       , 1);
  get_bits (block            , 2);
  get_bits (freeze_accessible, 1);
  get_bits (freeze_block     , 1);
}

child_info::operator string () {
  int i,offset=0;
  set_bits (accessible       , 1);
  set_bits (block            , 2);
  set_bits (freeze_accessible, 1);
  set_bits (freeze_block     , 1);
  return i;
}

bool
child_info::operator == (const child_info& ci) {
  return
    (accessible        == ci.accessible       ) &&
    (block             == ci.block            ) &&
    (freeze_accessible == ci.freeze_accessible) &&
    (freeze_block      == ci.freeze_block     );
}

bool
child_info::operator != (const child_info& ci) {
  return !(operator == (ci));
}

/******************************************************************************
* Constructors and destructors
******************************************************************************/

tag_info_rep::tag_info_rep (parent_info pi2, array<child_info> ci2):
  pi (pi2), ci (ci2)
{
}

tag_info_rep::tag_info_rep (int arity2, int props2): pi (0,0,0,0,0) {
  arity= arity2;
  props= props2;
}

tag_info_rep::tag_info_rep (int a, int x, int am, int cm, bool frozen):
  pi (a, x, am, cm, frozen),
  ci ((a+x)==0? 0: (cm==CHILD_UNIFORM? 1: (cm==CHILD_BIFORM? 2: (am+cm))))
{
  if (frozen) {
    int i, n= N(ci);
    for (i=0; i<n; i++)
      ci[i]= child_info (true);
  }
}

tag_info::tag_info (parent_info pi, array<child_info> ci) {
  rep= new tag_info_rep (pi, ci);
}

tag_info::tag_info (int arity, int props) {
  rep= new tag_info_rep (arity, props);
}

tag_info::tag_info (int a, int x, int am, int cm, bool frozen) {
  rep= new tag_info_rep (a, x, am, cm, frozen);
}

/******************************************************************************
* Further routines
******************************************************************************/

tag_info
tag_info_rep::no_border () {
  pi.no_border= true;
  return tag_info (pi, ci);
}

tag_info
tag_info_rep::accessible (int i) {
  ci[i].accessible= true;
  return tag_info (pi, ci);
}

child_info&
tag_info::operator () (int child, int n) {
  switch (rep->pi.child_mode) {
  case CHILD_UNIFORM:
    return rep->ci[0];
  case CHILD_BIFORM:
    if (rep->pi.arity_mode != ARITY_VAR_REPEAT) {
      if (child < ((int) rep->pi.arity_base)) return rep->ci[0];
      else return rep->ci[1];
    }
    else {
      if (child < (n-((int) rep->pi.arity_base))) return rep->ci[0];
      else return rep->ci[1];
    }
  case CHILD_DETAILED:
    if (((int) rep->pi.arity_mode) <= ARITY_OPTIONS)
      return rep->ci[child];
    else if (rep->pi.arity_mode == ARITY_REPEAT) {
      if (child < ((int) rep->pi.arity_base)) return rep->ci[child];
      else return rep->ci[(child-rep->pi.arity_base)%rep->pi.arity_extra+
			  rep->pi.arity_base];
    }
    else {
      if (child < (n-((int) rep->pi.arity_base)))
	return rep->ci[child%rep->pi.arity_extra];
      else return rep->ci[rep->pi.arity_base+rep->pi.arity_extra + child-n];
    }
  }
}

tag_info::operator tree () {
  return tree (TUPLE, "tag_info",
	       as_string (rep->arity), as_string (rep->props));
}

ostream&
operator << (ostream& out, tag_info ti) {
  out << "[ " << ti->arity << ", " << ti->props << " ]";
  return out;
}

tag_info
copy (tag_info ti) {
  tag_info ti2 (ti->arity, ti->props);
  ti2->pi= ti->pi;
  ti2->ci= copy (ti->ci);
  return ti2;
}

bool
operator == (tag_info ti1, tag_info ti2) {
  return (ti1->pi == ti2->pi) && (ti1->ci == ti2->ci);
}

bool
operator != (tag_info ti1, tag_info ti2) {
  return !(ti1 == ti2);
}
