
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

/******************************************************************************
* Properties of the tag
******************************************************************************/

parent_info::parent_info (int a, int x, int am, int cm, bool frozen) {
  arity_mode       = am;
  arity_min        = a;
  arity_extra      = x;
  child_mode       = cm;
  no_border        = false;
  block            = false;
  dynamic          = false;
  freeze_child     = frozen;
  freeze_arity     = frozen;
  freeze_no_border = frozen;
  freeze_block     = frozen;
  freeze_dynamic   = frozen;
}

/******************************************************************************
* Properties of the children of the tag
******************************************************************************/

child_info::child_info (bool frozen) {
  accessible       = 0;
  block            = 0;
  freeze_accessible= frozen;
  freeze_block     = frozen;
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

/******************************************************************************
* Properties of the children of the tag
******************************************************************************/

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
