
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

tag_info_rep::tag_info_rep (int arity2, int props2) {
  arity= arity2;
  props= props2;
}

tag_info::tag_info (int arity, int props) {
  rep= new tag_info_rep (arity, props);
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
  return tag_info (ti->arity, ti->props);
}
