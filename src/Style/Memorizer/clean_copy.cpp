
/******************************************************************************
* MODULE     : clean_copy.cpp
* DESCRIPTION: maintain a clean copy of the edit tree
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "clean_copy.hpp"

/******************************************************************************
* Attaching ips
******************************************************************************/

void
copy_ip (tree src, tree dest) {
  path src_ip= obtain_ip (src);
  path dest_ip= obtain_ip (dest);
  if (dest_ip != src_ip) {
    dest->obs= list_observer (ip_observer (src_ip), dest->obs);
    if (is_compound (src)) {
      int i, n= N(src);
      for (i=0; i<n; i++)
	copy_ip (src[i], dest[i]);
    }
  }
}

/******************************************************************************
* Clean copy callbacks
******************************************************************************/

void
copy_announce (tree src, tree& cct, modification mod) {
  //cout << "Announce copy " << mod << "\n";
  cct= clean_apply (cct, mod);
  copy_ip (src, cct);
}
