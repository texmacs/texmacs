
/******************************************************************************
* MODULE     : encoding.hpp
* DESCRIPTION: font encodings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef ENCODING_H
#define ENCODING_H
#include "resource.hpp"

RESOURCE(encoding);

/******************************************************************************
* The encoding structure
******************************************************************************/

struct encoding_rep: rep<encoding> {
  inline encoding_rep (string name);
  virtual string encode (string s) = 0; // conversion from universal encoding
  virtual string decode (string s) = 0; // conversion to universal encoding
  virtual bool token_forward  (string s, int& pos) = 0;
  virtual bool token_backward (string s, int& pos) = 0;
  virtual bool valid (string s);
};

inline encoding_rep::encoding_rep (string s): rep<encoding>(s) {}

extern encoding universal_enc;
extern encoding always_enc;
extern encoding almost_always_enc;
extern encoding math_enc;

encoding num_enc ();
encoding capital_enc ();
encoding alpha_enc ();
encoding alpha_num_enc ();

encoding join (encoding enc1, encoding enc2);

#endif // defined ENCODING_H
