
/******************************************************************************
* MODULE     : test_math.cpp
* DESCRIPTION: Test mathematical functions
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifdef ENABLE_TESTS
#include "math_tree.hpp"
#include "vector.hpp"
#include "ball.hpp"
#include "function.hpp"

void
test_math () {
  tree t= add ("x", mul (pow ("y", "2"), "z"));
  cout << "t\t= " << as_math_string (t) << "\n";
  cout << "t*t\t= " << as_math_string (mul (t, t)) << "\n";

  vector<double> v (1.0, 2.0, 3.0);
  cout << "v\t= " << v << "\n";
  cout << "exp v\t= " << exp (v) << "\n";
  cout << "[t,t]\t= " << vector<tree> (t, t) << "\n";

  ball<double> b (1.0, 1.0);
  cout << "b\t= " << b << "\n";
  cout << "b+b\t= " << b+b << "\n";
  cout << "exp b\t= " << exp (b) << "\n";

  function<double,double> x= coordinate_function<double,double> (0);
  cout << "x\t= " << x << "\n";
  cout << "x+x\t= " << x+x << "\n";
  cout << "exp x\t= " << exp (x) << "\n";

  function<double,double> f= pow (x, x);
  cout << "f(x)\t= " << f << "\n";
  cout << "f'(x)\t= " << derive (f, 0) << "\n";
  cout << "f''(x)\t= " << derive (derive (f, 0), 0) << "\n";
  cout << "f(2)\t= " << f (2) << "\n";
  cout << "f'(2)\t= " << derive (f, 0) (2) << "\n";
}

#endif // defined ENABLE_TESTS
