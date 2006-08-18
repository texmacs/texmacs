
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

//#define ENABLE_TESTS
#ifdef ENABLE_TESTS
#include "math_tree.hpp"
#include "vector.hpp"
#include "matrix.hpp"
#include "ball.hpp"
#include "function.hpp"

void
test_math () {
  {
    tree t= add ("x", mul (pow ("y", "2"), "z"));
    cout << "t\t= " << as_math_string (t) << "\n";
    cout << "t*t\t= " << as_math_string (mul (t, t)) << "\n";
    cout << "[t,t]\t= " << vector<tree> (t, t) << "\n";
    cout << "\n";
  }

  {
    vector<double> v (1.0, 2.0, 3.0);
    cout << "v\t= " << v << "\n";
    cout << "exp v\t= " << exp (v) << "\n";
    cout << "|v|\t= " << norm (v) << "\n";
    cout << "\n";
  }

  {
    vector<double> v (1.0, 2.0);
    matrix<double> m (1.0, 2, 2);
    m (0, 1)= 4;
    cout << "m\t= " << m << "\n";
    cout << "v\t= " << v << "\n";
    cout << "m*m\t= " << m*m << "\n";
    cout << "m*v\t= " << m*v << "\n";
    cout << "\n";
  }

  {
    ball<double> b (1.0, 0.1);
    cout << "b\t= " << b << "\n";
    cout << "b+b\t= " << b+b << "\n";
    cout << "exp b\t= " << exp (b) << "\n";
    cout << "\n";
  }

  {
    ball<double> b (1.0, 0.1);
    function<double,double> x= coordinate_function<double,double> (0);
    function<double,double> f= pow (x, x);
    cout << "x\t= " << x << "\n";
    cout << "x+x\t= " << x+x << "\n";
    cout << "exp x\t= " << exp (x) << "\n";
    cout << "f(x)\t= " << f << "\n";
    cout << "f'(x)\t= " << derive (f, 0) << "\n";
    cout << "f''(x)\t= " << derive (derive (f, 0), 0) << "\n";
    cout << "f(2)\t= " << f (2) << "\n";
    cout << "f'(2)\t= " << derive (f, 0) (2) << "\n";
    cout << "b\t= " << b << "\n";
    cout << "f(b)\t= " << f (b) << "\n";
    cout << "\n";
  }

  {
    vector<double> v (1.0, 2.0);
    ball<vector<double> > b (v, 0.01);
    function<vector<double>,double> x1=
      coordinate_function<vector<double>,double> (0);
    function<vector<double>,double> x2=
      coordinate_function<vector<double>,double> (1);
    function<vector<double>,double> f= exp (sin (x1) * x2);
    cout << "x1\t= " << x1 << "\n";
    cout << "x2\t= " << x2 << "\n";
    cout << "x1+x2\t= " << x1+x2 << "\n";
    cout << "f\t= " << f << "\n";
    cout << "f_1\t= " << derive (f, 0) << "\n";
    cout << "f_12\t= " << derive (derive (f, 0), 1) << "\n";
    cout << "b\t= " << b << "\n";
    cout << "f(b)\t= " << f (b) << "\n";
    cout << "\n";
  }

  {
    ball<double> b (1.0, 0.1);
    vector<double> vt (1.0, 0.0);
    vector<double> vu (0.0, 1.0);
    function<double,vector<double> > t=
      coordinate_function<double,vector<double> > (0, vt);
    function<double,vector<double> > u=
      coordinate_function<double,vector<double> > (0, vu);
    function<double,vector<double> > f= exp (t * u) + t;
    cout << "t\t= " << t << "\n";
    cout << "u\t= " << u << "\n";
    cout << "t+u\t= " << t+u << "\n";
    cout << "f\t= " << f << "\n";
    cout << "b\t= " << b << "\n";
    cout << "f(b)\t= " << f (b) << "\n";
    cout << "\n";
  }
}

#endif // defined ENABLE_TESTS
