
/******************************************************************************
* MODULE     : equations.hpp
* DESCRIPTION: Some tools for solving systems of equations
* COPYRIGHT  : (C) 2004  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef EQUATIONS_H
#define EQUATIONS_H
#include "point.hpp"

void tridiag_solve (array<double> a, array<double> b, array<double> c,
		    array<point> x, array<point> y, int n);

void quasitridiag_solve (array<double> a, array<double> b, array<double> c,
			 array<double> u, array<double> v,
			 array<point> x, array<point> y, int n);

void xtridiag_solve (array<double> a, array<double> b, array<double> c,
		     double a0, double a1,
		     array<point> x, array<point> y, int n);

#endif // defined EQUATIONS_H
