
/******************************************************************************
* MODULE     : equations.hpp
* DESCRIPTION: Some tools for solving systems of equations
* COPYRIGHT  : (C) 2004  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
