## Released under the GNU General Public License, see www.gnu.org
## Copyright 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert an Octave object to a Scheme expression
## Created: Sept 2002

function tmp=obj2scm(n,c)
	if (nargin<2)
		c=0;
	endif
	switch (typeinfo(n))
		case ("range")
			tmp=mat2scm(n);
		case ("matrix")
			tmp=mat2scm(n);
		case ("complex matrix")
			tmp=mat2scm(n);
		case ("bool matrix")
			tmp=mat2scm(n);
		case ("char matrix")
			tmp=mat2scm(n);
		case ("scalar")
			tmp=num2scm(n);
		case ("complex scalar")
			tmp=num2scm(n);
		case ("bool")
			tmp=num2scm(n);
		case ("struct")
			tmp=struct2scm(n,c+1,"(with \"mode\" \"math\" (big \"triangleup\"))");
		case ("string")
			tmp=str2scm(n);
		case ("list")
			tmp=list2scm(n,c);
	endswitch
endfunction
