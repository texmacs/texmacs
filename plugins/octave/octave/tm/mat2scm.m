## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert a matrix to a Scheme expression
## Created: Sept 2002

function tmp=mat2scm(M)
	global TMCOLORS;
	global TMCOLIDX;
	tmp="(with \"mode\" \"math\" \"formula style\" \"true\" (matrix (tformat (table";
	[r,c]=size(M);
	for i=1:r
		tmp=[tmp,"(row "];
		for j=1:c
			tmp1=num2scm(M(i,j));
			tmp=[tmp,"(cell ", tmp1, ") "];
		endfor
		tmp=[tmp,") "];
	endfor
	tmp=[tmp,"))))"];
endfunction
