## Released under the GNU General Public License, see www.gnu.org
## Copyright 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Displays a matrix via the TeXmacs interface
## Created: Sept 2002

function tmdisp(M)
	if (length(getenv("TEXMACS_PATH"))>0)
		two=sprintf("%c",2); five=sprintf("%c",5);
		tmp1=obj2scm(M);
		if strcmp(tmp1,"")
			disp(M);
		else
			tmp=[two,"scheme:",tmp1,five];
			disp(tmp);
		endif
	else
		disp(M);
	endif
endfunction
