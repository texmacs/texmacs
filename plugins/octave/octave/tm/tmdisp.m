## Released under the GNU General Public License, see www.gnu.org
## Copyright 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Displays a matrix via the TeXmacs interface
## Created: Sept 2002

function tmdisp(M)
	if (length(getenv("TEXMACS_PATH"))>0)
		two=sprintf("%c",2); five=sprintf("%c",5);
		tmp=[two,"scheme:",obj2scm(M),five];
		disp(tmp);
	else
		disp(M);
	endif
endfunction
