## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Execute a Scheme expression through TeXmacs
## Created: Sept 2002

function scheme(S)
	if (length(getenv("TEXMACS_PATH"))>0)
		two=sprintf("%c",2); five=sprintf("%c",5);
		tmp=[two,"scheme:",S,five];
		disp(tmp);
	else
		disp("TeXmacs and Scheme not available.");
	endif
endfunction
