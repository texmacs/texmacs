## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert a number to a Scheme string
## Created: Sept 2002

function tmp=num2scm(n)
	tmp="(with \"mode\" \"math\" \"";
	if (isreal(n))
		tmp=[tmp, num2str(n), "\")"];
	else
		if (real(n) != 0)
			if (imag(n)>=0)
				op="+";
			else
				op="-";
			endif
			tmp=["(with \"mode\" \"math\" \"", num2str(real(n)),op,num2str(abs(imag(n))),"<cdot><b-i>\")"];
		else
			tmp=["(with \"mode\" \"math\" \"", num2str(imag(n)),"<cdot><b-i>\")"];
		endif
	endif
endfunction
