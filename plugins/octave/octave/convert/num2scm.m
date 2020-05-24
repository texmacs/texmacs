## Released under the GNU General Public License, see www.gnu.org
## Copyright (C) 2002 by the Free Software Foundation
## Written by Michael Graffam mikegraffam@yahoo.com
##
## Convert a number to a Scheme string
## Created: Sept 2002

function ret= num2scm (n)
  ret= "(with \"mode\" \"math\" \"";
  if (isreal (n))
    ret= [ret, num2str(n), "\")"];
  else
    if (real(n) != 0)
      if (imag(n)>=0)
        op= "+";
      else
        op= "-";
      endif
      ret= ["(with \"mode\" \"math\" \"", num2str(real(n)),op,num2str(abs(imag(n))),"<cdot><b-i>\")"];
    else
      ret= ["(with \"mode\" \"math\" \"", num2str(imag(n)),"<cdot><b-i>\")"];
    endif
  endif
endfunction
