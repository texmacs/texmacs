
###############################################################################
##
## MODULE      : with_mode_math.m
## DESCRIPTION : with mode math
## COPYRIGHT   : (C) 2002  Michael Graffam mikegraffam@yahoo.com
##                   2020  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.


function ret= num2scm (n)
  switch (typeinfo (n))
    case ("complex scalar")
      real_str= num2str (real (n));
      imag_str= num2str (abs (imag (n)));

      if (imag (n) >= 0)
        op= "+";
      else
        op= "-";
      endif

      if (real (n) != 0)
        ret= with_mode_math ([real_str, op, imag_str, "<cdot><b-i>"]);
      elseif (imag (n) < 0)
        ret= with_mode_math ([op, imag_str, "<cdot><b-i>"]);
      else
        ret= with_mode_math ([imag_str, "<cdot><b-i>"]);
      endif
    case ("int32 scalar")
      ret= with_mode_math (int2str (n));
    case ("int64 scalar")
      ret= with_mode_math (int2str (n));
    case ("scalar")
      if (isfloat (n) && n == 0.0)
        ret= with_mode_math (dquote ("0.0"));
      else
        tmp= disp (n);
        ret= with_mode_math (dquote (strtrim (tmp)));
      endif
    case ("bool")
      if (n)
        ret= with_mode_math ("true");
      else
        ret= with_mode_math ("false");
      endif
    otherwise
      ret= "";
  endswitch
endfunction
