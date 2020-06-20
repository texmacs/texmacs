## Copyright (C) 2014 Andreas Weber <andy.weber.aw@gmail.com>
##
## This program is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{s} =} serialize (@var{obj})
## Serialize built-in octave datatype.
##
## Return a human-readable string which can be processed with "eval" to
## retrieve @var{obj}.
##
## @example
## @group
## x = [1,2;3,4];
## serialize(x)
##   @result{} [1 2;3 4]
## @end group
## @end example
##
## @example
## @group
## a.x = [ 3 4 5];
## a.y = @{pi, @{7,8@}@};
## serialize(a)
##   @result{} struct("x",[3 4 5],"y",@{@{3.14159265358979,@{7,8@}@}@})
## @end group
## @end example
##
## See the test section of serialize.m for more examples.
## @end deftypefn

function ret = serialize(obj)
  if (nargin != 1 || nargout > 1)
    print_usage ();
  endif
  if (isnumeric (obj) || ischar (obj) || islogical (obj))

    # GNU Octave <= 5.1.0 have a bug
    # see http://hg.savannah.gnu.org/hgweb/octave/rev/f998e243fa78
    if (islogical (obj) && compare_versions (OCTAVE_VERSION, "5.1.0", "<="))
      p = 1:ndims(obj);
      p(1:2) = p([2 1]);
      obj = reshape (permute (obj, p), size (obj));
    endif

    ret = __serialize_matrix__(obj);
  elseif (iscell (obj))
    ret = __serialize_cell_array__(obj);
  elseif (isstruct (obj))
    ret = __serialize_struct__(obj);
  else
    error('serialize for class "%s", type "%s" isn''t supported', class (obj), typeinfo (obj));
  endif
endfunction

function ret = __serialize_matrix__(m)
  if (ndims (m) == 2)
    if (ischar (m))
      ret = '[';
      for k = 1:rows (m)
        ret = [ret, '"', undo_string_escapes(m(k,:)), '";'];
      endfor
      ret(end) = ']';
    else
      ret = mat2str (m, 20, "class");
    endif
  else
    s = size (m);
    n = ndims (m);
    ret = sprintf ("cat(%i,", n);
    for (k = 1:size (m, n))
      idx.type = "()";
      idx.subs = cell(n,1);
      idx.subs(:) = ":";
      idx.subs(n) = k;
      tmp = subsref (m, idx);
      ret = [ret, __serialize_matrix__(tmp)];
      if (k < s(n))
        ret = [ret, ','];
      else
        ret = [ret, ')'];
      endif
    endfor
  endif
endfunction

function ret = __serialize_cell_array__ (in)
  if(ndims (in) == 2)
    ret = __serialize_2d_cell__ (in);
  else
    s = size (in);
    n = ndims (in);
    ret = sprintf ("cat(%i,", n);
    for (k = 1:size (in, n))
      idx.type = "()";
      idx.subs = cell(n,1);
      idx.subs(:) = ":";
      idx.subs(n) = k;
      tmp = subsref (in, idx);
      ret = [ret, __serialize_2d_cell__(tmp)];
      if (k < s(n))
        ret = [ret, ','];
      else
        ret = [ret, ')'];
      endif
    endfor
  endif
endfunction

function ret = __serialize_2d_cell__(in)
  assert (ndims (in) == 2);
  if (isempty (in))
    ret = '{}';
  else
    ret = '{';
    for (r = 1:rows (in))
      for (c = 1:columns (in))
        tmp = in{r,c};
        if (iscell (tmp))
          ret = [ret __serialize_cell_array__(tmp) ','];
        else
          ret = [ret serialize(tmp) ','];
        endif
      endfor
      ret(end) = ';';
    endfor
    ret(end) = '}';
  endif
endfunction

function ret = __serialize_struct__(in)
  assert (isstruct(in));
  ret = 'struct(';
  for [val, key] = in
    #iscell(val)
    if (iscell(val) && isscalar(in))
      tmp = ['{' serialize(val) '}'];
    else
      tmp = serialize(val);
    endif
    ret = [ ret '"' key '",' tmp ','];
  endfor
  ret = [ ret(1:end-1) ')'];
endfunction

%!function check_it(in)
%!  out = eval (serialize (in));
%!  assert (out, in, 16*eps);
%!endfunction

## [complex] scalars
%!test check_it (int8 (intmax ("int8")))
%!test check_it (int8 (intmin ("int8")))
%!test check_it (uint8 (intmax ("uint8")))
%!test check_it (int16 (intmax ("int16")))
%!test check_it (uint16 (intmax ("uint16")))
%!test check_it (int32 (intmax ("int32")))
%!test check_it (uint32 (intmax ("uint32")))
%!test check_it (int64 (intmax ("int64")))
%!test check_it (int64 (intmin ("int64")))
%!test check_it (uint64 (intmax ("uint64")))
%!test check_it (1.23456)
%!test check_it (pi)
%!test check_it (pi - 2 * pi * j)
%!test check_it (false)
%!test check_it (true)

## Inf, NA, NaN
%!test check_it ([1 2 inf NA NaN])

## 2D matrix
%!test check_it ([1,2;3,4])
%!test check_it ([1,2*pi;3,4.12i])
%!test check_it (logical ([0,1;0,0]))
%!test check_it (logical ([0,1;0,0;1 1]))

## static 3D matrix
%!test
%! a = zeros (2, 3, 2);
%! a(:,:,1) = [1, 3, 5; 2, 4, 6];
%! a(:,:,2) = [7, 9, 11; 8, 10, 12];
%! b = serialize (a);
%! assert(eval(b), a, 16*eps);

## random > 2D matrix
%!test check_it (rand (2, 3, 4));                   # random 3D
%!test check_it (logical (randi(2, 3, 4, 5) - 1));  # random logical 3D
%!test check_it (rand (2, 3, 4, 5));                # random 4D
%!test check_it (randi (3e4, 2, 3, 4, 5, "int16")); # random int16 4D
%!test check_it (rand (2, 3, 4, 5, 2));             # random 5D

## strings
%!test check_it ("huhu");
%!test check_it ("hello world\nsecond line");
%!test check_it ('hello world\nstill first line');
%!test check_it (["hello";"hi"]);
%!test check_it (cat(3, ["hi ";"ho "], ["xyz";"x"]));

## cells
%!test check_it ({})
%!test check_it ({3, "hello", pi, i})
%!test check_it ({3, "octave", pi, i}.')
%!test check_it ({3.1, "world";3, 2+i})
%!test check_it ({{pi, 5}, "world";{5, {}, 2+i}, 6})
%!test
%! tmp = cell(3,2,2);
%! tmp(1,:,:) = {3.1, "world";3, 2+i};
%! check_it (tmp)

## scalar structs
%!test check_it (struct ("foo",1))
%!test check_it (struct ("foo",1,"bar","hello"))
%!test check_it (struct("foo", {3, 4, 5}))

%!test
%! g.y = {"hello", "world"};
%! tmp = serialize(g);
%! assert(eval(tmp), g, 16*eps)

## empty scalar struct
%!test check_it (struct ("foo", {}))
%!test check_it (struct ("foo", []))

## struct array
%!test
%! x(1).a = "string1";
%! x(2).a = "string2";
%! x(1).b = 1;
%! x(2).b = 2;
%! assert (eval(serialize(x)), x, 16*eps)

## 2d struct array
%!test
%! y(1,1).a = "huhu";
%! y(1,2).b = pi;
%! y(2,1).a = "hello";
%! assert (eval(serialize(y)), y, 16*eps)

%!test check_it (struct("foo", {{"huhu", "haha"}}))
%!test check_it (struct("foo", {{3,4,5}}))
%!test check_it (struct("foo", {1, 2}, "bar", {{1,2},{3,4}}))
%!test check_it (struct("foo", {{1,2;10,11},{3;4;5}}))