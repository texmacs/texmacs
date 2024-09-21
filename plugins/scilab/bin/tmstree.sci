//*****************************************************************************
// MODULE     : tmstree.sci
// DESCRIPTION: Provide Scilab routines to render any Scilab variable as a
//              TeXmacs Scheme tree.
// COPYRIGHT  : (C) 2013 François Poulain, Joris van der Hoeven
//*****************************************************************************
// This software falls under the GNU general public license version 3 or later.
// It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
// in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
//*****************************************************************************

global ('tmstrees_defined_funcs')

function str= tmstree(a)
  // From any Scilab datatype.
  // Provide a representation in TeXmacs stree format
  //
  // Parameters
  // a: is a Scilab variable
  //
  // Description
  // Taking a variable, the tmstree function will provide a formatted
  // representation of it as a TeXmacs Scheme tree.
  // The following types are handled by this function:
  //  <itemizedlist>
  //    <listitem><para>Real / Complex matrices</para></listitem>
  //    <listitem><para>Boolean</para></listitem>
  //    <listitem><para>Integer</para></listitem>
  //    <listitem><para>String</para></listitem>
  //    <listitem><para>Polynomial types</para></listitem>
  //    <listitem><para>Rationnal</para></listitem>
  //    <listitem><para>List / Tlist / Mlist</para></listitem>
  //    <listitem><para>Struct</para></listitem>
  //    <listitem><para>Cell</para></listitem>
  //    <listitem><para>Hypermatrix</para></listitem>
  //    <listitem><para>Linear-invariant state space system</para></listitem>
  //  </itemizedlist>
  //
  // Examples
  // tmstree (rand(3,3)) // Return the stree representation of a 3,3 matrix
  //
  // s=poly(0,'s'); G=[1,s;1+s^2,3*s^3];
  // tmstree(G*s-1); // Show a polynomial through a stree representation
  //
  // See also
  // prettyprint
  //
  // Authors
  // François Poulain

  if argn (2) <> 1 then
    error (msprintf (gettext ("%s: Wrong number of input argument(s): " + ..
                              "%s expected."), "tmstree", "1"));
  end

  global ('tmstrees_defined_funcs')

  // // default length/size implementation is dreadful
  function i= len (x);
    i= prod (size (x));
  endfunction

  // append vectors
  function r= append (x, y)
    r= [x y];
  endfunction

  function z= map (f, x);
    z= [];
    for i= 1:len (x) do
      z(i)= f(x(i));
    end
  endfunction

  // quote strings
  function s= quote (x)
    s= "'"" + x + "'"";
  endfunction

  // construct stree node
  function s= make (l, x)
    scm= "scheme: ";
    if argn (2) == 1 | x == [] then
      s= scm + "(" + l + ")";
      return
    end
    s= makeStreeNode (l, x);
  endfunction

  // make concats
  function s= concat (x, sep)
    if x == [] then
      s= "";
      return;
    end
    if argn (2) == 2 then
      z= [];
      for i= 1:(len (x) - 1) do
        z= append (z, [x(i) sep]);
      end
      z= append (z, x(i+1));
      x= z;
    end
    if type (x) <> 10 then
      error (msprintf (gettext ("%s: Wrong type of input argument(s): " + ..
                                "%s expected."), "concat", "string"));
      s= "";
      return;
    end
    scm= "scheme: ";
    z= [];
    tmp= "";
    for i= 1:len (x) do
      is_scheme= strindex (x(i), scm);
      if is_scheme (1) == 1 then
        if tmp <> "" then
          z= append (z, tmp);
          tmp= "";
        end
        z= append (z, x(i));
      else
        tmp= tmp + x(i);
      end
    end
    if tmp <> "" then
      z= append (z, tmp);
    end
    if z == [] then
      s= "";
    elseif (len (z) == 1) then
      s= z;
    else
      s= make ("concat", z);
    end
  endfunction

  // make tables
  function str= table (a)
    [l, c]= size(a);
    rows= [];
    for i= 1:l do
      cells= [];
      for j= 1:c do
        cells= append (cells, make ("cell", a(i,j)));
      end
      rows= append (rows, make ("row", cells));
    end
    if rows == [] then
      rows= make ("row", "");
    end
    str= make ("table", rows);
  endfunction

  // handle matrixes
  function str= matrix2tmstree (a)
    if len (a) == 0 then
      str= make ("around", ["(" "" ")"]);
    return;
    end
    [l, c]= size (a);
    mat= emptystr (l, c);
    for i= 1:l do
      for j= 1:c do
        mat(i,j)= tmstree (a(i,j));
      end
    end
    str= make ("matrix", make ("tformat", table (mat)));
  endfunction

  // handle real
  function str= real2tmstree (a)
    if     isinf (a) then str= "<infty>";
    elseif isnan (a) then str= "NaN";
    else   str= string (a);
    end
    if strindex (str, 'D') <> [] then
      n= strsplit (str, 'D');
      m= tmstree (strtod (n(1)));
      e= make ("rsup", tmstree (strtod (n(2))));
      str= concat ([m "<times>10" e]);
    end
  endfunction

  // handle complex
  function str= complex2tmstree (a)
    re= real (a);
    im= imag (a);

    if re == 0 then
      re= "";
    else
      re= tmstree (re);
    end

    if im == 0 then
      sig= "";
    elseif im > 0 then
      sig= "+";
    else
      sig= "-";
    end

    if im == 0 then
      im= "";
    elseif im == 1 | im == -1 then
      im= "i";
    else
      im= tmstree (abs(im))
      im= concat ([im "*i"]);
    end

    if re == "" & sig == "+" then
      sig= "";
    end

    str= concat ([re sig im]);
  endfunction

  // handle polynoms
  function str= poly2tmstree (a)
    z= [];
    x= varn(a);
    c= coeff(a);
    for i= 1:len (c) do
      if c(i) <> 0 then
        expo= i-1;
        if len (z) <> 0 & real (c(i)) > 0 then
          z= append (z, "+");
        end
        if c(i) <> 1 | expo == 0 then
          if c(i) == -1 & expo <> 0 then
            z= append (z, "-");
          else
            is_pure= (norm (imag (c(i))) > %eps * norm (real (c(i)))) <> ..
                     (norm (real (c(i))) > %eps * norm (imag (c(i))));
            coef= tmstree (c(i));
            if is_pure then
              if imag (c(i)) > 0 then
                z= append (z, "+");
              end
              z= append (z, coef);
            else
              if len (z) <> 0 & (real (c(i)) < 0 | imag (c(i)) < 0) then
                z= append (z, "-");
                if expo <> 0 then
                  z= append (z, make ("around*", ["(" (tmstree (-c(i))) ")"]));
                else
                  z= append (z, coef);
                end
              else
                if expo <> 0 then
                  z= append (z, make ("around*", ["(" coef ")"]));
                else
                  z= append (z, coef);
                end
              end
            end
          end
        end
        if expo <> 0 then
          if c(i) <> 1 & c(i) <> -1 then
            z= append (z, '*');
          end
          z= append (z, x);
        end
        if expo > 1 then
          expo= make ("rsup", tmstree (expo));
          z= append (z, expo);
        end
      end
    end
    str= concat (z);
    if str == "" then
      str= "0";
    end
  endfunction

  // handle rational
  function str= rational2tmstree (a)
    [k,l]= size (a.num);
    if k*l > 1 then
      mat= emptystr (a.num);
      for i= 1:k do
        for j= 1:l do
          mat(i,j)= tmstree (a.num(i,j) / a.den(i,j));
        end
      end
      str= make ("matrix", make ("tformat", table (mat)));
      return;
    end
    num= tmstree (a.num);
    den= tmstree (a.den);
    if num == "0" then
      str= "0";
    elseif den == "1" then
      str= num;
    elseif num == den then
      str= "1";
    else
      str= make ("frac", [num den]);
    end
  endfunction

  // handle boolean
  function str= boolean2tmstree (a)
    if a then str= "T";
    else str= "F";
    end
  endfunction

  // handle sparse matrix
  function str= sparse2tmstree (a, prefix)
    if argn (2) == 1 then
      prefix= "sparse ";
    end
    var= make ("varname");
    [coo, val, siz]= spget(a);
    mat= emptystr (len (val), 3);
    val= map (tmstree, val);
    siz= map (tmstree, siz);
    siz= concat (siz, "<times>");
    [l, c]= size (coo);
    for i= 1:l do
      ind= map (tmstree, coo(i,:));
      ind= concat (ind, ",")
      ind= make ("rsub", ind);
      mat(i,1)= concat ([var ind]);
      mat(i,2)= "=";
      mat(i,3)= val(i);
    end
    mat= make ("tabular", make ("tformat", table (mat)));
    t1= make ("text", prefix);
    t2= make ("text", " matrix: ");
    str= concat ([t1 siz t2])
    str= make ("document", [str mat]);
  endfunction

  // handle Matlab sparse matrix
  function str= mtlb_sparse2tmstree (a)
    str= sparse2tmstree (a, "Matlab sparse ");
  endfunction

  // handle unsigned int
  function str= uint2tmstree (a)
    str= string (a);
  endfunction

  // handle string
  function str= string2tmstree (a)
    str= string (a);
    num= ascii (str);
    // Hack
    str= strsubst (str, "<", "<less@")
    str= strsubst (str, ">", "<gtr>")
    str= strsubst (str, "<less@", "<less>")
    str= strsubst (str, "\", "\\\\\\\\");
    str= strsubst (str, '""', "''''");
    if max (num) > 126 | min (num) < 32 then
      str= make ("extern", ["scilab-verbatim-<gtr>tree", str]);
    end
    str= make ("text", str);
  endfunction

  // handle list
  function str= list2tmstree (a, ind)
    if argn (2) == 1 then
      ind= emptystr (1, length (a));
    end
    lst= emptystr (length (a), 2);
    for i= 1:length (a) do
      tmp1= make ("varname");
      if ind (i) <> "" then
        tmp2= make ("rsub", tmstree (ind (i)));
      else
        tmp2= make ("rsub", tmstree (i));
      end
      lst(i,1)= concat ([tmp1 tmp2]);
      lst(i,2)= tmstree (getfield (i, a));
    end
    str= make ("block", make ("tformat", table (lst)));
  endfunction

  // handle structs
  function str= st2tmstree (a, with_title);
  if argn (2) == 1 then
    with_title= %t;
  end
  if len (a) <> 1 then
    [k, l]= size (a);
    mat= emptystr (k, l);
    for i= 1:k do
      for j= 1:l do
        mat(i,j)= st2tmstree (a(i,j), %f);
      end
    end
    str= make ("matrix", make ("tformat", table (mat)));
    [k,l]= size (a);
    tit= make ("text", " struct matrix:");
    tit= concat ([tmstree(k) "<times>" tmstree(l) tit]);
    str= make ("document", [tit str]);
  return;
  end
    st= fieldnames (a);
    st(:,2)= st;
    for i=1:len (st(:,1)) do
      st(i,1)= tmstree (st(i,1));
      st(i,1)= concat ([st(i,1) ":"]);
      st(i,2)= tmstree (a(st(i,2)));
    end
    str= make ("block", make ("tformat", table (st)));
    if with_title then
      tit= make ("text", "struct with fields:");
      str= make ("document", [tit str]);
    end
  endfunction

  // handle hypermatrices (inspired from %hm_p from Scilab)
  function str= hypermat2tmstree (a);
    dims= double (a.dims);
    nd= size (dims, '*');
    if nd < 3 then
      if nd == 0 then
        str= tmstree ([]);
      elseif nd == 1 then
        str= tmstree (a.entries);
      elseif nd == 2 then
        str= tmstree (matrix (a.entries), dims(1), dims(2));
      end
      return
    end
    indexes= (1:dims (3));
    for k= 4:nd do
      indexes= [(ones (1, dims(k)) .*. indexes); ..
                ((1:dims(k)) .*. ones (1, size(indexes,2)))];
    end
    i= 1;
    k= 1;
    sz= dims(1)*dims(2);
    hm= emptystr (len (a.entries)/sz, 3);
    for ind= indexes do
      tmp1= make ("varname");
      tmp2= ":,:," + strcat (string (ind'), ",");
      tmp2= make ("rsub", tmp2);
      hm(i, 1)= concat ([tmp1 tmp2]);
      hm(i, 2)= "=";
      hm(i, 3)= tmstree (matrix ((a.entries(k:k-1+sz)), dims(1), dims(2)));
      i= i + 1;
      k= k + sz;
    end
    str= make ("tabular", make ("tformat", table (hm)));
  endfunction

  // handle cells
  function str= ce2tmstree (a);
    tit= make ("text", "cell array:");
    if (len (a) == 0) then
      str= tmstree ([]);
      str= make ("document", [tit str]);
      return;
    end
    ent= emptystr (len (a.entries), 1);
    for i= 1:len (ent) do
      if a.entries(i) == [] then
        ent(i)= "{}";
      else
        ent(i)= tmstree (a.entries(i));
      end
    end

    dims= double (a.dims);
    nd= size (dims, '*');
    if nd < 3 then
      if nd == 0 then
        str= tmstree ([]);
      elseif nd == 1 then
        str= tmstree (ent);
      elseif nd == 2 then
        str= table (matrix (ent, dims(1), dims(2)));
        str= make ("matrix", make ("tformat", str));
      end
      return
    end
    indexes= (1:dims (3));
    for k= 4:nd do
      indexes= [(ones (1, dims(k)) .*. indexes); ..
                ((1:dims(k)) .*. ones (1, size(indexes,2)))];
    end
    i= 1;
    k= 1;
    sz= dims(1)*dims(2);
    hm= emptystr (len (ent)/sz, 3);
    for ind= indexes do
      tmp1= make ("varname");
      tmp2= ":,:," + strcat (string (ind'), ",");
      tmp2= make ("rsub", tmp2);
      hm(i, 1)= concat ([tmp1 tmp2]);
      hm(i, 2)= "=";
      tmp3= table (matrix ((ent(k:k-1+sz)), dims(1), dims(2)));
      hm(i, 3)= make ("matrix", make ("tformat", tmp3));
      i= i + 1;
      k= k + sz;
    end
    str= make ("block", make ("tformat", table (hm)));
    str= make ("document", [tit str]);
   endfunction
  
  // handle linear state space systems
  function str= statespace2tmstree (a)
    t= make ("around*", ["(" "t" ")"]);
    X= concat (["X " t]);
    Y= concat (["Y " t]);
    U= concat (["U " t]);
    A= tmstree (a.A);
    C= tmstree (a.C);
    if a.B == 0 then
      state= concat ([A "*" X]);
    else
      B= tmstree (a.B);
      state= concat ([A "*" X "+" B "*" U]);
    end
    if a.D == 0 then
      output= concat ([C "*" X]);
    else
      D= tmstree (a.D);
      output= concat ([C "*" X "+" D "*" U]);
    end
    select a.dt
      case 'c' then
        dX= make ("wide", ["X" "<dot>"]);
        dX= concat ([dX " " t]);
      case 'd' then
        dX= make ("around*", ["(" "t+dt" ")"]);
        dX= concat (["X " dX]);
      else
        dX= tmstree (a.dt);
        dX= concat (["t+" dX])
        dX= make ("around*", ["(" dX ")"]);
        dX= concat (["X " dX]);
    end
    state= [dX "=" state];
    output= [Y "=" output];
    tab= table ([state ; output])
    form= make ("cwith", ["1" "-1" "1" "1" "cell-halign" "r"]);
    str= make ("choice", make ("tformat", [form tab]));
  endfunction

  // handle typed list
  function str= tlist2tmstree (a)
    ind= emptystr (definedfields(a));
    fn= fieldnames (a);
    for i= 1:len (fn) do
      ind (i+1)= fn (i);
    end
    str= list2tmstree (a, ind);
  endfunction

  // handle verbatim content
  function str= verbatim2tmstree (a)
    try
      str= make ("text", make ("verbatim", string (a)));
    catch
      str= tmstree (lasterror ());
    end
  endfunction

  // main output routine
  if (type (a) < 5 | type (a) == 8 | type (a) == 10) & len (a) <> 1 then
    str= matrix2tmstree (a);
  else
    typ= type(a);
    // tlists and mlists are handled by the same way
    if typ == 17 then
      typ= 16;
    end
    select typ
    case 1 then
      if norm (imag (a)) > %eps * norm (real (a)) then
        str= complex2tmstree (a);
      else
        str= real2tmstree (real (a));
      end
    case 2 then
      str= poly2tmstree (a);
    case 4 then
      str= boolean2tmstree (a);
    case 5 then
      str= sparse2tmstree (a);
    case 6 then
      str= sparse2tmstree (a);
    case 7 then
      str= mtlb_sparse2tmstree (a);
    case 8 then
      str= uint2tmstree (a);
    case 10 then
      str= string2tmstree (a);
    case 15 then
      str= list2tmstree (a);
      tit= make ("text", "list with entries:");
      str= make ("document", [tit str]);
    case 16 then
      if type (a) == 16 then
        mt= "t";
      else
        mt= "m"
      end
      tit= make ("text", mt + "list of type " + typeof (a) + " with fields:");
      mtyp= strsubst (typeof (a), '-', '');
      mfunc= mtyp + '2tmstree';
      if exists (mfunc) then
        execstr ('is_func= type(' + mfunc  + ') == 13');
        if is_func then
          execstr ('str= ' + mfunc + '(a)');
          return;
        end
        str= tlist2tmstree (a);
        str= make ("document", [tit str]);
        return
      else
        if find (tmstrees_defined_funcs == mfunc) == [] then
          disp ("NOTE: you can customize the display of ''''" + mtyp + ..
                "'''' type by defining a function str=" + mfunc + "(arg)");
          tmstrees_defined_funcs= [tmstrees_defined_funcs mfunc];
        end
      end
      str= tlist2tmstree (a);
      str= make ("document", [tit str]);
    else
      str= verbatim2tmstree (a);
    end
  end
endfunction

// Public function reused by tmout ()
function s= makeStreeNode (l, x)
  scm= "scheme: ";
  if type (l) <> 10 then
    error (msprintf (gettext ("%s: Wrong type of input argument(s): " + ..
                              "%s expected."), "make", "string"));
  end
  if argn (2) == 1 | x == [] then
    s= scm + "(" + l + ")";
  return
  end
  if type (l) <> 10 | type (x) <> 10 then
    error (msprintf (gettext ("%s: Wrong type of input argument(s): " + ..
                              "%s expected."), "make", "string"));
  end
  for i= 1:size (x,'*') do
    is_scheme= strindex (x(i), scm);
    if is_scheme (1) == 1 then
      x(i)= part (x(i), length (scm):length (x(i)));
    else
      x(i)= "'"" + x(i) + "'"";
    end
  end
  s= scm + "(" + l + " " + strcat (x, " ") + ")";
endfunction
