<TeXmacs|1.0.0.6>

<style|<tuple|generic|macaulay2>>

<\body>
  <\session|macaulay2|default>
    <\output>
      Macaulay 2, version 0.9

      --Copyright 1993-2001, D. R. Grayson and M. E. Stillman

      --Singular-Factory 1.3b, copyright 1993-2001, G.-M. Greuel, et al.

      --Singular-Libfac 0.3.2, copyright 1996-2001, M. Messollen

      \ Macaulay 2 starting up\ 

      \;
    </output>

    <\input|macaulay2] >
      KK = ZZ/31991
    </input>

    <\input|macaulay2] >
      R = KK[x,y,z,w]
    </input>

    <\input|macaulay2] >
      I = ideal(x^2*y,x*y^2+x^3)
    </input>

    <\input|macaulay2] >
      J = gens gb I
    </input>

    <\input|macaulay2] >
      R = KK[a..d]
    </input>

    <\input|macaulay2] >
      I = monomialCurveIdeal(R,{1,3,4})
    </input>

    <\input|macaulay2] >
      codim I
    </input>

    <\input|macaulay2] >
      dim I
    </input>

    <\input|macaulay2] >
      codim (R^1/(I*R^1))
    </input>

    <\input|macaulay2] >
      M = coker gens I
    </input>

    <\input|macaulay2] >
      codim M
    </input>

    <\input|macaulay2] >
      dim M
    </input>

    <\input|macaulay2] >
      degree I
    </input>

    <\input|macaulay2] >
      degree M
    </input>

    <\input|macaulay2] >
      hilbertPolynomial M
    </input>

    <\input|macaulay2] >
      hilbertSeries M
    </input>

    <\input|macaulay2] >
      Mres = res M
    </input>

    <\input|macaulay2] >
      betti Mres
    </input>

    <\input|macaulay2] >
      \;
    </input>
  </session>

  \;
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|5mm>
    <associate|page medium|automatic>
    <associate|shrinking factor|4>
    <associate|page right margin|5mm>
    <associate|page top margin|5mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|5mm>
    <associate|reduction page left margin|25mm>
    <associate|page height|757760unit>
    <associate|page bottom margin|5mm>
    <associate|reduction page top margin|15mm>
    <associate|page width|1015808unit>
    <associate|language|english>
  </collection>
</initial>
