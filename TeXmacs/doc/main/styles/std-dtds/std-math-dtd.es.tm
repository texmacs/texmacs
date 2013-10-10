<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Etiquetamiento matemático estándar>

  El etiquetamiento matemático estándar está definido en <tmdtd|std-math>.

  <\explain|<markup|binom>>
    Para coeficientes binomiales <math|<binom|n|m>>.
  </explain>

  <\explain|<markup|choose>>
    Nombres alternative para <markup|binom> (en desuso).
  </explain>

  <\explain|<markup|shrink-inline>>
    A macro which switches to scriptsize text when you are not in display
    style. This macro is mainly used by developers. For instance, the
    <markup|binom> macro uses on it.
  </explain>

  Los siguients son entornos matemáticos tabulares estándar:

  <\explain|<markup|matrix>>
    Para matrices <math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|det>>
    Para determinantes <math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|choice>>
    Para listas de selección <math|<around|\||x|\|>=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|<text|if
    >x\<leqslant\>0>>|<row|<cell|x,>|<cell|<text|if >x\<geqslant\>0>>>>>>
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|El permiso está garantizado para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|spanish>
  </collection>
</initial>