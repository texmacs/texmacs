<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <samp|><expand|tmdoc-title|Maxima>

  <name|Maxima> no es sólo uno de los más viejos y mejores sistemas de
  álgebra computacional de los alrededores, es también uno de los únicos
  sistemas de propósito general para los cuales hay una implementación libre.
  Puede obtenerlo de\ 

  <\verbatim>
    \ \ \ \ http://www.ma.utexas.edu/users/wfs/maxima.html
  </verbatim>

  La versión soportada es <name|Maxima> 5.6 basada en GCL. Para <name|Maxima>
  5.6 basada en <name|CLisp>, edite su archivo <verbatim|tm_maxima> y
  reemplace <verbatim|-load> por <verbatim|-i>. Para <name|Maxima> 5.9-pre,
  reemplace -load por <verbatim|-p>. Problemas conocidos:

  <\itemize>
    <item>Si presiona <key|enter> cuando una sentencia no está completa
    (típicamente, terminada por <verbatim|;> o <verbatim|$>), la interface se
    colgará.

    <item>Si uste causa que aparezce el <em|prompt> de interrupción de Lisp,
    la interface se colgará.

    <item>El comando <verbatim|info> no está soportado (está denifnido en el
    Lisp subyacente, y es difícil para soportar la portabilidad).

    <item>Algunos comandos en el trabajo del depurador, pero algunos
    (incluyendo <verbatim|:c>) no funcionan, nadie sabe por qué.

    <item>El comando <verbatim|load> algunas veces se comporta
    misteriosamente,
  </itemize>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
