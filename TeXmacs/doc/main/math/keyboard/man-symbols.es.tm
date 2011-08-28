<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Tecleando símbolos matemáticos>

  Los caracteres griegos son obtenidos en <apply|TeXmacs> por la combinación
  de la tecla modificadora <prefix|M-A-> con una letra. Por ejemplo, <key|H-a>
  produce <with|mode|math|\<alpha\>> y <key|H-G> produce
  <with|mode|math|\<Gamma\>>. <apply|hyper-link|Recuerde|../../start/man-conventions.es.tm>
  que la tecla <prefix|math:greek> es equivalente a <prefix|M-A->, así que
  <with|mode|math|\<rho\>> puede también ser obtenido tecleando
  <with|mode|math|\<rho\>>. Similarmente, <prefix|math:bold>, <prefix|math:cal>, <prefix|math:frak> y
  <prefix|math:bbb> pueden ser usados a fin de teclear caractéres resaltados,
  caligráficos, fraktur y blackboard. Por ejmplo <shortcut|\<frak-m\>> produce
  <with|mode|math|\<frak-m\>>, <key|S-F6 R> produce
  <with|mode|math|\<bbb-R\>> y <shortcut|\<b-cal-Z\>> produce
  <with|mode|math|\<b-cal-Z\>>.

  Los caracteres griegos pueden también ser obtenidos como ``variantes'' de
  caracteres latinos usando la tecla <key|tab>. Por ejemplo, <key|p tab>
  produce <with|mode|math|\<pi\>>. La tecla <key|tab> es tambíen usada para
  obtener variantes de las letras griegas en sí mismas. Por ejemplo, tanto
  <key|H-p tab> como <key|p tab tab> producen <with|mode|math|\<varpi\>>.

  Muchos otros símbolos matemáticos son obtenidos por combinaciones
  ``naturales'' de teclas. Por ejemplo \ <key|- \<gtr\>> produce
  <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>> produce
  <with|mode|math|\<longrightarrow\>> y <key|\<gtr\> => produce
  <with|mode|math|\<geqslant\>>. Similarmente, <key|\| -> produce
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> produce
  <with|mode|math|\<mapsto\>> y <key|- \<gtr\> \<less\> -> produce
  <with|mode|math|\<rightleftarrows\>>. Algunas reglas generales se mantienen
  a fin de obtener variantes de símbolos:

  <\description>
    <expand|item*|<key|tab>>es la tecla principal para obtener variantes. Por
    ejemplo, <key|\<gtr\> => produce <with|mode|math|\<geqslant\>>, pero
    <key|\<gtr\> = tab> yields <format|no line
    break><with|mode|math|\<geq\>>. Similarmente, <key|\<less\> tab> produce
    <with|mode|math|\<prec\>>, <key|\<less\> tab => produce
    <with|mode|math|\<preccurlyeq\>> y <key|\<less\> tab = tab> produce
    <with|mode|math|\<preceq\>>. También, <key|P tab> produce
    <with|mode|math|\<wp\>> y <key|e tab> produce la constante
    <with|mode|math|\<mathe\>=exp(1)>. Usted puede ``realizar el ciclo hacia
    atrés'' using <key|S-tab>.

    <expand|item*|<key|@>>es usada para poner símbolos dentro de
    circunferencias o cuadrados. Por ejemplo, <key|@ +> produce
    <with|mode|math|\<oplus\>> y <key|@ x> yields
    <with|mode|math|\<otimes\>>. Análogamente, <key|@ tab +> produce
    <with|mode|math|\<boxplus\>>.

    <expand|item*|<key|/>>es usado para las negaciones. Por ejemplo, <key|=
    /> produce <with|mode|math|\<neq\>> and <key|\<less\> = /> produce
    <with|mode|math|<neg|\<leqslant\>>>. Note que <key|\<less\> = tab tab />
    produce <with|mode|math|\<nleqq\>>, mientras <key|\<less\> = tab tab /
    tab> produce <with|mode|math|\<lneqq\>>.

    <expand|item*|<key|!>>es usado después de las flechas a fin de forzar los
    superíndice a ser colocados arriba o abajo de la flecha. Por ejemplo,
    <key|- - \<gtr\> ^ x> produce <with|mode|math|\<longrightarrow\><rsup|x>
    >, pero <key|- - \<gtr\> ! ^ x> produce
    <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Varios otros símbolos que no pueden ser ingresados manualmente en la forma
  anterior son obtenidos usando el prefijo <prefix|symbol>. A continuación se
  muestra una pequeña tabla de tales símbolos:

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|1|-1|2|2|cell
  halign|c>|<cwith|1|-1|4|4|cell halign|c>|<cwith|1|-1|2|2|cell
  rborder|1ln>|<table|<row|<cell|Atajo>|<cell|Símbolo>|<cell|Atajo>|<cell|Símbolo>>|<row|<cell|<key|symbol a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<key|symbol n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<key|symbol u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<key|symbol v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<key|symbol w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Algunos
  símboloss que no pueden ser obtenidos usando reglas generales en una forma
  natural.>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas|Álvaro Cantero Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia>

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

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Algunos símboloss que no pueden ser obtenidos usando
      reglas generales en una forma natural.|<pageref|gly-1>>
    </associate>
  </collection>
</auxiliary>
