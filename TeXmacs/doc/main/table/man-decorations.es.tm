<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Bordes, acolchado y color de fondo>

  Puede especificar el ancho de los bordes y de los espacios de
  acolchado<\footnote>
    Los espacios de acolchado son aquellos que se encuentran entre el borde
    de la celda y el texto propiamente dicho en el interior de la misma.
  </footnote> de una celda en las cuatro posibles direcciones: a la
  izquierda, derecha, arriba y abajo (ver <apply|menu|Table|Cell border> ).
  Usted tiene los atajos de teclado \ <key|table b><render-key|<with|mode|math|x>>
  y <key|table p><render-key|<with|mode|math|x>> para especificar los anchos de
  borde y de acolchado de la celda.

  <tabular|<tformat|<table|<row|<cell|>>>>>El ancho del borde por defecto
  para las celdas en el entorno de bloque es de <verbatim|1ln>, por ejemplo,
  el ancho de linea estandar en el tipo de letra actual (similar al ancho de
  una linea de fracción). Este ancho se añade a la derecha y debajo de cada
  celda (exceptuando las celdas que están en la primera fila o columna). El
  acolchado horizontal por defecto de cada celda es <verbatim|1spc>: la
  anchura de un espacio en blanco en el actual tipo de de letra. El acolchado
  vertical por defecto de la celda es <verbatim|1sep>: la mínima separación
  estandar entre dos cajas pegadas.

  Las celdas se les puede dar un color de fondo vía <apply|menu|Table|Cell
  background color>.

  A la tabla entera se le puede dar un borde y un acolchado en
  <apply|menu|Table|Special table properties|Border>. En este caso, el
  acolchado se realiza en el exterior del borde.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Álvaro Cantero
  Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia|Offray Vladimir Luna
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
    <associate|language|spanish>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|1|?>>
    <associate|footnote-1|<tuple|1|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Borde de la celda>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Color de fondo de la celda>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Propiedades especiales de tabla>|<with|font
      family|<quote|ss>|Borde>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
