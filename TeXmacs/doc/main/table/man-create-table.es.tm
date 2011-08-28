<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creando tablas>

  A fin de crear una tabla usted puede usar o bien <apply|menu|Insert|Table>
  o uno de los siguientes atajos de teclado:

  <\description>
    <expand|item*|<key|table N t>>Crea una tabla normal.

    <expand|item*|<key|table N T>>Crea una tabla cuyas celdas están
    centradas.

    <expand|item*|<key|table N b>>Crea un ``bloque'' normal cuyas
    celdas entán separadas por líneas.

    <expand|item*|<key|table N B>>Crea un bloque cuyas celdas están
    centradas.
  </description>

  En modo matemático algunas estructuras tipo tabla son proveidas:

  <\description>
    <expand|item*|<key|table N m>>Crea una matriz.

    <expand|item*|<key|table N d>>Crea un determinante.

    <expand|item*|<key|table N c>>Crea una lista de elecciones.
  </description>

  El entorno <verbatim|\\eqnarray*> es también una clase especial de
  estructura tipo tabla, que se extiende una línea entere. Puede crear una
  lista de ecuaciones usando <apply|menu|Insert|Mathematics|Equations>.

  Cuando creas una nueva tabla, su tamaño es mínimo (usualmente
  <with|mode|math|1\<times\>1>) y sus celdas están vacías. Puedes insertar
  nuevas filas y columnas usando los atajos de teclado <shortcut|(structured-insert-left)>,
  <shortcut|(structured-insert-right)>, <key|A-arriba> y <key|A-abajo>. Por ejemplo,
  <shortcut|(structured-insert-right)> crea una nueva columna a la derecha de la columna que ocupa
  actualmente el cursor. También puede crear una nueva fila debajo de la fila
  actual del cursor pulsando <key|enter>.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Tabla>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Matemáticas>|<with|font
      family|<quote|ss>|Ecuaciones>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
