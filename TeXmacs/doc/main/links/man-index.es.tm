<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Generar un índice>

  Para la generación de un índice, primero tiene que crear las entradas de
  índice en su documento mediante <apply|menu|Insert|Link|Index entry>.
  Después debes colocar el cursor en el lugar donde desea que aparezca el
  índice de materias y pulsar en <apply|menu|Insert|Automatic|Index>. El
  índice de materias se crea entonces de un modo similar a la tabla de
  contenidos.

  En el menú <apply|menu|Insertar|Enlace|Entrada de índice> se pueden
  encontrar diversos tipos de entradas de índice. Las más sencillas son
  ``principal'', ``sub'' y ``subsub'', que son macros con uno, dos y tres
  argumentos respectivamente. Las entradas de tipo ``sub'' y ``subsub''
  pueden usarse para subordinar unas entradas a otras.

  Una entrada de índice compleja toma cuatro argumentos. El primero es una
  clave que sirve exclusivamente para ubicar la entrada en el índice, y debe
  ser una ``tupla'' (creada usando <key|inactive \<less\>>) cuyo primer
  componente es la categoría principal, el segundo la subcategoría, etc. El
  segundo argumento de una entrada de índice compleja es o bien ``blank'' o
  ``strong''; en este último caso, el número de página de la entrada
  aparecerá en negrita. El tercer argumento se suele dejar vacío, pero si
  crea dos entradas de índice con el mismo tercer argumento (no vacío,
  claro), entonces se creará un rango de números de página. El cuarto
  argumento, que es, de nuevo, una tupla, es la entrada en sí.

  También es posible crear una línea en el índice sin número de página,
  usando ``interject'' en <apply|menu|Insert|Link|Index entry>. El primer
  argumento de este macro es una clave para ordenar esta línea en el índice.
  El segundo argumento contiene el texto que saldrá en el índice. Esta
  construcción puede ser útil para crear diferentes secciones ``A'', ``B'' en
  su índice.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Entrada de
      índice>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Automático>|<with|font family|<quote|ss>|Índice de
      materias>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Entrada de
      índice>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Entrada de
      índice>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
