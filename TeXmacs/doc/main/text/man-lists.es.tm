<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Listas>

  Usando <apply|menu|Insert|Itemize> usted puede empezar una lista no numerada.
  Puede también seleccionar una marca particular como
  <with|mode|math|\<bullet\>> (bolos), <with|mode|math|<op|->> (guiones) o
  <with|mode|math|<op|\<rightarrow\>>> (flechas) para indicar las entradas en
  la lista o la marca por defecto. Las listas pueden ser <em|anidadas> dentro
  de otras marcas como en las siguiente lista:

  <\itemize>
    <item>Primer item.

    <item>Ahora viena la sublista:

    <\itemize>
      <item>Un subitem.

      <item>Otro.
    </itemize>

    <item>Un item final.
  </itemize>

  La marca por defecto es mostrada en una forma diferente dependiendo del
  nivel de anidamiento. En el nivel más externo, usamos la marca
  <with|mode|math|\<bullet\>>, en el segundo nivel <with|mode|math|\<circ\>>
  y así. Cuando está dentro de una lista, note que presionando <key|return>
  automáticamente inicia un nuevo item. Si usted necesita items que son de
  varios párrafos de largo, entonces usted puede usar siempre <key|S-return>
  a fin de iniciar un nuevo párrafo.

  Los entornos enumerados, que son iniciados usando
  <apply|menu|Insert|Enumerate>, se comporta en una forma similar como una
  lista no numerada, excepto que los items son numerados. A continuación
  sigue un ejemplo de una enumeración que puede ser iniciada usando
  <apply|menu|Insert|Enumerate|Roman>:

  <\expand|enumerate-Roman>
    <item>Un primer item.

    <item>Un segundo

    <item>Y un último
  </expand>

  El último tipo de listas son las listas descriptivas. Ellas son iniciadas
  usando, <apply|menu|Insert|Description> y permite describir una lista de
  conceptos:

  <\description>
    <expand|item*|Gnu.>Una peluda pero gentil bestia.

    <expand|item*|Gnat.>Sólo vive en un zoológico
  </description>

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
    <associate|language|spanish>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-5|<tuple|III.|?>>
    <associate|idx-6|<tuple|III.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|III.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Bolos>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Lista numerada>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Lista numerada>|<with|font
      family|<quote|ss>|Roman>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Descripción>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
