<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Archivos de estilo <TeXmacs>>

  Uno de las fortalezas fundamentales de <TeXmacs> es la posiblidad de
  escribir sus propios archivos y paquetes de estilo. El propósito de los
  archivos de estilo es múltiple:

  <\itemize>
    <item>Permiten la abstracción de los elementos repetitivos en los textos,
    como secciones, teoremas, enumeraciones, etc.

    <item>Forman un mecanismo que permite estruturar su texto. Por ejemplo,
    puede indicar que una porción dada de su texto es una abreviación, una
    cita o ``importante''.

    <item>Estilos de documento estándar le capacitan para escribir documentos
    de apariencia profesional, porque los archivos de estilo correspondientes
    han sido escrito con mucho cuidad por gente que sabe mucho acerca de
    tipografía y estética.
  </itemize>

  Para un documento es posible asociar uno o varios estilos de documentos, lo
  cuales son estandar o definidos por el usuario. El principal estilo de un
  documento es seleccionado en el menú <apply|menu|Document|Style>. Los
  estilos extra pueden ser adicionados usando <apply|menu|Document|Use
  package>.

  Desde el punto de vista del editor, cada estilo corresponde a un archivo
  <verbatim|.ts>. Los archivos correspondientes para cada estilo son
  procesados como si fueran documentos usuales, pero al final, el editor sólo
  mantiene el entorno final como el entorno inicila para el documento
  principal. Más precisamente, los archivos de estilo son procesados en
  orden, al igual que sus propios estilos, de una manera recursiva.

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
    <associate|language|spanish>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Usar paquete>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
