<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cabeceras estándard>

  El d.t.d. <tmdtd|header> provee etiquetas para personalizar las cabeceras y
  pies de página. La personalización es basada en la idea de que podemos
  especificar un <em|texto de página> para cada página. Este texto de página
  puede por ejemplo ser el título corriente o el nombre de la sección actual.
  El texto de la página puede depender de la paridad de una página y aparecer
  en una forma diferentes para páginas especiales como comienzos de nuevos
  capítulos. Las siguientes etiquetas controlan la disposición física de los
  diferentes tipos de páginas:

  <\description>
    <expand|item*|<markup|start-page>>Esta etiqueta, con el texto de la
    página como su único argumento, especifica la disposición de una primera
    página de un capítulo o sección.

    <expand|item*|<markup|odd-page-text>>Similar a <markup|start-page>, pero
    para la disposición de páginas impares ordinarias.

    <expand|item*|<markup|even-page-text>>Similar a <markup|start-page>, pero
    para la disposición de páginas pares ordinarias.
  </description>

  Las siguientes etiquetas controlan las acciones lógicas relacionadas con la
  cabecera a ser tomadas, cuando se especifica un título, un autor, o cuando
  se empieza una nueva sección.

  <\description>
    <expand|item*|<markup|header-title>>Una etiqueta con el ``argumento
    título'' que es usado en la especificación del título del documento.

    <expand|item*|<markup|header-author>>Una etiqueta con el ``argumento
    autor'' que es usado en la espeficicación del autor del documento.

    <expand|item*|<markup|header-primary>>Una etiqueta con el ``argumento
    nombre de sección'' que es usado en el comienzo de cada nueva sección
    primaria. (<abbr|i.e.> <markup|chapter> para el estilo libro, o
    <markup|section> para el estilo artículo).

    <expand|item*|<markup|header-secondary>> Una etiqueta con el ``argumento
    nombre de sección'' que es usado en el comienzo de cada nueva sección
    secundaria. (<abbr|i.e.> <markup|section> para el estilo libro, o
    <markup|subsection> para el estilo artículo).
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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|header>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|odd-page-text>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|even-page-text>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-title>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-author>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-primary>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-secondary>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsection>>|<pageref|idx-14>>
    </associate>
  </collection>
</auxiliary>
