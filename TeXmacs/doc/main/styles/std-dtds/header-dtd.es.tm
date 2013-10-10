<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Cabeceras estándard>

  El d.t.d. <tmdtd|header> provee etiquetas para personalizar las cabeceras y
  pies de página. La personalización es basada en la idea de que podemos
  especificar un <em|texto de página> para cada página. Este texto de página
  puede por ejemplo ser el título corriente o el nombre de la sección actual.
  El texto de la página puede depender de la paridad de una página y aparecer
  en una forma diferentes para páginas especiales como comienzos de nuevos
  capítulos. Las siguientes etiquetas controlan la disposición física de los
  diferentes tipos de páginas:

  <\explain|<markup|start-page>>
    Esta etiqueta, con el texto de la página como su único argumento,
    especifica la disposición de una primera página de un capítulo o sección.
  </explain>

  <\explain|<markup|odd-page-text>>
    Similar a <markup|start-page>, pero para la disposición de páginas
    impares ordinarias.
  </explain>

  <\explain|<markup|even-page-text>>
    Similar a <markup|start-page>, pero para la disposición de páginas pares
    ordinarias.
  </explain>

  Las siguientes etiquetas controlan las acciones lógicas relacionadas con la
  cabecera a ser tomadas, cuando se especifica un título, un autor, o cuando
  se empieza una nueva sección.

  <\explain|<markup|header-title>>
    Una etiqueta con el ``argumento título'' que es usado en la
    especificación del título del documento.
  </explain>

  <\explain|<markup|header-author>>
    Una etiqueta con el ``argumento autor'' que es usado en la espeficicación
    del autor del documento.
  </explain>

  <\explain|<markup|header-primary>>
    Una etiqueta con el ``argumento nombre de sección'' que es usado en el
    comienzo de cada nueva sección primaria. (<abbr|i.e.> <markup|chapter>
    para el estilo libro, o <markup|section> para el estilo artículo).
  </explain>

  <\explain|<markup|header-secondary>>
    \ Una etiqueta con el ``argumento nombre de sección'' que es usado en el
    comienzo de cada nueva sección secundaria. (<abbr|i.e.> <markup|section>
    para el estilo libro, o <markup|subsection> para el estilo artículo).
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
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