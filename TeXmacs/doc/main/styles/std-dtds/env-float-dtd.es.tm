<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Entornos para objetos flotantes>

  El <abbr|d.t.d.> <tmdtd|env-float> provee etiquetas para los objetos
  flotantes. Las siguientes etiqueta es la única de altno nivel:\ 

  <\explain|<markup|footnote>>
    Hace una nota de pie de página
  </explain>

  Las siguientes etiquetas de bajo nivel pueden ser usadas para definiciones
  de alto nivel figura y entornos tipo tabla <markup|big-figure>,
  <markup|small-figure>, <markup|big-table> y <markup|small-table>:

  <\explain|<markup|render-small-figure>>
    Un macro para mostrar una pequeña figura. Los argumentos son nombres
    cortos (como ``figura'' o ``tabla'') para la lista de figuras, su nombre
    real (como ``Figura 2.3'' o ``Tabla 5''), la figura en sí misma y un
    texto explicativo que acompaña a dicha figura o tabla.
  </explain>

  <\explain|<markup|render-big-figure>>
    Una variante de <markup|render-small-figure> para mostrar una gran
    figura.
  </explain>

  Las siguientes etiquetas pueden ser usadas para personalizar la
  apareciencia del texto alrededor de las figuras, tablas y notas de pie de
  página:

  <\explain|<markup|figure-name>>
    Un macro que controla la apariencia del texto ``Figura''. Por defecto,
    usamos fuente resaltada.
  </explain>

  <\explain|<markup|figure-sep>>
    El separador entre la figura y su número y texto explicativo. Por defecto
    es un punto seguido por un espacio.
  </explain>

  <\explain|<markup|footnote-sep>>
    El separador entre el número del pie de página y el texto. Por defecto es
    un punto seguido por un espacio.
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