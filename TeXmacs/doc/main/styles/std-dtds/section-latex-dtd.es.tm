<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Secciones estilo <LaTeX>>

  El d.t.d. section-latex provee las etiquetas estándar para las secciones,
  que son las mismas en <LaTeX>. Las etiquetas más seccionales toman sólo un
  argumento: el nombre de la sección. En el futuro, planeamos proveer
  etiquetas alternativas con dos argumentos, que permitirán ver el cuerpo de
  una sección como parte de la estructura. Las siguientes etiqetas usualmente
  permiten secciones numeradas, que son referenciadas en la tabla de
  contenidos:

  <\explain|<markup|chapter>>
    Macro para producir un potencialmente numerado título de capítulo.
  </explain>

  <\explain|<markup|section>>
    Macro para producir un potencialmente numerado título de sección.
  </explain>

  <\explain|<markup|subsection>>
    Macro para producir un potencialmente numerado título de subsección.
  </explain>

  <\explain|<markup|subsubsection>>
    Macro para producir un potencialmente numerado título de subsubsección.
  </explain>

  <\explain|<markup|paragraph>>
    Macro para producir un potencialmente numerado título de párrafo.
  </explain>

  <\explain|<markup|subparagraph>>
    Macro para producir un potencialmente numerado título de subpárrafo.
  </explain>

  Las etiquetas <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*> y <markup|subparagraph*>
  pueden ser usados para producir variantes no numeradas de las etiquetas
  anteriores, las cuales no son referenciadas en la tabla de contenidos. El
  d.t.d. <tmdtd|section-latex> también provee las siguientes etiquetas:

  <\explain|<markup|chapter**>>
    Macro con dos argumentos: un tipo especial de capítulo (como ``Epílogo'')
    y el nombre del capítulo.
  </explain>

  <\explain|<markup|appendix>>
    Una variante de <markup|chapter> o <markup|section> para producir
    apéndices.
  </explain>

  <\explain|<markup|section-sep>>
    Un macro para personalizar el separador entre el número de una sección y
    su título. Por defecto, usamos dos espacios.
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