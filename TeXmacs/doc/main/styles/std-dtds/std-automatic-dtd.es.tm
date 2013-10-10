<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Generación automática de contenido>

  El \ <abbr|d.t.d.> <tmdtd|std-automatic> se especifica para la generación
  automática de contenido auxiliar como tables de contenidos y bibliografías,
  así como también para la presentación de tal contenido auxiliar. Las
  siguientes etiquetas son usadas para las bibliografías:

  <\explain|<markup|cite>>
    Una función con un número arbitrario de argumentos. Cada argumento es una
    citación correspondients a un item en un archivo BiB-<TeX>. Las
    citaciones son mostradas en la misma forma como ellas son referenciadas
    en la bibliografía y también proveen hiperenlaces a las referencias
    correspondientes. Las citaciones son mostradas como macas de pregunta si
    usted no ha generado la bibliografía.
  </explain>

  <\explain|<markup|nocite*>>
    Similar a <markup|cite>, pero las citaciones no son mostradas en el texto
    principal.
  </explain>

  <\explain|<markup|bibitem*>>
    Una función que especifica como mostrar un item en la bibliografía.
  </explain>

  Las siguientes etiquetas son usadas para compilar tablas de contenidos:

  <\explain|<markup|toc-main-1>>
    Una función con un argumento para crear entradas primordiales en la tabla
    de contenidos. Esta función puede por ejemplo ser usada cuando un libro
    consiste de varias partes.
  </explain>

  <\explain|<markup|toc-main-2>>
    Una función con un argumento para crear una entrada principal en una
    tabla de contenidos. Esta función es usada regularmente para capítulos.
  </explain>

  <\explain|<markup|toc-normal-1>>
    Una función con un argumento para crear una entrada normal en la tabla de
    contenidos. Esta función es frecuentemente usada para secciones.
  </explain>

  <\explain|<markup|toc-normal-2>>
    Similar a <markup|toc-normal-2> para entradas menos importantes como en
    subsecciones.
  </explain>

  <\explain|<markup|toc-normal-3>>
    Similar a <markup|toc-normal-3> para entradas incluso menos importantes
    como subsubsecciones.
  </explain>

  <\explain|<markup|toc-small-1>>
    Usada para entradas no muy importantes tales como párrafos (puede ser
    ignorada).
  </explain>

  <\explain|<markup|toc-small-2>>
    Usada para entradas incluso menos importantes tales como subpárrafos.
  </explain>

  <\explain|<markup|toc-dots>>
    La separación entre una entrada en la tabla de contenidos y el
    correspondiente número de página. Por defecto, usamos puntos
    horizontales.
  </explain>

  Las siguientes etiquetas son usadas para índices:

  <\explain|<markup|index>>
    Una función con un argumento <var|x>, que inserta <var|x> en el índice
    como una entrada principal.
  </explain>

  <\explain|<markup|subindex>>
    Una función con dos argumentos <var|x> y <var|y>, que inserta <var|y> en
    el índice como una subentrada de <var|x>.
  </explain>

  <\explain|<markup|subsubindex>>
    Una función con tres argumentos <var|x>, <var|y> y <var|z>, que inserta
    <var|z> en el índice como una subentrada de <var|y>, que es a su vez una
    subentrada de <var|x>.
  </explain>

  <\explain|<markup|index-complex>>
    Una función con cuatro argumentos <var|key>, <var|how>, <var|range>,
    <var|entry>, que está documentada en la sección acerca de
    <hlink|generación del índice|../../links/man-index.es.tm>.
  </explain>

  <\explain|<markup|index-line>>
    Esta función toma un argumento <var|key>, que dice como ordenar una
    entrada y el argumento actual <var|entry>. Ningún número de página es
    generado.
  </explain>

  <\explain|<markup|index-1>>
    Macro con una entrada índice y un número de página, que es usada para
    visualizar una entrada principal del index en el índice.
  </explain>

  <\explain|<markup|index-1*>>
    Similar a <markup|index-1>, pero sin el número de página.
  </explain>

  <\explain|<markup|index-<math|n>>>
    (con <math|n> entre <math|1> y <math|5>): macro con una entrada en el
    índice y un número de página, que es usado para visualizar una entrada en
    el índice de nivel <math|n>.
  </explain>

  <\explain|<markup|index-<math|n>*>>
    Similar a <markup|index-<math|n>>, pero sin el número de página.
  </explain>

  <\explain|<markup|index-dots>>
    El macro que produce los puntos entre una entrada al índice y el (los)
    correspondiente(s) número(s) de página.
  </explain>

  Las siguientes etiquetas son usadas en los glosarios:

  <\explain|<markup|glossary>>
    Una función que inserta su único argumento en el glosario.
  </explain>

  <\explain|<markup|glossary-dup>>
    Para crear un número de página adicional para una entrada que había sido
    insertada antes.
  </explain>

  <\explain|<markup|glossary-explain>>
    Una función para insertar una entrada en el glorario con su explicación.
  </explain>

  <\explain|<markup|glossary-line>>
    Inserta una entrada de glosario sin número de página.
  </explain>

  <\explain|<markup|glossary-1>>
    Macro for mostrar una entrada de glosario \ y su número de página
    correspondiente.
  </explain>

  <\explain|<markup|glossary-2>>
    Macro para presentar una entrada de glosario, su explicación y su número
    de página.
  </explain>

  <\explain|<markup|glossary-dots>>
    El macro que produce los putnos entre una entrada del glosario y el (los)
    correspondiente(s) número(s) de página.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <tmdoc-license|El permiso está garantizado para copiar, distribuir y/o
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