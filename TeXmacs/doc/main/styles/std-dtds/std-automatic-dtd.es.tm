<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Generación automática de contenido>

  El \ <abbr|d.t.d.> <tmdtd|std-automatic> se especifica para la generación
  automática de contenido auxiliar como tables de contenidos y bibliografías,
  así como también para la presentación de tal contenido auxiliar. Las
  siguientes etiquetas son usadas para las bibliografías:

  <\description>
    <expand|item*|<markup|cite>>Una función con un número arbitrario de
    argumentos. Cada argumento es una citación correspondients a un item en
    un archivo BiB-<TeX>. Las citaciones son mostradas en la misma forma como
    ellas son referenciadas en la bibliografía y también proveen hiperenlaces
    a las referencias correspondientes. Las citaciones son mostradas como
    macas de pregunta si usted no ha generado la bibliografía.

    <expand|item*|<markup|nocite*>>Similar a <markup|cite>, pero las
    citaciones no son mostradas en el texto principal.

    <expand|item*|<markup|bibitem*>>Una función que especifica como mostrar
    un item en la bibliografía.
  </description>

  Las siguientes etiquetas son usadas para compilar tablas de contenidos:

  <\description>
    <expand|item*|<markup|toc-main-1>>Una función con un argumento para crear
    entradas primordiales en la tabla de contenidos. Esta función puede por
    ejemplo ser usada cuando un libro consiste de varias partes.

    <expand|item*|<markup|toc-main-2>>Una función con un argumento para crear
    una entrada principal en una tabla de contenidos. Esta función es usada
    regularmente para capítulos.

    <expand|item*|<markup|toc-normal-1>>Una función con un argumento para
    crear una entrada normal en la tabla de contenidos. Esta función es
    frecuentemente usada para secciones.

    <expand|item*|<markup|toc-normal-2>>Similar a <markup|toc-normal-2> para
    entradas menos importantes como en subsecciones.

    <expand|item*|<markup|toc-normal-3>>Similar a <markup|toc-normal-3> para
    entradas incluso menos importantes como subsubsecciones.

    <expand|item*|<markup|toc-small-1>>Usada para entradas no muy importantes
    tales como párrafos (puede ser ignorada).

    <expand|item*|<markup|toc-small-2>>Usada para entradas incluso menos
    importantes tales como subpárrafos.

    <expand|item*|<markup|toc-dots>>La separación entre una entrada en la
    tabla de contenidos y el correspondiente número de página. Por defecto,
    usamos puntos horizontales.
  </description>

  Las siguientes etiquetas son usadas para índices:

  <\description>
    <expand|item*|<markup|index>>Una función con un argumento <var|x>, que
    inserta <var|x> en el índice como una entrada principal.

    <expand|item*|<markup|subindex>>Una función con dos argumentos <var|x> y
    <var|y>, que inserta <var|y> en el índice como una subentrada de <var|x>.

    <expand|item*|<markup|subsubindex>>Una función con tres argumentos
    <var|x>, <var|y> y <var|z>, que inserta <var|z> en el índice como una
    subentrada de <var|y>, que es a su vez una subentrada de <var|x>.

    <expand|item*|<markup|index-complex>>Una función con cuatro argumentos
    <var|key>, <var|how>, <var|range>, <var|entry>, que está documentada en
    la sección acerca de <apply|hyper-link|generación del
    índice|../../links/man-index.es.tm>.

    <expand|item*|<markup|index-line>>Esta función toma un argumento
    <var|key>, que dice como ordenar una entrada y el argumento actual
    <var|entry>. Ningún número de página es generado.

    <expand|item*|<markup|index-1>>Macro con una entrada índice y un número
    de página, que es usada para visualizar una entrada principal del index
    en el índice.

    <expand|item*|<markup|index-1*>>Similar a <markup|index-1>, pero sin el
    número de página.

    <expand|item*|<markup|index-<with|mode|math|n>>>(con <with|mode|math|n>
    entre <with|mode|math|1> y <with|mode|math|5>): macro con una entrada en
    el índice y un número de página, que es usado para visualizar una entrada
    en el índice de nivel <with|mode|math|n>.

    <expand|item*|<markup|index-<with|mode|math|n>*>>Similar a
    <markup|index-<with|mode|math|n>>, pero sin el número de página.

    <expand|item*|<markup|index-dots>>El macro que produce los puntos entre
    una entrada al índice y el (los) correspondiente(s) número(s) de página.
  </description>

  Las siguientes etiquetas son usadas en los glosarios:

  <\description>
    <expand|item*|<markup|glossary>>Una función que inserta su único
    argumento en el glosario.

    <expand|item*|<markup|glossary-dup>>Para crear un número de página
    adicional para una entrada que había sido insertada antes.

    <expand|item*|<markup|glossary-explain>>Una función para insertar una
    entrada en el glorario con su explicación.

    <expand|item*|<markup|glossary-line>>Inserta una entrada de glosario sin
    número de página.

    <expand|item*|<markup|glossary-1>>Macro for mostrar una entrada de
    glosario \ y su número de página correspondiente.

    <expand|item*|<markup|glossary-2>>Macro para presentar una entrada de
    glosario, su explicación y su número de página.

    <expand|item*|<markup|glossary-dots>>El macro que produce los putnos
    entre una entrada del glosario y el (los) correspondiente(s) número(s) de
    página.
  </description>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  Cárdenas>

  <expand|tmdoc-license|El permiso está garantizado para copiar, distribuir
  y/o modificar este documento bajo los terminos de la GNU Free Documentation
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
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-automatic>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nocite*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|bibitem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-1>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-2>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-1>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-2>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-dots>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subindex>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubindex>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-complex>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-line>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1*>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>*>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-dots>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dup>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-explain>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-line>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-1>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-2>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dots>>|<pageref|idx-34>>
    </associate>
  </collection>
</auxiliary>
