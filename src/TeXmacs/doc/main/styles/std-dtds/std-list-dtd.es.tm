<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Standard lists>

  Las listas <TeXmacs> estandar están definidas en <tmdtd|std-list>. Las
  listas no numeradas son:

  <\description>
    <expand|item*|<markup|itemize>>La etiqueta antes de cada item depende de
    la profunidad de anidamiento.

    <expand|item*|<markup|itemize-minus>>Usa <with|mode|math|-> para la
    etiqueta.

    <expand|item*|<markup|itemize-dot>>Usa <with|mode|math|\<bullet\>> para
    la etiqueta.

    <expand|item*|<markup|itemize-arrow>>Usa <with|mode|math|\<rightarrow\>>
    para la etiqueta.
  </description>

  Las lista numeradas corresponden a los siguientes entornos:

  <\description>
    <expand|item*|<markup|enumerate>>La clase de número antes de cada item
    depende de la profunidad de anidamiento.

    <expand|item*|<markup|enumerate-numeric>>Numera los items por 1, 2, 3,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-roman>>Numera los items por i, ii, iii,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-Roman>>Numera los items por I, II, III,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-alpha>>Numera los items por a), b), c),
    <abbr|etc.>

    <expand|item*|<markup|enumerate-Alpha>>Numera los items por A, B, C,
    <abbr|etc.>
  </description>

  Los siguients entornos puedes ser usadas para listas descriptivas.

  <\description>
    <expand|item*|<markup|description>>El entorno por defecto para listas
    descriptivas (usualmente <markup|description-compact>).

    <expand|item*|<markup|description-compact>>Alinea los lados izquierdos de
    los items en las listas y pone sus descripciones prontamente después de
    estos.

    <expand|item*|<markup|description-dash>>Similar a
    <markup|description-compact>, pero usa un  para seperar cada item de su
    descripción.

    <expand|item*|<markup|description-align>>Alinea los lados izquierdos de
    las descripciones, mientras alínea los items a la derecha.

    <expand|item*|<markup|description-long>>Pone los items y sus
    descripciones en líneas distintas.
  </description>

  Los nuevos items en una lista son indicados a través de la etiqueta
  <markup|item> o la etiqueta unaria <markup|item*> en el caso de las
  descripcioones. Los desarrolladores también encontrarán unos pocos macros
  adicionales, pero inestables en <tmdtd|std-list> para definir estructuras
  de lista adicionales.

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
    <associate|language|english>
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
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-list>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-minus>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-dot>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-arrow>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-numeric>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-roman>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-Roman>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-alpha>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-Alpha>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-dash>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-align>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-long>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|item>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|item*>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-list>>|<pageref|idx-21>>
    </associate>
  </collection>
</auxiliary>
