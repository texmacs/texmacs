<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Entornos>

  En forma similar a las etiquetas basadas en contenido, los entornos son
  usados para marcar porciones de un texto con un significado especial. Sin
  embargo mientras las <apply|hyper-link|etiquetas basadas en
  contenido|man-content-tags.es.tm> usualmente encierran pequeñas porciones
  de texto, los entornos a menudo encierran porciones que son de varios
  párrafos de longitud. Los entornos frecuentemente usados en matemáticas son
  <markup|theorem> y <markup|proof>, como en el ejemplo de abajo:

  <\theorem>
    No existen enteros positivos <with|mode|math|a>, <with|mode|math|b>,
    <with|mode|math|c> y <with|mode|math|n> con
    <with|mode|math|n\<geqslant\>3>, tales que
    <with|mode|math|a<rsup|n>+b<rsup|n>=c<rsup|n>>.
  </theorem>

  <\proof>
    No tengo espacio suficiente aquí para escribir la prueba.
  </proof>

  Usted puede ingresar entornos usando <apply|menu|Insert|Environment>. Otros
  entornos con una visualización similar a los teoremas con
  <markup|proposition>, <markup|lemma>, <markup|corollary>, <markup|axiom>,
  <markup|definition>. Puede usar el macro <markup|dueto> (ingresado usando
  <key|\\ d u e t o return>) a fin de especificar la(s) persona(s) a la(s)
  cual(es) el teorema es debido, como en

  <\theorem>
    <dueto|Pitágoras>Bajo adecuadas circunstancias, tenemos
    <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  Otros entornos frecuentemente usados con una visualización similar a los
  teoremas, pero que no enfatizan el texto encerrado, son <markup|remark>,
  <markup|note>, <markup|example>, <markup|warning>, <markup|exercise> y
  <markup|problem>. Los entornos restantes <markup|verbatim>, <markup|code>,
  <markup|quote>, <markup|quotation> y <markup|verse> pueden ser usados a fin
  de entrar texto o códigio multipárrafo, citas o poesía.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-30|<tuple|2|?>>
    <associate|idx-20|<tuple|2|?>>
    <associate|idx-10|<tuple|2|?>>
    <associate|idx-31|<tuple|2|?>>
    <associate|idx-21|<tuple|2|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|2|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-22|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-23|<tuple|2|?>>
    <associate|idx-14|<tuple|2|?>>
    <associate|idx-24|<tuple|2|?>>
    <associate|idx-15|<tuple|2|?>>
    <associate|idx-25|<tuple|2|?>>
    <associate|idx-16|<tuple|2|?>>
    <associate|idx-26|<tuple|2|?>>
    <associate|idx-17|<tuple|2|?>>
    <associate|idx-27|<tuple|2|?>>
    <associate|idx-18|<tuple|2|?>>
    <associate|idx-28|<tuple|2|?>>
    <associate|idx-19|<tuple|2|?>>
    <associate|idx-29|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Entorno>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proposition>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|lemma>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|corollary>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|axiom>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|definition>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dueto>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|remark>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|note>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|example>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|warning>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercise>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|problem>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quote>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quotation>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verse>>|<pageref|idx-20>>
    </associate>
  </collection>
</auxiliary>
