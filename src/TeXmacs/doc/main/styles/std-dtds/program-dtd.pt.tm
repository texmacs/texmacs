<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Marcação especial para programas e sessões interativas>

  A <abbr|d.t.d.> <tmdtd|program> fornece principalmente os seguintes
  ambientes para álgebra computacional:

  <\description>
    <expand|item*|<markup|session>>Macro com três argumentos: a linguagem de
    álgebra computacional, o nome da sessãoe e o próprio corpo da sessão.

    <expand|item*|<markup|input>>Macro com dois argumentos: um prompt e a
    própria entrada.

    <expand|item*|<markup|output>>Macro com o corpo da saída como seu
    argumento.
  </description>

  Na verdade, estes ambientes estão baseados em ambientes da forma
  <markup|<em|lan>-session>, <markup|<em|lan>-input> e
  <markup|<em|lan>-output> para cada linguagem <verbatim|<em|lan>>.

  A <abbr|d.t.d.> <tmdtd|program> também fornece alguma marcação para
  diagramação de programas de computador. Estas etiquetas devem ser
  consideradas, no entanto, muito voláteis, já que pretendemos substituí-las
  por um conjunto de etiquetas bem mais detalhado:

  <\description>
    <expand|item*|<markup|algorithm>>Macro com dois argumentos, o nome do
    algoritmo e o algoritmo em si, possivelmente acoplado à sua
    especificação.

    <expand|item*|<markup|body>>O corpo do algoritmo.

    <expand|item*|<markup|indent>>Para recuar parte do algoritmo.
  </description>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

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
    <associate|language|portuguese>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
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
      magenta>|program>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|session>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|input>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|output>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|<with|font shape|<quote|italic>|lan>-session>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|<with|font shape|<quote|italic>|lan>-input>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|<with|font shape|<quote|italic>|lan>-output>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|program>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|algorithm>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|body>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|indent>>|<pageref|idx-11>>
    </associate>
  </collection>
</auxiliary>
