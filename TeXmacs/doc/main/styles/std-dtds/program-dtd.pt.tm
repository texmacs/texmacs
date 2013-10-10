<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Marcação especial para programas e sessões interativas>

  A <abbr|d.t.d.> <tmdtd|program> fornece principalmente os seguintes
  ambientes para álgebra computacional:

  <\explain|<markup|session>>
    Macro com três argumentos: a linguagem de álgebra computacional, o nome
    da sessãoe e o próprio corpo da sessão.
  </explain>

  <\explain|<markup|input>>
    Macro com dois argumentos: um prompt e a própria entrada.
  </explain>

  <\explain|<markup|output>>
    Macro com o corpo da saída como seu argumento.
  </explain>

  Na verdade, estes ambientes estão baseados em ambientes da forma
  <markup|<em|lan>-session>, <markup|<em|lan>-input> e
  <markup|<em|lan>-output> para cada linguagem <verbatim|<em|lan>>.

  A <abbr|d.t.d.> <tmdtd|program> também fornece alguma marcação para
  diagramação de programas de computador. Estas etiquetas devem ser
  consideradas, no entanto, muito voláteis, já que pretendemos substituí-las
  por um conjunto de etiquetas bem mais detalhado:

  <\explain|<markup|algorithm>>
    Macro com dois argumentos, o nome do algoritmo e o algoritmo em si,
    possivelmente acoplado à sua especificação.
  </explain>

  <\explain|<markup|body>>
    O corpo do algoritmo.
  </explain>

  <\explain|<markup|indent>>
    Para recuar parte do algoritmo.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito Willmersdorf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|portuguese>
  </collection>
</initial>