<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Markup matematici standard>

  I markup matematici standard sono definiti in <tmdtd|std-math>.

  <\explain|<markup|binom>>
    Per i coefficienti binomiali <math|<binom|n|m>>.
  </explain>

  <\explain|<markup|choose>>
    Nome alternativo per <markup|binom> (obsoleto).
  </explain>

  <\explain|<markup|shrink-inline>>
    Macro che permette di passare alla dimensione del testo quando non si è
    nello stile di visualizzazione. Questa macro viene usata principalmente
    dagli sviluppatori. Per esempio, la macro <markup|binom> la utilizza.
  </explain>

  I seguenti sono ambienti matematici standard per le tabelle:

  <\explain|<markup|matrix>>
    Per le matrici <math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|det>>
    Per i determinanti <math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|choice>>
    Per le liste di scelta <math|<around|\||x|\|>=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|<text|if
    >x\<leqslant\>0>>|<row|<cell|x,>|<cell|<text|if >x\<geqslant\>0>>>>>>
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia Gecchelin|Andrea
  Centomo>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|italian>
  </collection>
</initial>