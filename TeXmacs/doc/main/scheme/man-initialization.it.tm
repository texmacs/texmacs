<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Scrivere file di inizializzazione personalizzati>

  Quando si apre <apply|TeXmacs> viene eseguito il file

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/init-texmacs.scm
  </verbatim>

  e, se esiste, viene eseguito anche il file dell'utente

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/progs/my-init-texmacs.scm
  </verbatim>

  Per default, il path <verbatim|$TEXMACS_HOME_PATH> coincide con
  <verbatim|.TeXmacs>. Analogamente, ogni volta che si crea un nuovo buffer,
  viene eseguito il file

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/init-buffer.scm
  </verbatim>

  e, se esiste, viene eseguito anche

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/progs/my-init-buffer.scm
  </verbatim>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia
  Gecchelin|Andrea Centomo>

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
    <associate|language|italian>
  </collection>
</initial>
