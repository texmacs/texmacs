<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Formato documenti (0.3.4)>

  Il formato TeXmacs dei documenti è stato profondamente modificato in modo
  da rendere, in futuro, TeXmacs compatibile con il formato XML. Fatto ancora
  più importante, i vecchi ambienti di stile come

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>environment\|open\|close\<gtr\>\<gtr\>,
  </verbatim>

  applicati tramite corrispondenza a coppie
  \ <verbatim|\<less\>begin\|env\<gtr\>text\<less\>end\|env\<gtr\>>, sono
  stati rimpiazzati da macro\ 

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>macro\|body\|open\<less\>body\<gtr\>close\<gtr\>\<gtr\>,
  </verbatim>

  applicate attraverso un'unica espansione
  <verbatim|\<less\>expand\|env\|text\<gtr\>>. In modo del tutto analogo,
  coppie corrispondenti <verbatim|\<less\>set\|var\|val\<gtr\>text\<less\>reset\|var\<gtr\>>,
  relative a variabili ambientali, sono state sostituite da costrutti del
  tipo <verbatim|\<less\>with\|var\|val\|text\<gtr\>> più prossimi agli
  attributi XML. Da un punto di vista tecnico queste modifiche comportano
  complicazioni nel caso in cui il corpo del <verbatim|testo> sia composto di
  molti paragrafi. Per questo i documenti strutturati in modo poco accurato
  possono venire visualizzati in modo scorretto nelle nuove versioni del
  programa (finora comunque abbiamo rilevato solo una modifica minore nei
  nostri documenti). Oltre a questo, per mantenere un alto livello di
  strutturazione del documento il comportamento dell'editor, relativamente ad
  ambienti che prevedono paragrafi multipli, è stato modificato leggermente.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>
