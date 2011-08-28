<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Comandi ibridi e simulazione <LaTeX> >

  <apply|TeXmacs> permette di attivare comandi <apply|LaTeX> direttamente
  dalla tastiera, come viene spiegato qui di seguito. Si preme innanzitutto
  il taso <key|\\> per attivare la modalità di comando ibrida
  <apply|LaTeX>/<apply|TeXmacs>. Poi si preme il comando che si desidera
  eseguire. Appena si finisce di scrivere il comando, a sinistra della barra
  a piè di pagina verrà visualizzato qualcosa del genere

  <\verbatim>
    \ \ \ \ \<less\>return\<gtr\>: comando da eseguire
  </verbatim>

  Quando si batte il tasto <key|return> il comando verrà
  eseguito. Per esempio, in modalità matematica, si può creare una frazione
  digitando <key|\\ f r a c return>.

  Se il comando richiesto non è (riconosciuto come) un comando <apply|LaTeX>,
  il programma controllerà innanzitutto se il comando esiste come macro,
  funzione o ambiente di <apply|TeXmacs> (forniti dal file di stile). Se è
  così, la macro, la funzione o l'ambiente corrispondente vengono applicati
  (con il numero corretto di argomenti). Altrimenti, il programma assume che
  il comando corrisponda ad una variabile d'ambiente e ne richiede il valore.
  Il tasto <key|\\> è sempre equivalente a uno dei comandi <key|inactive l>,
  <key|inactive e>, <key|inactive a>, <key|inactive #> o
  <key|inactive v>.

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
