<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creare scorciatoie da tastiera personalizzate>

  Le corrispondenze dei tasti sono specificate usando il comando

  <\verbatim>
    \ \ \ \ (kbd-map predicate . keymaps)
  </verbatim>

  La parte <verbatim|predicate> specifica sotto quali condizioni le
  corrispondenze dei tasti sono valide. Esempi di predicati sono
  <verbatim|always?>, <verbatim|in-math?> e <verbatim|in-french?>, ma
  l'utente può definire dei predicati personalizzati. Ogni elemento in
  <verbatim|keymaps> può avere una delle seguenti forme:

  <\verbatim>
    \ \ \ \ (key-combination action_1 ... action_n)<format|next line>
    \ \ \ (key-combination result)<format|next line> \ \ \ (key-combination
    result help-message)
  </verbatim>

  Nel primo caso, le <verbatim|action_i> sono i comandi <apply|scheme>
  associati alla stringa <verbatim|key-combination>. Nel secondo e nel terzo
  caso, <verbatim|result> è una stringa che deve essere inserita nel testo
  dopo che la combinazione di tasti <verbatim|key-combination> è stata
  eseguita. Può essere visualizzato un ulteriore messaggio
  <verbatim|help-message> quando viene eseguita <verbatim|key-combination>.

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
