<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Formule matematiche>

  Per poter comporre delle formule matematiche, si deve innanzitutto attivare
  la ``modalità matematica'' premendo il tasto <key|$> o inserendo
  un'equazione (utilizzando <apply|menu|Insert|Mathematics|Equation>). In
  modalità matematica, si hanno a disposizione dei comandi specifici e delle
  combinazioni di tasti per scrivere simboli matematici e formule. Per
  esempio, il prefisso <prefix|M-A-> può essere utilizzato per scrivere le lettere
  greche (si ricordi che <prefix|M-A-> è equivalente a \ <prefix|math:greek>,
  <key|escape escape escape> o
  <prefix|A-C->).

  L'editor agevola la scrittura di formule matematiche seguendo certe regole.
  Questa proprietà, che sarà ulteriormente sviluppata nelle versioni future,
  è utile quando si interagisce con un pacchetto di computer algebra.
  Attualmente, si deve, per esempio, scrivere esplicitamente il simbolo della
  moltiplicazione <key|*> tra i simboli <with|mode|math|a> e
  <with|mode|math|b>. Per default, scivendo <key|a b> si otterrà
  <with|mode|math|mode|text|ab> e non <with|mode|math|a*b>.

  <\traverse>
    <apply|branch|Oggetti matematici principali|keyboard/man-main.it.tm>

    <apply|branch|Simboli matematici|keyboard/man-symbols.it.tm>

    <apply|branch|Operatori grandi|keyboard/man-big.it.tm>

    <apply|branch|Delimitatori grandi|keyboard/man-large.it.tm>

    <apply|branch|Accenti larghi|keyboard/man-wide.it.tm>
  </traverse>

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
    <associate|preamble|false>
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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Matematica>|<with|font
      family|<quote|ss>|Equazione>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
