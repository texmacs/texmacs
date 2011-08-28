<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ulteriore personalizzazione dell'interfaccia>

  Dopo aver scritto un'interfaccia funzionante tra un dato sistema e
  <apply|TeXmacs> è possibile migliorarne l'implementazione. Vedremo tra poco
  in quali direzioni questo sia possibile.

  In primo luogo è possibile personalizzare il comportamento della tastiera
  all'interno di una sessione <verbatim|myplugin>, aggiungendo i menu
  ritenuti più opportuni. Il metodo per realizzare questo è descritto nel
  capitolo riguardante il linguaggio di estensione <name|Guile/Scheme> ed è
  possibile aggiungere questo supporto nel file <verbatim|init-myplugin.scm>.
  Ancora una volta si consiglia di dare un'occhiata ai materiali relativi ai
  plugin attualmente già implementati in <TeXmacs> che si trovano all'interno
  della directory <verbatim|$TEXMACS_HOME_PATH/plugins>.

  Alcuni output del sistema da implementare potrebbero richiedere dei
  marcatori speciali. Ad esempio, se si vuole associare un tipo invisibile a
  ciascuna sub-espressione dell'output. Allora conviene creare una macro
  <verbatim|exprtype>, con due argomenti, in <verbatim|myplugin.ts> e mandare
  a <TeXmacs>, in fase di output, un'espressione <apply|LaTeX> del tipo
  <verbatim|\\exprtype{1}{Integer}>.

  Nel caso in cui per connettere un sistema a <apply|TeXmacs> siano state
  usate delle pipe, è possibile eseguire direttamente comandi <apply|TeXmacs>
  durante l'output, incorporando nell'output parti di codice del tipo:

  <\verbatim>
    \ \ \ \ [DATA_BEGIN]command:scheme-program[DATA_END]
  </verbatim>

  Quando invece il cursore si trova all'interno di una sessione del sistema
  interfacciato, è possibile utilizzare il comando <name|Scheme>:

  <\verbatim>
    \ \ \ \ (extern-exec plugin-command)
  </verbatim>

  attraverso il quale è possibile dare un comando prestabilito.

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
