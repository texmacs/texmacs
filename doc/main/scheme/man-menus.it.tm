<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creare menu dinamici personalizzati>

  Si può definire (o modificare) un (una parte di un) menu con il nome
  <verbatim|name> usando

  <\verbatim>
    \ \ \ \ (menu-bind name . prog)
  </verbatim>

  e aggiungere nuove voci ad un (ad una parte di un) menu esistente con il
  nome <verbatim|name> usando

  <\verbatim>
    \ \ \ \ (menu-extend name . prog)
  </verbatim>

  Qui <verbatim|prog> è un programma che rappresenta le voci del menu. In
  particolare, si possono guardare i file nella directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu
  </verbatim>

  per vedere come sono definiti i menu standard di <apply|TeXmacs>.

  Più precisamente, il programma <verbatim|prog> in <verbatim|menu-set> o
  <verbatim|menu-append> è una lista di voci che possono avere una delle
  seguenti forme:

  <\verbatim>
    \ \ \ \ (=\<gtr\> "pulldown menu name" menu-definition)<format|next line>
    \ \ \ (-\<gtr\> "pullright menu name" menu-definition)<format|next line>
    \ \ \ ("entry" action)<format|next line> \ \ \ ---<format|next line>
    \ \ \ (if condition menu-definition)<format|next line> \ \ \ (link
    variable)
  </verbatim>

  I costruttori <verbatim|=\<gtr\>> e <verbatim|-\<gtr\>> sono utilizzati per
  creare dei menu che si aprono verso il basso o verso destra e
  <verbatim|menu-definition> deve contenere un programma che crea il submenu.
  Il costruttore <verbatim|("entry" action)> crea una voce normale, dove
  <verbatim|action> sarà compilata ed eseguita quando si clicca su
  <verbatim|entry>. Gli elementi del menu possono essere separati usando
  <verbatim|--->. Il costruttore <verbatim|if> è usato per inserire elementi
  del menu solamente se una certa <verbatim|condition> è soddisfatta (per
  esempio, se ci si trova in modalità matematica).

  Infine, se si dichiara un menu <verbatim|name>, allora si può utilizzare
  questo menu indirettamente usando il costruttore <verbatim|link>. Questo
  modo indiretto di dichiarare dei sottomenu ha due vantaggi

  <\itemize>
    <item>Un sottomenu ``indiretto'' può essere collegato a tanti menu quanti
    si vuole.

    <item>Nuovi elementi possono essere aggiunti a dei sottomenu
    ``indirecti'' <with|font shape|italic|a posteriori> usando
    <verbatim|menu-append>.
  </itemize>

  I menu principali di <apply|TeXmacs> sono <verbatim|texmacs-menu>,
  <verbatim|texmacs-popup-menu>, <verbatim|texmacs-main-icons>,
  <verbatim|texmacs-context-icons> e <verbatim|texmacs-extra-icons>. Altri
  menu standard indiretti sono <verbatim|file-menu>, <verbatim|edit-menu>,
  <verbatim|insert-menu>, <verbatim|text-menu>, <verbatim|paragraph-menu>,
  <verbatim|document-menu>, <verbatim|options-menu> and <verbatim|help-menu>.

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
