<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Introduzione>

  Come <name|Emacs>, <apply|TeXmacs> supporta un linguaggio di estensione di
  tipo <name|Lisp>, ovvero il dialetto <with|font shape|small-caps|Guile
  Scheme> del progetto <with|font shape|small-caps|Gnome>. Per la
  documentazione su <with|font shape|small-caps|Guile Scheme>, facciamo
  riferimento a\ 

  <\verbatim>
    \ \ \ http://www.gnu.org/software/guile/guile.html
  </verbatim>

  <apply|scheme> ha il vantaggio che può essere esteso con tipi e routine
  esterne del C e C++. Nel nostro caso, abbiamo esteso <apply|scheme> con
  routine che si possono utilizzare per creare dei menu e delle combinazioni
  di tasti personalizzati, e persino per scrivere delle proprie estensioni
  per <apply|TeXmacs>.

  Se si sono scaricati i file sorgenti di <apply|TeXmacs>, potrebbe essere
  interessante dare un'occhiata ai file\ 

  <\verbatim>
    \ \ \ Guile/Glue/build-glue-basic.scm<format|next line>
    \ \ Guile/Glue/build-glue-editor.scm<format|next line>
    \ \ Guile/Glue/build-glue-server.scm
  </verbatim>

  Questi tre file ``glue'' contengono le routine C++ che sono visibili in
  <apply|scheme>. Qui di seguito, discuteremo alcune delle routine più
  importanti. In futuro, scriveremo una guida di riferimento più completa
  sull'argomento. Si possono comunque guardare i file scheme <verbatim|.scm>
  nella directory <verbatim|$TEXMACS_PATH/progs>.

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
