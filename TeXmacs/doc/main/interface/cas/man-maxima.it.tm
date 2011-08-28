<TeXmacs|1.0.1.23>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Maxima>

  <name|Maxima> non è solamente uno dei più vecchi e migliori sistemi di
  computer algebra in circolazione, ma è anche un sistema dedicato per il
  quale esiste una libera implementazione. Per ottenere questo software
  potete consultare il sito\ 

  <\verbatim>
    \ \ \ \ http://www.ma.utexas.edu/users/wfs/maxima.html
  </verbatim>

  La versione supportata è <name|Maxima> 5.6 basata su <name|GCL>. Per la
  corrispondente versione basata su <name|CLisp> è necessario editare il file
  <verbatim|tm_maxima> e rimpiazzare <verbatim|-load> con <verbatim|-i>. Per
  <name|Maxima> 5.9-pre, replace <verbatim|-load> con <verbatim|-p>. Problemi
  noti:

  <\itemize>
    <item>se si preme <key|return> quando un comando è
    incompleto (tipicamente non concluso con <verbatim|;> o <verbatim|$>),
    l'interfaccia si blocca;

    <item>se si causa la comparsa del Lisp break prompt l'interfaccia si
    blocca;

    <item>il comando <verbatim|info> non è supportato (è definito nel Lisp
    soggiacente e difficile da supportare in modo portabile);

    <item>alcuni comandi nel debugger funzionano, altri (incluso
    <verbatim|:c>) misteriosamente non funzionano;

    <item>il comando <verbatim|load> si comporta talvolta in modo strano.
  </itemize>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Andrea Centomo>

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
