<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Portare <TeXmacs> su altre piattaforme>

  Dal momento che abbiamo principalmente accesso a sistemi PC/Linux e SUN,
  siamo interessati a collaborare con persone che desiderano portare
  <apply|TeXmacs> su altri sistemi Unix o con X Window, e a mantenere la
  corrispondente distribuzione. Se desiderate fare questo date un'occhiata ai
  file:

  <\verbatim>
    \ \ \ \ configure.in<format|next line> \ \ \ src/Basic/fast_alloc.cpp
  </verbatim>

  Sono benvenuti gli specialisti di <verbatim|autoconf>, redhat e rpm, non da
  ultimo per comunicare suggerimenti, patches, ecc....

  Oltre al porting di <apply|TeXmacs> su altri sistemi Unix, sarebbe
  interessante avere il porting di <apply|TeXmacs> sotto Windows e Mac OS.
  Per dare aiuti in questa direzione conviene prima di tutto iscriversi alla
  mailing list <verbatim|texmacs-dev@gnu.org>. Sono in corso discussioni su
  come eseguire i diversi porting e su quale interfaccia grafica sia più
  adatta a questo scopo (Gtk, Qt, Wxwindows o GNUstep). La nostra strategia
  attuale consiste nel sistemare tutte le parti di codice relative alla GUI
  in una ben specificata TMGUI API e solo successivamente eseguirne il
  porting. Ciò permette di supportare sistemi multipli di interfacce
  grafiche. Ulteriori dettagli si possono comunque avere consultando gli
  archivi della mailing list <verbatim|texmacs-dev@gnu.org>.

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
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
    <associate|toc-5|<tuple|<uninit>|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>
