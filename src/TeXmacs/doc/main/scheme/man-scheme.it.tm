<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Personalizzare <TeXmacs>>

  Una della maggiori caratteristiche di <TeXmacs> è che può essere
  profondamente personalizzato. Innanzitutto, gli aspetti più importanti del
  programma possono essere <apply|hyper-link|configurati|../config/man-configuration.it.tm>
  in <apply|menu|Edit|Preferences>. Quasi tutte le parti di <TeXmacs> possono
  essere interamente adattate o riprogrammate usando il linguaggio di
  estensione <name|Guile>/<name|Scheme>. Alcuni semplici esempi si possono
  trovare nelle sezioni seguenti:

  <\traverse>
    <apply|branch|Introduzione al linguaggio di estensione
    <name|Guile>|man-guile-intro.it.tm>

    <apply|branch|Scrivere dei file di inizializzazione
    personalizzati|man-initialization.it.tm>

    <apply|branch|Creare dei menu dinamici personalizzati|man-menus.it.tm>

    <apply|branch|Creare delle scorciatoie da tastiera
    personalizzate|man-keyboard.it.tm>

    <apply|branch|Altri file interessanti|man-files.it.tm>
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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
