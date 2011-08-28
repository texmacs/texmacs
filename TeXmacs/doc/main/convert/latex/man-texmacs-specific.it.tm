<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Caratteristiche specifiche di <TeXmacs> >

  Alcune primitive di composizione tipografica di <apply|TeXmacs> non
  possiedono un analogo in <apply|LaTeX>, e l'algoritmo di conversione li
  trasformerà semplicemente in spazi bianchi. Alcune caratteristiche
  principali che sono specifiche di <apply|TeXmacs> sono le seguenti:

  <\itemize>
    <item>Primi a sinistra.

    <item>Separatori grandi tra le parentesi grandi.

    <item>Mosaici.

    <item>Alberi.

    <item>Macro personalizzate complesse.

    <item>Spazi verticali ``prima'' e ``dopo''.

    <item>Flag di indentazione ``prima'' e ``dopo''.
  </itemize>

  Si dovrebbe cercare di evitare di usare queste caratteristiche specifiche
  di <apply|TeXmacs>, se si prevede di convertire il proprio documento in
  <apply|LaTeX>. Comunque, in futuro, il programma di conversione dovrebbe
  generare per default un file encapsulated postscript di una conversione più
  intellegibile.

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
    <associate|toc-1|<tuple|<uninit>|?>>
  </collection>
</references>
