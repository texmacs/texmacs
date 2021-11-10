<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Bordi, spaziature e colori di sfondo>

  Si possono specificare le larghezze dei bordi e le spaziature di una cella
  nelle quattro direzioni possibili: a sinistra, a destra, in basso e in alto
  (si veda il menu <apply|menu|Table|Cell border>). Per specificare le
  dimensioni dei bordi e le spaziature di una cella si possono anche
  utilizzare delle scorciatoie da tastiera della forma <key|table b
 ><render-key|<with|mode|math|x>> e <key|table p><render-key|<with|mode|math|x>>.

  Per default, la larghezza dei bordi delle celle nell'ambiente blocco è
  <verbatim|1ln>, cioè la larghezza standard di una linea nel font corrente
  (come la larghezza di una barra di frazione). Questa larghezza viene
  utilizzata a destra e in basso di ogni cella (tranne quando la cella è
  nella prima riga o nella prima colonna). La spaziatura orizzontale di
  default per le celle è <verbatim|1spc>: l'ampiezza di uno spazio bianco nel
  font corrente. La spaziatura verticale è di <verbatim|1sep>: la separazione
  minima standard tra due rettangoli consecutivi.

  Attraverso <apply|menu|Table|Cell background color> si può assegnare un
  colore di sfondo alle celle.

  Si può dare un bordo e una spaziatura per l'intera tabella in
  <apply|menu|Table|Special table properties|Border>. In questo caso, la
  spaziatura è applicata all'esterno del bordo.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Bordo della cella>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Colore di sfondo della cella>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabella>|<with|font
      family|<quote|ss>|Proprietà speciali della tabella>|<with|font
      family|<quote|ss>|Bordo>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
