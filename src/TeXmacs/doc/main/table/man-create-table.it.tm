<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creare tabelle>

  Per poter creare una tabella, si può utilizzare il menu
  <apply|menu|Insert|Table> o una delle seguenti combinazioni di tasti:

  <\description>
    <expand|item*|<expand|kbd-table|N t>>Per creare una tabella normale.

    <expand|item*|<expand|kbd-table|N T>>Per creare una tabella le cui celle
    sono centrate.

    <expand|item*|<expand|kbd-table|N b>>Per creare un ``blocco'' normale, le
    cui celle sono separate da linee.

    <expand|item*|<expand|kbd-table|N B>>Per creare un blocco le cui celle
    sono centrate.
  </description>

  In modalità matematica, sono disponibili altre strutture di tipo tabella:

  <\description>
    <expand|item*|<expand|kbd-table|N m>>Per creare una matrice.

    <expand|item*|<expand|kbd-table|N d>>Per creare un determinante.

    <expand|item*|<expand|kbd-table|N c>>Per creare una lista di scelta.
  </description>

  Anche l'ambiente <verbatim|\\eqnarray*> è una speciale struttura di tipo
  tabella, che si estende su più righe. Si può creare una lista di equazioni
  utilizzando <apply|menu|Text|Mathematics|Equations>.

  Appena si crea una tabella, le sue dimensioni sono minime (solitamente
  <with|mode|math|1\<times\>1>) e le sue celle sono vuote. Nuove righe e
  colonne vengono inserite utilizzando le combinazioni di tasti
  <key|A-<expand|key-left>>, <key|A-<expand|key-right>>,
  <key|A-<expand|key-up>> e <key|A-<expand|key-down>>. Per esempio,
  <key|A-<expand|key-right>> crea una nuova colonna a destra della posizione
  corrente del cursore. Si può anche inserire una nuova riga sotto la
  posizione corrente del cursore premendo il tasto <key|<expand|key-return>>.

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
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Tabella>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Matematica>|<with|font
      family|<quote|ss>|Equazioni>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
