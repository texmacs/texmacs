<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Generare un indice analitico>

  Per generare un indice, innanzitutto devono essere inserite le voci
  dell'indice nel proprio documento usando <apply|menu|Insert|Link|Index
  entry>. Al passo successivo, si deve porre il cursore nella posizione in
  cui si desidera che venga generato l'indice e di deve cliccare su
  <apply|menu|Insert|Automatic|Index>. L'indice verrà così generato in maniera
  simile al sommario.

  Nel menu <apply|menu|Insert|Link|Index entry>, si trovano diversi tipi voci
  dell'indice. I più semplici sono ``pricipale'', ``secondo livello'',
  ``terzo livello'', che sono macro con uno, due e tre argomenti
  rispettivamente. Le voci della forma ``secondo livello'' e ``terzo
  livello'' possono essere utilizzate per voci di indici subordinati rispetto
  ad altri.

  Le voci di un indice complesso ammettono quattro argomenti. Il primo è una
  chiave secondo la quale la voce sarà ordinata e dovrà essere una ``-upla''
  (creata utilizzando <key|inactive \<less\>>) la cui prima componente è la
  categoria principale, la seconda una sottocategoria, ecc. Il secondo
  argomento delle voci di un indice complesso è bianca o ``strong'', nel qual
  caso il numero della pagina della voce corrispondente apparirà in
  grassetto. Il terzo argomento solitamente è vuoto, ma se si creano due voci
  dell'indice con il medesimo terzo argomento non vuoto, allora questo creerà
  un intervallo di numeri di pagine. Il quarto argomento, che pure è una
  -upla, è la voce stessa.

  È anche possibile creare nell'indice una riga senza un numero di pagina
  utilizzando ``interiezione'' in <apply|menu|Insert|Link|Index entry>. Il
  primo argomento di questa macro è una chiave che indica come ordinare la
  riga dell'indice. Il secondo argomento contiene il testo effettivo. Questo
  costrutto può essere utile per creare diverse sezioni ``A'', ``B'', ecc.
  nel proprio indice.

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
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font family|<quote|ss>|Voce
      dell'indice analitico>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Automatico>|<with|font family|<quote|ss>|Indice
      analitico>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font family|<quote|ss>|Voce
      dell'indice analitico>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserisci>|<with|font
      family|<quote|ss>|Collegamento>|<with|font family|<quote|ss>|Voce
      dell'indice analitico>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
