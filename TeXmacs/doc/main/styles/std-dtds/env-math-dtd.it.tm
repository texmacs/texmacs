<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambienti matematici>

  Il <abbr|d.t.d.> <tmdtd|env-math> specifica quali ambienti matematici
  possono essere usati all'interno della modalità testo. In altre parole, gli
  ambienti dovrebbero essere utilizzati all'interno della modalità testo, ma
  i loro corpi contengono formule matematiche o tabelle di formule
  matematiche

  <\explain|<markup|equation>>
    Equazione numerata.
  </explain>

  <\explain|<markup|equation*>>
    Equazione non numerata.
  </explain>

  <\explain|<markup|eqnarray>>
    Lista di equazioni numerate (non ancora attiva).
  </explain>

  <\explain|<markup|eqnarray*>>
    Lista di equazioni non numerate.
  </explain>

  Nell'ambiente <markup|eqnarray*>, si può utilizzare il tag
  <markup|eq-number> per numerare l'equazione.\ 

  <\warning>
    La numerazione delle equazioni nelle tabelle non è ancora come dovrebbe
    essere. In particulare, attualmente il tag <markup|eqnarray> è
    equivalente a <markup|eqnarray*>. Quando il tag <markup|eqnarray> sarà
    implementato correttamente, sarà disponibile anche un tag
    <markup|no-number> per sopprimere il numero di un'equazione, e un
    pacchetto di stile per porre la numerazione a sinistra delle equazioni.
  </warning>

  <\warning>
    Non è ancora disponibile l'opzione per porre la numerazione a sinistra
    delle equazioni. Tuttavia, si può utilizzare il tag manuale
    <markup|leq-number> per fare ciò. Si ha anche a disposizione un
    <markup|next-number> che visualizza direttamente il numero e aumenta il
    contatore dell'equazione.
  </warning>

  <\warning>
    Noi non incoraggiamo l'uso degli ambienti AMS-<TeX> <verbatim|align>,
    <verbatim|gather> e <verbatim|split>. Comunque, essi sono disponibili
    sotto i nomi <markup|align>, <markup|gather>, <markup|eqsplit> insieme
    con le loro varianti <markup|align*>, <markup|gather*> e
    <markup|eqsplit*>. In futuro, prevediamo di implementare ambienti più
    potenti.
  </warning>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Lucia Gecchelin|Andrea
  Centomo>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|italian>
  </collection>
</initial>