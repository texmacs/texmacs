<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambienti matematici>

  Il <abbr|d.t.d.> <tmdtd|env-math> specifica quali ambienti matematici
  possono essere usati all'interno della modalità testo. In altre parole, gli
  ambienti dovrebbero essere utilizzati all'interno della modalità testo, ma
  i loro corpi contengono formule matematiche o tabelle di formule
  matematiche

  <\description>
    <expand|item*|<markup|equation>>Equazione numerata.

    <expand|item*|<markup|equation*>>Equazione non numerata.

    <expand|item*|<markup|eqnarray>>Lista di equazioni numerate (non ancora
    attiva).

    <expand|item*|<markup|eqnarray*>>Lista di equazioni non numerate.
  </description>

  Nell'ambiente <markup|eqnarray*>, si può utilizzare il tag
  <markup|eqnumber> per numerare l'equazione.\ 

  <\warning>
    La numerazione delle equazioni nelle tabelle non è ancora come dovrebbe
    essere. In particulare, attualmente il tag <markup|eqnarray> è
    equivalente a <markup|eqnarray*>. Quando il tag <markup|eqnarray> sarà
    implementato correttamente, sarà disponibile anche un tag
    <markup|nonumber> per sopprimere il numero di un'equazione, e un
    pacchetto di stile per porre la numerazione a sinistra delle equazioni.
  </warning>

  <\warning>
    Non è ancora disponibile l'opzione per porre la numerazione a sinistra
    delle equazioni. Tuttavia, si può utilizzare il tag manuale
    <markup|leqnumber> per fare ciò. Si ha anche a disposizione un
    <markup|nextnumber> che visualizza direttamente il numero e aumenta il
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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-14|<tuple|3|?>>
    <associate|idx-15|<tuple|3|?>>
    <associate|idx-16|<tuple|3|?>>
    <associate|idx-17|<tuple|3|?>>
    <associate|idx-18|<tuple|3|?>>
    <associate|idx-19|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-math>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnumber>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nonumber>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|leqnumber>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nextnumber>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align*>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather*>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit*>>|<pageref|idx-19>>
    </associate>
  </collection>
</auxiliary>
