<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Definire nuovi ambienti>

  <tmdtd|env-manage> contiene dei markup di alto livello che possono essere
  utilizzati dall'utente per definire nuovi ambienti per i teoremi, le note,
  gli esercizie e le figure:

  <\description>
    <expand|item*|<markup|newtheorem>>Definisce un ambiente di tipo teorema.
    Si deve specificare un nome per l'ambiente (come ``esperimento'') e il
    testo corrispondente (come ``Esperimento'').

    <expand|item*|<markup|newremark>>Analogo a <markup|newtheorem>, ma per le
    note.

    <expand|item*|<markup|newexercise>>Analogo a <markup|newtheorem>, ma per
    gli esercizi.

    <expand|item*|<markup|newfigure>>Analogo a <markup|newtheorem>, ma per le
    figure (per le coppie: grande e piccola).
  </description>

  Il <abbr|d.t.d.> contiene anche dei murkup di basso livello per le
  definizioni attuali degli ambienti. Infatti, la definizione di nuovi
  teoremi avviene in due passi. Al primo passo, il tag <markup|newtheorem> è
  utilizzato per specificare quale ambiente di tipo teorema dovrà essere
  definito. Al secondo passo (proprio prima che il documento dell'utente
  venga processato) gli ambienti di tipo teorema vengono effettivamente
  definiti. Questo meccanismo rende possibile personalizzare gli ambienti in
  pacchetti che sono processati tra i due passi. Per esempio, la numerazione
  dei teoremi viene personalizzata in questo modo.

  <\warning>
    Attualmente, si dovrebbe usare <markup|newtheorem> e tag simili in un
    file di stile personale o in un pacchetto. Se si utilizza
    <markup|newtheorem> direttamente in un documento, allora la numerazione
    può risultare scorretta a causa dello schema a due passi spiegato sopra.
    Questo inconveniente scomparirà appena sarà possibile specificare dei
    preamboli corretti per i documenti <TeXmacs>.
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
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-manage>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newremark>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newexercise>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newfigure>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-11>>
    </associate>
  </collection>
</auxiliary>
