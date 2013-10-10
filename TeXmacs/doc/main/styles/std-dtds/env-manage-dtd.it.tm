<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Definire nuovi ambienti>

  <tmdtd|env-manage> contiene dei markup di alto livello che possono essere
  utilizzati dall'utente per definire nuovi ambienti per i teoremi, le note,
  gli esercizie e le figure:

  <\explain|<markup|new-theorem>>
    Definisce un ambiente di tipo teorema. Si deve specificare un nome per
    l'ambiente (come ``esperimento'') e il testo corrispondente (come
    ``Esperimento'').
  </explain>

  <\explain|<markup|new-remark>>
    Analogo a <markup|new-theorem>, ma per le note.
  </explain>

  <\explain|<markup|new-exercise>>
    Analogo a <markup|new-theorem>, ma per gli esercizi.
  </explain>

  <\explain|<markup|new-figure>>
    Analogo a <markup|new-theorem>, ma per le figure (per le coppie: grande e
    piccola).
  </explain>

  Il <abbr|d.t.d.> contiene anche dei murkup di basso livello per le
  definizioni attuali degli ambienti. Infatti, la definizione di nuovi
  teoremi avviene in due passi. Al primo passo, il tag <markup|new-theorem> è
  utilizzato per specificare quale ambiente di tipo teorema dovrà essere
  definito. Al secondo passo (proprio prima che il documento dell'utente
  venga processato) gli ambienti di tipo teorema vengono effettivamente
  definiti. Questo meccanismo rende possibile personalizzare gli ambienti in
  pacchetti che sono processati tra i due passi. Per esempio, la numerazione
  dei teoremi viene personalizzata in questo modo.

  <\warning>
    Attualmente, si dovrebbe usare <markup|new-theorem> e tag simili in un
    file di stile personale o in un pacchetto. Se si utilizza
    <markup|new-theorem> direttamente in un documento, allora la numerazione
    può risultare scorretta a causa dello schema a due passi spiegato sopra.
    Questo inconveniente scomparirà appena sarà possibile specificare dei
    preamboli corretti per i documenti <TeXmacs>.
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