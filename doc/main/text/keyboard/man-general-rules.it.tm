<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Regole generali sui prefissi>

  Poiché ci sono molte scorciatoie da tastiera, è importante avere qualche
  indicazione per classificarle in diverse categorie, per poterle memorizzare
  più facilmente. Come regola generale, le scorciatoie da tastiera che
  appartengono alla stessa categoria sono identificate da un prefisso comune.
  I più comuni di tali prefissi sono:

  <\description>
    <expand|item*|<key|C-<with|mode|math|x>>>Le scorciatoie basate sul tasto
    control vengono utilizzate per i comandi di redazione usati
    frequentemente. Essi dipendono molto dalla ``apparenza'' definita in
    <apply|menu|Edit|Preferences>. Per esempio, se si utilizza un aspetto
    compatibile con <name|Emacs>, allora le combinazioni di tasti della forma
    <key|C-<with|mode|math|x>> corrispondono ai comandi <name|Emacs>, come
    <key|C-y> per copiare parti di un testo.

    <expand|item*|<key|A-<with|mode|math|x>>>Il tasto alternate è utilizzato
    per i comandi che dipendono dalla modalità in cui si sta lavorando. Per
    esempio, <expand|kbd-text|s> produce del testo <strong|enfatizzato> in
    modalità testo e una radice quadrata <with|mode|math|<sqrt|>> in modalità
    matematica. Si noti che che <key|<expand|key-escape> <expand|key-escape>>
    è equivalente a <key|A->.

    <expand|item*|<key|M-<with|mode|math|x>>>Il tasto meta è utilizzato per
    comandi <TeXmacs> generici, che possono essere usati in tutte le
    modalità. Per esempio, <expand|kbd-gen|!> produce un'etichetta. Esso
    viene anche utilizzato per comandi addizionali di redazione, come
    <key|A-w> per copiare del testo se si usa l'aspetto <name|Emacs>. Si noti
    che <key|<expand|key-escape>> è equivalente a <key|M->.

    <expand|item*|<key|H-<with|mode|math|x>>>Il tasto hyper è utilizzato per
    produrre simboli speciali come i caratteri greci in modalità matematica.
    Si può configurare la propria tastiera in modo tale che il tasto Maius
    svolga il ruolo del tasto hyper. Il tasto <key|F5> è equivalente a
    <key|H->.
  </description>

  Ricordiamo che i particolari tasti modificatori, che sono utilizzati per
  ottenere i prefissi <key|M-> e <key|H->, possono essere
  <apply|hyper-link|configurati|../../config/man-config-kbd-modkeys.it.tm> in
  <apply|menu|Edit|Preferences>.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
