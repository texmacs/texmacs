<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Configurazione dei tasti modificatori>

  <apply|TeXmacs> utilizza cinque principali modificatori da tastiera:
  <prefix|S->, <prefix|C->,
  <prefix|A->, <prefix|M-> e
  <prefix|M-A->, che vengono abbreviati rispettivamente con
  <prefix|S->, <prefix|C->, <prefix|A->, <prefix|M-> e <prefix|M-A->. I tasti
  <prefix|S-> e \ <prefix|C-> si trovano su tutte
  le tastiere; il tasto \ <prefix|A-> su quasi tutte. Molte
  tastiere moderne sono dotate anche del tasto <key|windows>
  che, in <TeXmacs>, è equivalente a <prefix|M->.

  Prima di riconfigurare la tastiera è consigliabile verificare se ciò sia
  veramente necessario. Se sulla vostra tastiera ci sono i tasti
  corrispondenti a <prefix|S->, <prefix|C->,
  <prefix|A-> e <prefix|M->, probabilmente non
  saranno necessarie modifiche di configurazione. Una possibile eccezione è
  che desideriate utilizzare il tasto <key|capslock> per
  scrivere simboli matematici. In questo caso dovrete far corrispondere il
  tasto <key|capslock> al tasto <prefix|M-A->.

  Per riconfigurare la tastiera è sufficiente selezionare, nel menu
  <apply|menu|Edit|Preferences|Keyboard>, il modificatore logico che
  desiderate far corrispondere ad un dato tasto fisico. Ad esempio,
  selezionando <apply|menu|Windows key|Map to M modifier>, il tasto
  <key|windows> verrà fatto corrispondere al modificatore
  <prefix|M->. In modo del tutto analogo, selezionando
  <apply|menu|Caps-lock key|Map to H modifier>, il tasto
  <key|capslock> verrà fatto corrispondere al modificatore
  <prefix|M-A->.

  Sfortunatamente il sistema X Window permette di effettuare solo
  riconfigurazioni globali. Per questo, se in <TeXmacs> viene riconfigurato
  il tasto <key|capslock>, il nuovo comportamento di questo
  tasto interesserà anche tutte le altre applicazioni in cui questo stesso
  tasto viene utilizzato. Perciò è consigliabile riconfigurare solo i tasti
  che non vengono utilizzati per scopi diversi in altre applicazioni. Ad
  esempio, il tasto <key|windows> non viene solitamente
  utilizzato in molte altre applicazioni, percui la sua riconfigurazione non
  ha ripercussioni. Alcuni utilizzatori potrebbero decidere di effettuare una
  opportuna riconfigurazione globale della tastiera. Ciò può essere fatto
  ricorrendo al comando <verbatim|xmodmap> per informazioni sul quale si
  rimanda alle corrispondenti pagine del manuale.

  In alcuni casi può accadere che i tasti corrispondenti a
  <prefix|A->, <prefix|M-> e
  <prefix|M-A->, siano presenti sulla tastiera ma non funzionino
  nel modo desiderato. Per farli funzionare adeguatamente è sufficiente
  rimappare i prefissi <prefix|A->, <prefix|M-> e <prefix|M-A-> in altri modificatori
  logici appartenenti al primo gruppo di sottomenu in
  <apply|menu|Edit|Preferences|Keyboard>.

  Ad esempio, per avere compatibilità con Emacs, potreste decidere di
  scambiare il tasto <prefix|M-> o <key|windows> con
  il tasto <prefix|A-> senza tuttavia eseguire una modifica
  globale. Ciò può essere fatto cercando i modificatori corrispondenti a
  questo tasto; tipicamente avremo <key|Mod1> al posto di
  <prefix|A-> e <key|Mod4> al posto di
  <prefix|M-> o <key|windows>. Quindi eseguiremo le
  dovute permutazioni nel menu <apply|menu|Edit|Preferences|Keyboard>,
  selezionando <apply|menu|A modifier|Equivalent for Mod4> e <apply|menu|M
  modifier|Equivalent for Mod1>.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Tastiera>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tasto Window>|<with|font
      family|<quote|ss>|Map to M modifier>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Caps-lock key>|<with|font
      family|<quote|ss>|Map to H modifier>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Tastiera>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Tastiera>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|A modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod4>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|M modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod1>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
