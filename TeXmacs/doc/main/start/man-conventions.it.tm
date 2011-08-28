<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Convenzioni di questo manuale>

  In tutto il manuale di <TeXmacs> le voci dei menu saranno scritte
  utilizzando font di tipo <em|sans serif>, come in <apply|menu|Document>,
  <apply|menu|File|Load> o <apply|menu|Format|Font shape|Italic>. I comandi da
  tastiera saranno scritti con un font di tipo <em|typewriter> all'interno di
  un riquadro, come ad esempio <key|C-s>. A destra delle voci dei menu si
  vedono, se disponibili, le equivalenti combinazioni di tasti. Per tali
  combinazioni si utilizzano le abbreviazioni seguenti:

  <\description>
    <expand|item*|<prefix|S->>Per le combinazioni che includono il tasto shift
    (Maius).

    <expand|item*|<prefix|C->>Per le combinazioni che includono il tasto control
    (Ctrl).

    <expand|item*|<verbatim|><prefix|A->>Per le combinazioni che includono il
    tasto alternate (Alt).

    <expand|item*|<prefix|M->>Per le combinazioni che includono il tasto meta.

    <expand|item*|<prefix|M-A->>Per le combinazioni che includono il tasto hyper.
  </description>

  Per esempio, <shortcut|(make-with font-series bold)> rappresenta <key|A-C-b>.
  Gli spazi all'interno delle abbreviazioni da tastiera indicano che sono
  premuti più tasti, o combinazioni di tasti, in sequenza. Per esempio,
  <key|table N b> rappresenta <prefix|table> <key|N>
  <key|b>.

  I tasti <prefix|A->, <prefix|M-> e
  <prefix|M-A-> non sono disponibili su tutte le tastiere. Sui PC
  recenti, il tasto <prefix|M-> è spesso sostituito con il tasto
  <key|windows>. Nel caso in cui uno o più dei tasti
  modificatori non siano presenti sulla propria tastiera, si può utilizzare
  <key|escape> invece di <prefix|M->, <key|escape
  escape> invece di <prefix|A-> e <prefix|math:greek>,
  <key|escape escape escape> o
  <prefix|A-C-> invece di <prefix|M-A->. Per esempio, <key|escape w> è
  equivalente a <key|A-w>. È anche possibile <apply|hyper-link|configurare i
  tasti modificatori|../config/man-config-kbd-modkeys.it.tm> in modo da
  ottenere il massimo vantaggio dal potente insieme di abbreviazioni da
  tastiera fornito da <TeXmacs>.

  Si noti che i menu e il comportamento della tastiera durante l'uso di
  <TeXmacs> sono <em|contestuali>, cioè essi dipendono dalla modalità
  corrente (modalità testo o ``modalità matematica''), dal linguaggio
  corrente e dalla posizione del cursore nel documento. Ad esempio, in
  modalità matematica, ci sono delle abbreviazioni da tastiera speciali che
  servono per comporre le formule matematiche, ma che sono inutili nella
  modalità testo.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|File>|<with|font
      family|<quote|ss>|Carica>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Forma del carattere>|<with|font
      family|<quote|ss>|Corsivo>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
