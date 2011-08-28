<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Utilizzo base>

  Si può dare inizio ad una sessione dal menu <apply|menu|Insert|Session>. Una
  sessione consiste di una sequenza di ambienti input e output, e tra questi,
  eventualmente, delle parti di testo. Quando si preme il tasto
  <key|return> all'interno di un ambiente di input di una
  sessione, il testo nell'ambiente stesso viene valutato e il risultato viene
  visualizzato in un ambiente di output.

  Quando si inserisce un comando in una sessione, l'applicazione tenta di
  eseguirlo. Molti comandi possono essere lanciati conteporaneamente in uno
  stesso documento, ma l'output sarà attivo solamente nella sessione dove c'è
  il cursore e nella posizione del cursore stesso. Per questo motivo, si
  raccomanda di utilizzare buffer diversi per esecuzioni parallele. Le
  esecuzioni possono essere interrotte dalla barra delle icone. Inoltre è
  possibile disconnettere (chiudere) l'applicazione; in questo caso non
  potranno essere eseguiti altri comandi nella sessione corrispondente.

  Nella barra delle icone propria della sessione sono a disposizione alcuni
  bottoni per selezionare l'input in modalità matematica e per interrompere
  l'esecuzione. Se implementata per il sistema dato, la modalità matematica
  per l'input consente di inserire i dati in entrata in formato grafico
  bidimensionale. \ Altri due bottoni consentono di interrompere l'esecuzione
  di un particolare comando (ma ciò non funziona bene per alcuni sistemi) o
  di disconnettere l'esecuzione del sistema esterno. \ Quando si preme il
  tasto <key|return> nell'input di un sistema non connesso,
  questo verrà automaticamente riconnesso.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Sessione>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
