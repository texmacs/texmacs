<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Scrivere un proprio plugin>

  Per scrivere un proprio plugin, ad esempio <verbatim|<em|mioplugin>>, si
  inizia creando la directory

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|mioplugin>
  </verbatim>

  in cui sistemare tutti i file (si ricorda che <verbatim|$TEXMACS_HOME_PATH>
  per default è <verbatim|$HOME/.TeXmacs>). Opzionalmente è possibile creare
  le seguenti sottodirectory:

  <\expand|description-dash>
    <expand|item*|<verbatim|bin>>per file binari

    <expand|item*|<verbatim|doc>>per la documentazione (non ancora
    supportata).

    <expand|item*|<verbatim|langs>>per il supporto linguistico, come ad
    esempio il dizionario (non ancora supportata).

    <expand|item*|<verbatim|lib>>per le librerie

    <expand|item*|<verbatim|packages>>per i pacchetti di stile

    <expand|item*|<verbatim|progs>>per i programmi in <value|scheme>

    <expand|item*|<verbatim|src>>per i file sorgente

    <expand|item*|<verbatim|styles>>per i file di stile
  </expand>

  L'idea è che i file presenti in queste sottodirectory vengano
  automaticamente riconosciuti all'avvio di <TeXmacs>. Ad esempio se è
  prevista una sottodirectory <verbatim|bin> allora la stringa

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|mioplugin>/bin
  </verbatim>

  verrà automaticamente aggiunta, all'avvio, alla variabile ambientale
  <verbatim|PATH>. Osserviamo come la struttura della sottodirectory di un
  plugin sia molto simile alla struttura della sottodirectory di
  <verbatim|$TEXMACS_PATH>.

  <\example>
    Il tipo più semplice di plugin consiste solo di file di dati, ossia di
    una collezione di file di stile e di pacchetti. Per creare un plugin di
    questo genere è sufficiente creare le sottodirectory:

    <\verbatim>
      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|mioplugin>

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|mioplugin>/styles

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|mioplugin>/packages
    </verbatim>

    e sistemare i file di stile e i pacchetti nelle ultime due
    sottodirectory. Dopo aver rilanciato <TeXmacs> i file di stile e i
    pacchetti appariranno automaticamente nei menu
    <apply|menu|Document|Style> e <apply|menu|Document|Use package>.
  </example>

  Per plugin più complessi, che richiedono l'aggiunta di parti di codice in
  <value|scheme> o in <name|C++>, è necessario scrivere un file di
  configurazione in <value|scheme>

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|mioplugin>/progs/init-<em|mioplugin>.scm
  </verbatim>

  che dovrebbe contenere istruzioni del tipo

  <\expand|scheme-fragment>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>
  </expand>

  dove le opzioni <verbatim|<em|configuration-options>> descrivono le
  principali azioni da eseguire all'inizio, incluso il controllo del
  funzionamento del plugin stesso. Nella sezione seguente descriveremo alcuni
  semplici plugin e la loro configurazione. Molti altri esempi sono contenuti
  nella directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  Alcuni di questi verranno comunque descritti in dettaglio nel capitolo
  relativo alla stesura di nuove interfacce.

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
    <associate|idx-1|<tuple|1|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Stile>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Usa pacchetto>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
