<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Opzioni di configurazione per i plugin>

  Come spiegato in precedenza, il file <value|scheme> di configurazione
  <verbatim|<em|mioplugin>/progs/init-<em|mioplugin>.scm>, relativo a un
  plugin con nome <verbatim|<em|mioplugin>>, contiene istruzioni del tipo

  <\expand|scheme-fragment>
    (plugin-configure <em|mioplugin>

    \ \ <em|configuration-options>)
  </expand>

  Di seguito sono elencate le opzioni possibili per
  <verbatim|<em|configuration-options>>:

  <\expand|description-dash>
    <expand|item*|<verbatim|<with|font series|medium|(:require
    <em|condition>)>>>questa opzione serve per specificare una
    <verbatim|<em|condizione>> che deve essere soddisfatta per garantire il
    funzionamento corretto del plugin. Tipicamente si va a verificare se nel
    sistema sono presenti alcuni file binari o alcune librerie necessarie per
    il funzionamento. Se questa condizione fallisce <TeXmacs> continuerà a
    lavorare come se il plugin non esistesse. In questo caso la parte di
    configurazione successiva del plugin non verrà letta. Per questa ragione
    l'opzione <verbatim|:require> viene di norma scritta per prima nella
    lista delle opzioni di configurazione;

    <expand|item*|<verbatim|<with|font series|medium|(:version
    <em|version-cmd>)>>>questa opzione serve per specificare un'espressione
    <value|scheme> <verbatim|<em|version-cmd>> che valuta la versione del
    plugin;

    <expand|item*|<verbatim|<with|font series|medium|(:setup
    <em|cmd>)>>>questo comando viene eseguito solo quando la versione del
    plugin cambia da una esecuzione di <TeXmacs> all'altra. Ciò capita
    principalmente quando viene installata una nuova versione di <TeXmacs> o
    viene aggiunta qualche altra applicazione accessoria;

    <expand|item*|<verbatim|<with|font series|medium|(:initialize
    <em|cmd>)>>>questa espressione esegue l'espressione <value|scheme>
    <verbatim|<em|cmd>>. Tipicamente viene inserita subito dopo l'opzione
    <verbatim|:require> in modo che il plugin venga configurato solamente una
    volta appurata la sua esistenza. Per plugin che richiedono numerose
    istruzioni per essere programmati è importante che il file
    <verbatim|<em|mioplugin>/progs/init-<em|mioplugin>.scm> sia piccolo in
    quanto esso viene eseguito ad ogni avvio di <TeXmacs>. Per ridurre il
    tempo di caricamento di <TeXmacs> la maggior parte dei comandi
    <value|scheme> possono essere inseriti in moduli separati, alcuni dei
    quali possono essere caricati attraverso comandi di inizializzazione;

    <expand|item*|<verbatim|<with|font series|medium|(:launch
    <em|shell-cmd>)>>>questa opzione specifica che il plugin riesce a
    valutare espressioni su una pipe; ciò avviene utilizzando un'applicazione
    accessoria lanciata da riga di comando con il comando
    <verbatim|<em|shell-cmd>>;

    <expand|item*|<verbatim|<with|font series|medium|(:link <em|lib-name>
    <em|export-struct> <em|options>)>>>questa opzione è simile a
    <verbatim|:launch>, salvo che ora l'applicazione esterna viene collegata
    dinamicamente. Per ulteriori informazioni si rimanda alla sezione
    relativa ai <apply|hyper-link|link dinamici|../interface/interface-dynlibs.it.tm>;

    <expand|item*|<verbatim|<with|font series|medium|(:session
    <em|menu-nome>)>>>questa opzione indica che il plugin supporta una
    valutazione per sessioni interattive da shell. Un oggetto de tipo
    <verbatim|<em|menu-oggetto>> verrà inserito nel menu
    <apply|menu|Insert|Session> per lanciare questo tipo di sessioni;

    <expand|item*|<verbatim|<with|font series|medium|(:serializer
    ,<em|fun-nome>)>>>se il plugin si può utilizzare in modalità di
    valutazione allora questa opzione specifica la funzione <value|scheme>
    \ <verbatim|<em|fun-nome>> utilizzata per trasformare gli alberi
    <TeXmacs> in stringhe;

    <expand|item*|<verbatim|<with|font series|medium|(:commander
    ,<em|fun-nome>)>>>questo comando è simile all'opzione
    <verbatim|:serializer> con la differenza che esso viene usato per
    trasformare comandi speciali in stringhe;

    <expand|item*|<verbatim|<with|font series|medium|(:tab-completion
    ,<em|flag>)>>>questo comando indica se il plugin supporta o meno i
    completamenti tramite tabulatore.
  </expand>

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|1|?>>
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
