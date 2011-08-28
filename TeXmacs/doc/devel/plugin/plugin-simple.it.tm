<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Un esempio di plugin in <value|scheme>>

  <paragraph*|Il plugin <verbatim|world>>

  Consideriamo il plugin <verbatim|world> nella directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  Questo plugin mostra come estendere <TeXmacs> aggiungendo parti di codice
  scritte in linguaggio <value|scheme> al file

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|world/progs/init-world.scm>
  </verbatim>

  Per testare il plugin <verbatim|world> è necessario copiare ricorsivamente
  la directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/world
  </verbatim>

  in <verbatim|$TEXMACS_PATH/plugins> o in
  <verbatim|$TEXMACS_HOME_PATH/plugins>. Dopo aver rilanciato <TeXmacs> il
  plugin dovrebbe essere automaticamente riconosciuto (un menu
  <apply|menu|World> dovrebbe comparire nella barra dei menu).

  <paragraph*|Come funziona>

  Questo file contiene essenzialmente questa parte di codice:

  <\expand|scheme-fragment>
    (define (world-initialize)

    \ \ (menu-extend texmacs-extra-menu

    \ \ \ \ (=\<gtr\> "World"

    \ \ \ \ \ \ \ \ ("Hello world" (insert-string "Hello world")))))

    \;

    (plugin-configure world

    \ \ (:require #t)

    \ \ (:initialize (world-initialize)))
  </expand>

  Le opzioni di configurazione <expand|scheme-code|:require> specificano le
  condizioni che devono essere soddisfatte per permettere a <TeXmacs> di
  riconoscere il plugin (ad esempio controllando se alcuni programmi sono o
  meno disponibili nel sistema). La configurazione fallisce se le richieste
  formulate non vengono soddisfatte.

  L'opzione <expand|scheme-code|:initialize> specifica quale istruzione deve
  essere eseguita in fase di inizializzazione (modulo soddisfazione delle
  richieste precedenti). Nel nostro esempio abbiamo solamente creato un nuovo
  livello di menu <apply|menu|World> con la voce <apply|menu|World|Hello
  world>, che permette di inserire il testo ``Hello world''. In generale la
  routine di inizializzazione dovrebbe essere molto breve e, se necessario,
  richiamare un ulteriore modulo per l'effettiva inizializzazione del plugin.
  Mantenendo semplice la struttura del file
  <verbatim|init-<em|mioplugin>.scm> verranno ridotti i tempi di avvio di
  <TeXmacs>.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|World>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|World>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|World>|<with|font
      family|<quote|ss>|Hello world>>|<pageref|idx-3>>
    </associate>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|Il plugin
      <with|font family|<quote|tt>|language|<quote|verbatim>|world><value|toc-dots><pageref|toc-1>>

      <with|left margin|<quote|6fn>|font size|<quote|0.84>|Come
      funziona<value|toc-dots><pageref|toc-2>>
    </associate>
  </collection>
</auxiliary>
