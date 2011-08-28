<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <\expand|tmdoc-title>
    Un esempio di plugin in <name|C++>
  </expand>

  <paragraph*|Il plugin <verbatim|minimal>>

  Consideriamo l'esempio del plugin <verbatim|minimal> nella directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  Il plugin è costituito dai seguenti file:

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|minimal/Makefile>

    \ \ \ \ <expand|example-plugin-link|minimal/progs/init-minimal.scm>

    \ \ \ \ <expand|example-plugin-link|minimal/src/minimal.cpp>
  </verbatim>

  Per testare il plugin è necessario copiare ricorsivamente la directory

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/minimal
  </verbatim>

  in <verbatim|$TEXMACS_PATH/progs> o in <verbatim|$TEXMACS_HOME_PATH/progs>.
  Quindi si esegue <verbatim|Makefile> utilizzando

  <\verbatim>
    \ \ \ \ make
  </verbatim>

  in modo da compilare il file <verbatim|minimal.cpp> e creare il file
  binario

  <\verbatim>
    \ \ \ \ minimal/bin/minimal.bin
  </verbatim>

  Dopo aver rilanciato <TeXmacs> il plugin dovrebbe essere automaticamente
  riconosciuto.

  <paragraph*|Come funziona>

  Il plugin <verbatim|minimal> è un esempio di interfaccia minimale tra
  <TeXmacs> e un programma esterno; il programma esterno
  <verbatim|minimal.cpp> viene <apply|hyper-link|spiegato|../interface/interface-pipes.it.tm>
  in dettaglio nel capitolo relativo alla scrittura di interfacce. Il file di
  inizializzazione <verbatim|init-minimal.scm> contiene essenzialmente il
  seguente codice:

  <\expand|scheme-fragment>
    (plugin-configure minimal

    \ \ (:require (url-exists-in-path? "minimal.bin"))

    \ \ (:launch "minimal.bin")

    \ \ (:session "Minimal"))
  </expand>

  L'opzione <expand|scheme-code|:require> controlla se <verbatim|minimal.bin>
  esiste nel path, percui fallirà se si dimentica di eseguire
  <verbatim|Makefile>. L'opzione <expand|scheme-code|:launch> specifica come
  lanciare il programma esterno. L'opzione <verbatim|:session> indica che
  sarà possibile creare una sessione per il plugin <verbatim|minimal>
  utilizzando il menu <apply|menu|Insert|Session|Minimal>.

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
    <associate|toc-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Testo>|<with|font
      family|<quote|ss>|Sessione>|<with|font
      family|<quote|ss>|Minimal>>|<pageref|idx-1>>
    </associate>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|The <with|font
      family|<quote|tt>|language|<quote|verbatim>|minimal>
      plugin<value|toc-dots><pageref|toc-1>>

      <with|left margin|<quote|6fn>|font size|<quote|0.84>|How it
      works<value|toc-dots><pageref|toc-2>>
    </associate>
  </collection>
</auxiliary>
