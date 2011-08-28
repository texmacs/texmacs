<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Ein- und Ausgabe über Pipelines, Grundlagen>

  Konfiguration und Kompilierung eines sehr einfachen Plugins,
  <verbatim|minimal>, findet sich in <hyper-link|Beispiel eines Plugin in
  <name|C++>|../plugin/plugin-binary.de.tm> im Kapitel über
  <hlink|Plugins|../plugin/plugins.de.tm>. \ Wir beginnen mit der Analyse des
  Quellcodes <example-plugin-link|minimal/src/minimal.cpp>. Das
  Hauptprogramm, <verbatim|main>, besteht im wesentlichen aus:

  <\cpp-fragment>
    int

    main () {

    \ \ <em|display-startup-banner>

    \ \ while (true) {

    \ \ \ \ <em|read-input>

    \ \ \ \ <em|display-output>

    \ \ }

    \ \ return 0;

    }
  </cpp-fragment>

  Gemäÿ Vorgabe sendet <TeXmacs> eine Zeichenkette, die mit <verbatim|'\\n'>
  endet an die Anwendung. Dem entsprechend besteht der Code für
  <verbatim|<em|read-input>> aus folgenden Zeilen

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');
  </cpp-fragment>

  Die Ausgabe ist etwas komplizierter, denn <TeXmacs> muss eindeutig
  feststellen können, dass die Ausgabe beendet ist. Das wird erreicht, in dem
  jedwede Ausgabe, in Blockform eingekapselt wird. Das ist im vorliegenden
  Fall ein Begrüÿungstext, banner, und die interaktive Reaktion der
  Anwendung. Die Kapselung sieht folgendermaÿen aus:

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|format>:<em|message><render-key|DATA_END>>>
  </quotation>

  Hier stehen <verbatim|DATA_BEGIN> und <verbatim|DATA_END> für spezielle
  Steuer-Buchstaben:

  <\cpp-fragment>
    #define DATA_BEGIN \ \ ((char) 2)

    #define DATA_END \ \ \ \ ((char) 5)

    #define DATA_ESCAPE \ ((char) 27)
  </cpp-fragment>

  Der Steuer-Buchstabe <verbatim|DATA_ESCAPE> dient zur Maskierung
  <verbatim|DATA_BEGIN> und <verbatim|DATA_END> innerhalb des kommunizierten
  Textes

  <\quotation>
    <\framed-fragment>
      <\with|font-family|tt>
        <tabular|<tformat|<table|<row|<cell|<render-key|DATA_ESCAPE><space|0.6spc><render-key|DATA_BEGIN>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<render-key|DATA_BEGIN>>>|<row|<cell|<render-key|DATA_ESCAPE><space|0.6spc><render-key|DATA_END>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<render-key|DATA_END>>>|<row|<cell|<render-key|DATA_ESCAPE><space|0.6spc><render-key|DATA_ESCAPE>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<render-key|DATA_ESCAPE>>>>>>
      </with>
    </framed-fragment>
  </quotation>

  <verbatim|<em|format>> spezifiziert das Format für den zu übertragenden
  Text, <verbatim|<em|message>>. In unserem Beispiel ist der Code,
  <verbatim|<em|display-startup-banner>,> Übertragung und Darstellung der
  Begrüÿungs-Botschaft in <TeXmacs> folgender

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "Hi there!";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  Entsprechend wurde <verbatim|<em|display-output>> so programmiert

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer;

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  <\remark>
    <TeXmacs> geht davon aus, dass keine Ausgabe mehr kommt, wenn ein
    <render-key|DATA_END> ein <render-key|DATA_BEGIN> abschlieÿt. Deshalb muss jegliche
    Ausgabe in einen <em|einzigen> äuÿeren
    <render-key|DATA_BEGIN>-<render-key|DATA_END>-Block eingeschlossen werden. Es ist
    möglich, solche abgeschlossenen <render-key|DATA_BEGIN>-<render-key|DATA_END> Blöcke in
    einander zu verschachteln. Sie dürfen aber keinesfalls mehr als einen in
    sich geschlossenen Block senden, da <TeXmacs> sofort die Kontrolle
    übernimmt, wenn der äuÿere Block abgeschlossen ist.
  </remark>

  <\remark>
    In unserem Beispiel wurde der <value|cpp> Code der Anwendung in die
    Schnittstelle übernommen. Der übliche Weg, wenn Sie eine
    <TeXmacs>-Schnittstelle für eine bereits existierende Anwendung
    <verbatim|<em|myapp>> schreiben, besteht darin, in der Anwendung eine
    Start-Option <verbatim|--texmacs> zu implementieren. Dann braucht man die
    Verzeichnisse <verbatim|<em|myapp>/src> und <verbatim|<em|myapp>/bin>
    nicht mehr. Es reicht das Plugin zu konfigurieren, indem man z.B. so
    etwas wie die folgenden Zeilen in eine Initialisierungs-Datei
    <verbatim|<em|myapp>/progs/init-<em|myapp>.scm> einfügt:

    <\scheme-fragment>
      (plugin-configure <em|myapp>

      \ \ (:require (url-exists-in-path? "<em|myapp>"))

      \ \ (:launch "<em|myapp> --texmacs")

      \ \ (:session "<em|Myapp>"))
    </scheme-fragment>

    Wenn der Quellcode der Anwendung <verbatim|<em|myapp>> nicht verändert
    werden kann oder darf, dann bleibt immer noch die Möglichkeit, ein
    Ein-/Ausgabe-Filter <verbatim|tm_<em|myapp>> zu schreiben, der die
    Umsetzung vornimmt. Unter den Standard-Plugins im Verzeichnis\ 

    <\verbatim>
      \ \ \ \ plugins
    </verbatim>

    finden Sie mehrere Beispiele.
  </remark>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|german>
  </collection>
</initial>