<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Ein Beispiel mit <name|C++> Code>

  <paragraph*|Das <verbatim|minimal> plugin>

  betrachten wir das Beispiel <verbatim|minimal> im Verzeichnis

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/minimal
  </verbatim>

  Es besteht aus den Dateien:

  <\verbatim>
    \ \ \ \ <example-plugin-link|minimal/Makefile>

    \ \ \ \ <example-plugin-link|minimal/progs/init-minimal.scm>

    \ \ \ \ <example-plugin-link|minimal/src/minimal.cpp>
  </verbatim>

  Um das Plugin auszuprobieren müssen Sie das Verzeichnis

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins/minimal
  </verbatim>

  rekursiv in das Verzeichnis <verbatim|$TEXMACS_PATH/progs> kopieren oder in
  das Verzeichnis <verbatim|$TEXMACS_HOME_PATH/progs>. Danach müssen Sie in
  dem Verzeichnis minimal den Befehl

  <\verbatim>
    \ \ \ \ make
  </verbatim>

  ausführen und so den Programmcode <verbatim|minimal.cpp> zu einer
  Binärdatei kompilieren

  <\verbatim>
    \ \ \ \ minimal/bin/minimal.bin
  </verbatim>

  Wenn Sie jetzt <TeXmacs> neu starten sollte das Plugin automatisch erkannt
  werden.

  <paragraph*|Wie es funktioniert.>

  Das <verbatim|minimal> Plugin demonstriert eine Minimal-Schnittstelle
  zwischen <TeXmacs> und einem externen Programm. Dieses Programm und sein
  Quellcode in <verbatim|minimal.cpp> wird eingehender im Kapitel über
  <hyper-link|Schittstellen|../interface/interface-pipes.de.tm> erklärt. Die
  Initialisierungs-Datei <verbatim|init-minimal.scm> enthält den folgenden
  Code:

  <\scheme-fragment>
    (plugin-configure minimal

    \ \ (:require (url-exists-in-path? "minimal.bin"))

    \ \ (:launch "minimal.bin")

    \ \ (:session "Minimal"))
  </scheme-fragment>

  Die <scheme-code|:require> Option prüft, ob <verbatim|minimal.bin> im
  Suchpfad gefunden werden kann. Deshalb wird die Initialisierungs-Routine
  abgebrochen, wenn Sie vergessen haben sollten, das Plugin zu kompilieren.
  Die <scheme-code|:launch> Option erklärt, wie das externe Programm
  aufzurufen ist. Die <verbatim|:session> Option sorgt dafür, dass der Befehl
  <menu|Insert|Session|Minimal> bereitsteht, um eine \ <verbatim|minimal>
  Sitzung zu erzeugen.

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