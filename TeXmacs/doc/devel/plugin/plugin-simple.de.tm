<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Beispiel für ein Plugin mit <value|scheme>-Code>

  <paragraph*|Das <verbatim|world> plugin>

  Betrachten wir das <verbatim|world> Plugin im Verzeichnis

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins
  </verbatim>

  Es zeigt wie man <TeXmacs> erweitert mit ein wenig zusätzlichen
  <value|scheme>-Code, den Sie in der Datei

  \ \ <example-plugin-link|world/progs/init-world.scm>

  finden. Um das Plugin zu testen müssen Sie sie das Verzeichnis
  world/progs/init-world.scm rekursiv in das Verzeichnis
  <verbatim|$TEXMACS_PATH/plugins> bzw. <verbatim|$TEXMACS_HOME_PATH/plugins>
  kopieren. Wenn Sie dann <TeXmacs> erneut starten, sollte das Plugin
  automatisch erkannt werden und eine entsprechendes Menü eingerichtet
  werden.

  <paragraph*|Wie es funktioniert.>

  Die Datei <verbatim|init-world.scm> enthält den folgenden Code:

  <\scheme-fragment>
    (define (world-initialize)

    \ \ (menu-extend texmacs-extra-menu

    \ \ \ \ (=\<gtr\> "World"

    \ \ \ \ \ \ \ \ ("Hello world" (insert-string "Hello world")))))

    \;

    (plugin-configure world

    \ \ (:require #t)

    \ \ (:initialize (world-initialize)))
  </scheme-fragment>

  Die Konfigurations-Option <scheme-code|:require> spezifiziert eine
  Bedingung, die erfüllt sein muss, damit das Plugin von <TeXmacs> wird.
  Später werden wir damit überprüfen, ob bestimmte Programme vorhanden sind
  oder nicht. Hier ist sie, da auf wahr gesetzt, praktisch unwirksam. Wenn
  die Bedingung nicht erfüllt wäre, würde die Konfiguration abgebrochen.

  Die Option <scheme-code|:initialize> gibt eine Anweisung, die durchgeführt
  werden soll, wenn die Bedingung erfüllt ist. In unserem Beispiel erzeugen
  wir im Hauptmenü einen Menü-Eintrag <menu|World> und einen Menü-Punkt
  <menu|World|Hello world>, der dazu benutzt werden kann, den Text \RHello
  world'' in das Dokument einzufügen. Im allgemeinen sollte eine solche
  Routine kurz sein und ein Modul laden, das die wirkliche Initialisierung
  durchführt. Das hat den Vorteil, dass kleine
  <verbatim|init-<em|myplugin>.scm> Dateien die Zeit, die <TeXmacs> zum
  Hochfahren braucht, kurz hält.

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
    <associate|preamble|false>
  </collection>
</initial>