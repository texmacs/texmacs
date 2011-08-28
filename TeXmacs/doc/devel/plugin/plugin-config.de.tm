<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Zusammenfassung der Konfigurations-Optionen für Plugins>

  Wie bereits gesagt wurde, sollte die <value|scheme>-Konfigurations-Datei
  <verbatim|<em|myplugin>/progs/init-<em|myplugin>.scm> eines Plugins mit dem
  Namen <verbatim|<em|plugin>> eine Anweisung der Form

  <\scheme-fragment>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </scheme-fragment>

  enthalten. Hier folgt eine Liste von vorhandenen
  <verbatim|<em|configuration-options>>:

  <\description-dash>
    <item*|<verbatim|<with|font-series|medium|(:require
    <em|condition>)>>>Diese Option spezifiziert eine Bedingung
    <verbatim|<em|condition>>, die erfüllt sein muss, damit das Plugin
    funktioniert. Normalerweise wird überprüft, ob bestimmte Dateien
    vorhanden sind. Wenn die Bedingung nicht wahr ist, dann wird <TeXmacs>so
    weitermachen, als ob das Plugin gar nicht existiert. Die Konfiguration
    wird abgebrochen. Die <verbatim|:require> Option ist normalerweise die
    allererste.

    <item*|<verbatim|<with|font-series|medium|(:version
    <em|version-cmd>)>>>Diese Option führt einen <value|scheme>-Ausdruck
    <verbatim|<em|version-cmd>> \ aus, der zu der Version des Plugins
    evaluiert.

    <item*|<verbatim|<with|font-series|medium|(:setup <em|cmd>)>>>Diese
    Anweisung wird nur ausgeführt, wenn die Version des Plugins sich seit dem
    letzten Aufruf von <TeXmacs> geändert hat. Das passiert eigentlich nur,
    wenn Sie neue Versionen von <TeXmacs> oder Hilfsanwendungen neu
    installiert haben..

    <item*|<verbatim|<with|font-series|medium|(:initialize <em|cmd>)>>>Diese
    Option führt den <value|scheme>-Ausdruck <verbatim|<em|cmd>> aus. Er
    folgt normalerweise unmittelbar auf <verbatim|:require> option, so dass
    das Plugin nur konfiguriert wird, wenn es auch existiert. Auch groÿe
    Plugins sollten eine kleine \ <verbatim|<em|myplugin>/progs/init-<em|myplugin>.scm>
    Datei haben, damit der <TeXmacs>-Start nicht zu sehr verzögert wird, denn
    diese Routine muss bei jedem Start abgearbeitet werden. darum sollte der
    Groÿteil der <value|scheme>-Anweisungen in einer eigenen Datei
    untergebracht werden, die von dem Initialisierungs-Befehl geladen wird.

    <item*|<verbatim|<with|font-series|medium|(:launch
    <em|shell-cmd>)>>>Diese Option erklärt, dass das Plugin Ausdrücke über
    eine Pipeline evaluieren kann, die eine Hilfsanwendung benutzt, welche
    mit dem System-Befehl \ <verbatim|<em|shell-cmd>> gestartet wird..

    <item*|<verbatim|<with|font-series|medium|(:link <em|lib-name>
    <em|export-struct> <em|options>)>>>Diese Option entspricht weitgehend
    :launch, nur wird die externe Anwendung damit dynamisch eingebunden.
    Weitere Informationen finden Sie im Abschnitt <hyper-link|Dynamisch
    ladbare Bibliotheken|../interface/interface-dynlibs.ed.tm>.

    <item*|<verbatim|<with|font-series|medium|(:session
    <em|menu-name>)>>>Diese Option erklärt, dass das Plugin ein Anwendung
    unterstützt, die in interaktiven Sitzungen Eingaben evaluieren kann. Ein
    Menü-Punkt <verbatim|<em|menu-item>> wird in das Menü
    <menu|Insert|Session> eingetragen, um damit solche Sitzungen zu starten.

    <item*|<verbatim|<with|font-series|medium|(:serializer
    ,<em|fun-name>)>>>Wenn das Plugin zur Evaluierung taugt, dann dient diese
    Option dazu die <value|scheme>-Funktion <verbatim|<em|fun-name>> zu
    benennen, die \ <TeXmacs>-Bäume ind Zeichenketten umformt.

    <item*|<verbatim|<with|font-series|medium|(:commander
    ,<em|fun-name>)>>>Diese Anweisung entspricht weitgehend
    <verbatim|:serializer>, nur dass :commander benutzt wird, um spezielle
    Befehle in Zeichenketten umzuwandeln .

    <item*|<verbatim|<with|font-series|medium|(:tab-completion #t)>>>Diese
    Anweisung erklärt, dass das Plugin Text-Ergänzungen unterstützt.

    <item*|<verbatim|<with|font-series|medium|(:test-input-done #t)>>>Diese
    Anweisung erklärt, dass das Plugin ein Routine hat, die testen kann, ob
    die Eingabe vollständig ist.
  </description-dash>

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