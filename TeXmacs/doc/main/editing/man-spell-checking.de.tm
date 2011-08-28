<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Rechtschreibprüfung>

  Wenn Sie das Programm <verbatim|ispell> installiert haben, dann können sie
  es zur Rechtschreibprüfung benutzen, indem Sie <shortcut|(spell-start)> oder den
  Menübefehl <menu|Edit|Spell> ausführen. Sie sollten sich aber vergewissern,
  dass die Wörterbücher, der von Ihnen benutzten Sprachen vorhanden sind.

  Wenn Sie die Rechtschreibprüfung gestartet haben, entweder für den gesamten
  Text oder für eine Auswahl, werden Sie bei jedem falsch geschriebenen Wort
  zu einer Aktion aufgefordert. Die möglichen Aktionen finden Sie in der
  Fuÿzeile:

  <\description>
    <item*|a)>Akzeptiert das Wort und jedes weitere Auftreten.

    <item*|r)>Ersetzt das falsch geschriebene Wort durch eine Korrektur, die
    Sie einzugeben haben.

    <item*|i)>Besagt, dass das \RFalsch geschriebene Wort`` korrekt
    geschrieben ist und so in das Wörterbuch aufgenommen werden soll.

    <item*|1-9)>Mehrere Vorschläge, die sie verwenden können.
  </description>

  Beachten Sie, dass <verbatim|ispell> nur nach falsch geschriebenen Worten
  sucht. Es werden keine grammatikalischen Fehler gefunden.

  Wenn Sie die Rechtschreibprüfung starten, wird das Wörterbuch derjenigen
  Sprache verwendet, die an der Cursorposition bzw. am Beginn der Auswahl
  aktiv war. Nur Text in dieser Sprache wird geprüft. Wenn Ihr Text mehrere
  verschiedene Sprachen benutzt, d.h. das Textteile mit Befehlen des Menüs
  <menu|Format|Language> explizit als Text einer bestimmten Sprache
  formatiert wurden, dann müssen Sie die Rechtschreibprüfung für jede dieser
  Sprachen einzeln durchführen.

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