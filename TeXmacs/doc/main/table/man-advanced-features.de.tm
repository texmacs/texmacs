<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Erweiterte Tabellen-Eigenschaften>

  In den Menüs finden Sie einige speziellere Tabellen-Funktionen. U.a. sind
  dies folgende:

  <\itemize>
    <item>Sie können eine Zelle vergröÿern. Diese hat dann die Höhe und
    Breite mehrerer Zellen (rechts bzw. unten von der Ursprungszelle).
    Menübefehl: <menu|Table|Special cell properties|Set span>.

    <item>Einfügung von ganzen Unter-Tabellen in einzelnen Zellen.
    Menübefehl: <menu|Table|Special cell properties|Subtable>.

    <item>Korrektur der vertikalen und horizontalen Ausrichtung von Text,
    damit die Basislinien übereinstimmen. Menübefehl: <menu|Table|Special
    cell properties|Text height correction>.

    <item>Horizontaler Umbruch des Zelleninhalts. Menübefehl:
    <menu|Table|Special cell properties|Hyphenation>.

    <item>Vertikaler Seiten-Umbruch von ganzen Tabellen.

    <item>Mehrere Spalten und/oder Zeilen können verbunden werden. Die
    verbundenen Zellen werden Teil des sichtbaren Randes der verbleibenden
    Zellen. Menübefehl: <menu|Table|Special cell properties|Glue
    decorations>.

    <item>Desaktivierung der Tabelle, um ihren Quellcode sehen zu können.
    Menübefehl: <menu|Table|Special table properties|Deactivate>.

    <item>Eine Referenzzelle definieren. Danach werden alle neu geschaffenen
    Nachbarzellen gleichartig formatiert. Menübefehl: <menu|Table|Special
    table properties|Set extension center>.

    <item>Minimale und maximale Tabellen-Gröÿe in Zellzahlen angeben. Dies
    wird beim Editieren berücksichtigt und ist vor allem bei der Definition
    von Makros wichtig. Menübefehl: <menu|Table|Special table properties|Size
    limits>.
  </itemize>

  Derzeit sind alle Tabellen Inhalte eines Kontexts: <markup|tabular>,
  <markup|block>, <markup|matrix>, usw.. Wenn Sie eigenen Makros schreiben
  wollen, können Sie mit dem Befehl <menu|Table|Special table
  properties|Extract format> das formatierende Konstrukt einer existierenden
  Tabelle aus seinem Kontext extrahieren.

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