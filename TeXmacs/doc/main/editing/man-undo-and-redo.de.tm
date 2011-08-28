<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Änderungen zurücknehmen oder wiederholen>

  Man kann die Änderungen, die man in einem Dokument vorgenommen hat,
  schrittweise zurücknehmen und zwar von dem Moment an, an dem Sie <TeXmacs>
  gestartet haben. Man erreicht das mit <menu|Edit|Undo> bzw. <shortcut|(undo 0)>
  oder <key|C-_>. Nach dem Zurücksetzen kann man mit Zurückkehren in den
  ursprünglichen Zustand wieder zurück: \ <menu|Edit|Redo> oder <shortcut|(redo 0)>.

  Die Anzahl gespeicherter Änderungen ist standardmäÿig auf 100 beschränkt.
  Man kann die Zahl aber in der persönlichen Initialisierungs-Datei z.B. auf
  1000 erhöhen, indem man die folgende Zeile einfügt:

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  Wenn Sie eine negative Zahl angeben, gibt es kein obere Schranke auÿer
  Ihrem Speicher.

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