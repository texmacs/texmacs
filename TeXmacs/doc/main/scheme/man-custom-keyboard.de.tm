<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Eigene Kurzbefehle erzeugen>

  Tastenzuordnungen, keymaps, können mit dem folgenden Befehl festgelegt
  werden\ 

  <\verbatim>
    \ \ \ \ (kbd-map predicate . keymaps)
  </verbatim>

  Das Prädikat, predicate, spezifiziert unter welchen Umständen die
  \RZuordnung`` gelten soll. Beispiele sind: <verbatim|always?>,
  <verbatim|in-math?> und <verbatim|in-french?>. Der Anwender kann aber seine
  eigenen Prädikate definieren. Jeder Eintrag in <verbatim|keymaps> hat eine
  der folgenden Formen:

  <\verbatim>
    \ \ \ \ (key-combination action_1 ... action_n)<next-line>
    \ \ \ (key-combination result)<next-line> \ \ \ (key-combination result
    help-message)
  </verbatim>

  Im ersten Fall sind die Aktionen <verbatim|action_i> <scheme>-Befehle, die
  mit der Zeichenkette <verbatim|key-combination> assoziiert werden. Im
  zweiten und dritten Fall ist <verbatim|result> eine Zeichenkette, die in
  den Text eingefügt wird, wenn die <verbatim|key-combination> beendet ist.
  Im dritten Fall wird eine zusätzliche optionale Nachricht
  <verbatim|help-message> angezeigt werden.

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