<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Makro Expansion (1.0.2.3 -- 1.0.2.7)>

  Eine wichtige Änderung betrifft das Datenformat. Makro-Expansionen und
  Funktionen wie

  <\scheme-fragment>
    (expand <em|tag> <em|arg-1> ... <em|arg-n>)
  </scheme-fragment>

  <\scheme-fragment>
    (apply <em|tag> <em|arg-1> ... <em|arg-n>)
  </scheme-fragment>

  ersetzt durch fest kodierte Tags

  <\scheme-fragment>
    (<em|tag> <em|arg-1> ... <em|arg-n>)
  </scheme-fragment>

  Auÿerdem wurden Funktionen systematisch durch Makros ersetzt. Die wenigen
  eingebauten Funktionen wurden mit Hilfe des neuen <markup|xmacro>
  Konstrukts umgeschrieben. Wenn Sie irgendwann eine Funktion geschrieben
  haben, dann werden Sie sie umschreiben müssen.

  Diese neue Vorgehensweise favorisiert eine einheitliche Behandlung von
  Makros und Funktionen. Sie sorgt gleichzeitig dafür, dass die zugehörige
  <name|Scheme> Repräsentation der internen Darstellung entspricht. Immer
  mehr Informationen über Tags werden in <abbr|D.R.D.>s (Data Relation
  Definition) gespeichert. Diese Information dient vor allem dazu
  heuristische Automatiken zu implementieren.

  Beachten Sie, dass es aufgrund dieser Änderungen zu eigenartigen Fehlern
  kommen kann. Bitte machen Sie vorher Sicherungskopien und benachrichtigen
  Sie uns, wenn Sie ungewöhnliches Verhalten bemerken.

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