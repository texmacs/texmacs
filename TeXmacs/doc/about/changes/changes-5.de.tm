<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Tastatur (1.0.0.11 -- 1.0.1)>

  Die <TeXmacs> Tasten-Zuordnungen wurden sinnvoller gestaltet. Hier eine
  Liste de wichtigsten Änderungen:

  <\itemize>
    <item>Der <key|E->-Präfix wurde in <prefix|M-> umbenannt.

    <item><key|escape> ist äquivalent zu <prefix|M-> und
    <key|escape>-<key|escape> zu <prefix|A->.

    <item>Modus-abhängige Kurzbefehle werden jetzt mit <prefix|A-> eingeleitet.
    Insbesondere benutzen Akzente jetzt <prefix|A-> anstatt <key|E->.

    <item>Varianten erhält man mit <key|tab> anstelle von
    <key|*> und man kann zurückkehren mit
    <key|S-tab>.

    <item>Griechische Buchstaben erhält man mit <prefix|A-C->, <prefix|math:greek>, oder
    Hyper <prefix|M-A->, der in <menu|Edit|Preferences> konfiguriert werden kann.
    Im Mathematik-Modus kann man griechische Buchstaben auch als Variante von
    lateinischen Buchstaben erhalten, z.B. liefert <key|p tab>
    \ <with|mode|math|\<pi\>>.

    <item>Die Bedeutung der Cursor-Tasten zusammen mit <key|Strg>, <key|Alt>
    und <key|M> hat sich geändert.
  </itemize>

  Sie können im Menü <menu|Edit|Preferences|Look and feel> zwischen mehreren
  Arten von Aussehen und Verhalten wählen. Die Vorgabe ist
  <samp|<localize|Emacs>>, aber Sie können auch das frühere Verhalten,
  <localize|old style>, wählen.\ 

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