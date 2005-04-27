<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Konfiguration der Tastatur-Modifier >

  <TeXmacs> benutzt fünf Tastatur-Modifier: \ <key|<key-shift>>,
  \ \ <key|strg>, \ <key|alt>, \ <key|meta> und \ <key|hyper> die als
  \ <key|S->, \ <key|C->, <key|A->, <key|M-> und \ <key|H-> abgekürzt werden.
  Die Tasten \ <key|<key-shift>> und \ <key|strg> sind normalerweise \ auf
  praktisch allen Tastaturen vorhanden, die \ <key|alt> Taste auf den
  Meisten. Neuere Tastaturen für PC's haben üblicherweise noch die
  \ <key|windows> taste, diese ist in <TeXmacs> \ äquivalent zu der Taste
  <key|meta>.

  Bevor Sie die Konfiguration Ihrer Tastatur verändern, sollten Sie prüfen ob
  das auch wirklich nötig ist. Wenn Sie Tasten entsprechend zu \ <key|shift>,
  \ <key|strg>, \ <key|alt> und \ <key|meta> auf Ihrer Tastatur haben,
  sollten Sie nichts ändern. Möchten Sie jedoch einen einfachen Tastendruck
  wie <key|feststellen> verwenden um beispielsweise mathematische Symbole zu
  schreiben, können Sie es natürlich trotzdem tun. In diesem Fall sollten Sie
  <key|hyper> der Taste <key|feststellen> zuweisen.

  Um die Konfiguration der Tastatur zu verändern, wählen Sie einfach einen
  der Tastatur-Modifier über <specific|texmacs|><menu|Edit|Preferences|Keyboard>,
  den Sie einer vorhandenen Taste zuweisen möchten. Wenn Sie beispielsweise
  <menu|Windows key|Legen auf M Modifier> wählen, wird die <key|windows>
  Taste dem <key|meta> Modifier zugewiesen. Ebenso wird <key|feststellen> dem
  <key|hyper> Modifier zugewiesen, wenn Sie <menu|Caps-lock key|Legen auf H
  Modifier> aus dem Menü wählen.

  Unglücklicherweise erlaubt X-Window nur eine systemweite Konfiguration.
  Deshalb hat es Auswirkungen auf alle anderen Anwendungen wenn Sie in
  <TeXmacs> die Konfiguration der <key|feststellen> Taste verändern da
  <key|feststellen> dann dort ebenfalls die neue Funktion hat. Aus diesem
  Grund ist es wichtig, nur die Tasten zu ändern, die nicht für etwas Anderes
  in den übrigen Anwendungen benötigt werden. Die <key|windows> Taste
  beispielsweise wird nicht von vielen anderen Applikationen benutzt, deshalb
  ist es normalerweise ungefährlich die Konfiguration für diese Taste zu
  verändern. Möglicherweise bevorzugen Sie es eine entsprechende systemweite
  Rekonfiguration vorzunehmen. Dies können Sie mit dem Kommando <kbd|xmodmap>
  erreichen; lesen Sie hierzu die zugehörige Manual-Page für weitere
  Informationen.

  In manchen Fällen werden Sie schon die entsprechenden Tasten zu <key|alt>,
  <key|meta> und <key|hyper> auf Ihrer Tastatur haben, was aber eventuell
  nicht Ihren Vorstellungen entsprechen könnte. Um dies zu ändern können Sie
  die <key|A->, <key|M-> und <key|H-> Präfixe über die erste Gruppe von
  Untermenüs in <menu|Edit|Preferences|Keyboard> anderen Modifiern zuweisen.

  Für die Kompatibilität zu Emacs möchten Sie vielleicht die <key|meta> oder
  <key|windows> Tasten mit <key|alt> vertauschen ohne systemweite Änderungen
  hervorzurufen. Um dies zu erreichen, müssen Sie herausfinden welche
  Modifier diesen Tasten entsprechen, normalerweise ist das <key|Mod1> für
  <key|alt> und <key|Mod4> für <key|meta> oder <key|windows>. Das Vertauschen
  wird schlieÿlich über <menu|Edit|Preferences|Keyboard>, durch auswählen von
  <menu|A modifier|Äquivalent für Mod4> und <menu|M Modifier|Äquivalent für
  Mod1> durchgeführt.

  <tmdoc-copyright|1998-2003|Joris van der Hoeven, Christoph Strobel>

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
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>