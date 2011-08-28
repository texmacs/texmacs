<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Konfiguration der Modifizier-Tasten>

  <TeXmacs> benutzt fünf Tastatur-Modifikatoren: <key|Umschalt>,
  <key|Steuerung>, <key|Alt>, <key|Meta> und <key|Hyper>, die mit \ <prefix|S->,
  <prefix|C->, <prefix|A->, <prefix|M-> und <prefix|M-A-> abgekürzt werden. Die
  <key|Umschalt>- und die <key|Steuerung>-Taste gibt es praktisch auf allen
  Tastaturen, die \ <key|Alt>-Taste auf fast allen. Die meisten heutigen
  Tastaturen haben auch eine \ <key|windows>-Taste, die gewöhnlich die
  Funktion der <key|Meta>-Taste für <TeXmacs> übernimmt.

  Bevor Sie nun Ihre Tastatur neu konfigurieren, sollten Sie überprüfen, ob
  das erforderlich ist. Wenn Sie Tasten haben, die in vernünftigerweise die
  Funktion der Tasten <key|Umschalt>, <key|Steuerung>, <key|Alt> und
  <key|Meta> erfüllen, dann ist es wahrscheinlich besser, alles so zu lassen,
  wie es ist. Eine mögliche Ausnahme ist, das Sie mit einem einfachen
  Tastendruck z.B. auf die <key|Feststelltaste> schnell auf die Eingabe
  mathematischer Formeln umzuschalten. In diesem Fall sollten Sie die
  Funktion der <key|Hyper>-Taste der <key|Feststelltaste> zuweisen.

  Um die Tastaturkonfiguration anzupassen, wählen Sie im Menü
  <menu|Edit|Preferences|Keyboard> die Taste, die Sie umlegen wollen.
  Beispielsweise wird, wenn Sie <menu|Windows key|Map to M modifier> wählen,
  die <key|windows>-Taste der <key|Meta>-Taste entsprechen. Wenn Sie
  <menu|Caps-lock key|Map to H modifier> wählten entspricht die
  <key|Feststelltaste> der <key|Hyper>-Taste.

  Leider erlaubt das X-Window-System nur systemweite Konfigurationen, so dass
  jede Änderung dieser Konfigurationen alle Anwendungen gleichermaÿen
  betrifft. Deshalb sollte man nur solche Tasten umkonfigurieren, die nicht
  von anderen Anwendungen benötigt werden, es kann sonst böse Überraschungen
  geben. Die <key|Windows>-Taste wird selten von anderen Anwendungen benutzt.
  Es schadet gewöhnlich nichts, wenn man diese Taste umkonfiguriert. Man kann
  dazu die <TeXmacs>-Optionen benutzen oder \ auch den
  <verbatim|xmodmap>-Befehl zur Systemkonfiguration. Lesen Sie dazu \Rman
  xmodmap``.\ 

  Manchmal haben Sie bereits Tasten auf Ihrer Tastatur, die in ihrer Funktion
  \ <key|Alt>, <key|Meta> und <key|Hyper> entsprechen, aber nicht gerade so,
  wie Sie es gerne hätten. Dann können Sie die Kurzbefehle <prefix|A->, <prefix|M->
  und <prefix|M-A-> auf andere logische Modifikatoren (Mod1 bis Mod4) mit den
  Befehlen der ersten Gruppe des Menüs \ <menu|Edit|Preferences|Keyboard>
  umlegen. Wenn Sie beispielsweise Emacs Kompatibilität haben wollen, möchten
  Sie vielleicht die \ <key|Meta>- oder die <key|Windows>-Taste mit der
  <key|Alt>-Taste vertauschen ohne irgendwelche systemweite Änderungen. Man
  kann das tun, wenn man herausgefunden hat, welcher Modifikator zu welcher
  Taste gehört. (Meist entspricht \ <key|Mod1> \ <key|Alt> und <key|Mod4>
  \ <key|Meta> oder <key|Windows>). Dann wird die Vertauschung mit den
  Befehlen des Menüs <menu|Edit|Preferences|Keyboard> durchgeführt, indem
  \ man <menu|A modifier|Equivalent for Mod4> und <menu|M modifier|Equivalent
  for Mod1> wählt.

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