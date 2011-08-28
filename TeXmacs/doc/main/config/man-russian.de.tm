<TeXmacs|1.0.7.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Anmerkungen für russische und ukrainische Anwender>

  Um russischen oder ukrainischen Text einzugeben, haben Sie verschiedene
  Optionen:

  <\itemize>
    <item>Wählen Sie Russisch als Ihre Sprache mit dem Menübefehl
    <menu|Edit|Preferences|Language|Russian>. Wenn \ <TeXmacs> mit russischen
    Menüs startete, dann wurde bereits automatisch Russisch oder Ukrainisch
    vorgegeben.

    <item>Wählen Sie Russisch bzw. Ukrainisch als Sprache für das ganze
    Dokument mit dem Menübefehl <menu|Document|Language|Russian> bzw.
    <menu|Document|Language|Ukrainian>.

    <item>Wählen Sie Russisch bzw. Ukrainisch als Sprache für ein Textstück
    \ mit dem Menübefehl <menu|Format|Language|Russian> bzw.
    <menu|Format|Language|Ukrainian>.
  </itemize>

  Wenn Ihr X-Server die xkb-Erweiterung nutzt und zwischen \Rlateinischen''
  und kyrillischen Tastatur-Moden umschalten kann, dann brauchen Sie nichts
  besonderes zu tun. Schalten Sie einfach auf den kyrillischen Tastatur-Modus
  um. Die Software, die man dafür braucht ist in allen modernen
  Linux-Distributionen vorhanden und normalerweise ist die xkb-Erweiterungen
  <with|font-family|tt|XF86Config> standardmäÿig eingestellt. Die Tastatur
  wird mit <with|font-family|tt|setxkbmap> konfiguriert. Wenn X startet führt
  es diesen Befehl mit der systemweiten Xkbmap-Datei aus. Sie befindet sich
  normalerweise in <with|font-family|tt|/etc/X11/xinit>. Anschlieÿend wird
  <with|font-family|tt|setxkbmap> mit \ <with|font-family|tt|~/.Xkbmap>
  ausgeführt, sofern diese existiert. Eine typische
  <with|font-family|tt|~/.Xkbmap> kann so aussehen

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  Das bedeutet, dass der Tastatur-Modus mit <render-key|l-shift r-shift>
  umgeschaltet wird. Andere häufig benutzte Alternativen sind <prefix|C-S->
  oder <key|C- A->. Sehen sie in <with|font-family|tt|/usr/X11R6/lib/X11/xkb/>
  für weitere Einzelheiten. Wenn Sie häufiger russische Texte schreiben
  wollen, dann ist das die bevorzugte Methode auf modernen Linux-Systemen.\ 

  Auf älteren Linux-Systemen ist die xkb-Erweiterung oft inaktiviert. Sie
  werden mit <with|font-family|tt|xmodmap> konfiguriert. Wenn X startet führt
  es den Befehl mit der systemweiten Xmodmap-Datei aus. Sie befindet sich
  normalerweise in <with|font-family|tt|/etc/X11/xinit>. Anschlieÿend wird
  mit <with|font-family|tt|xmodmap> mit <with|font-family|tt|~/.Xmodmap>
  ausgeführt, sofern diese existiert. Sie können die Tastenkombination für
  den Tastatur-Modus mit einem 1-Byte Code (z.B. koi8-r) im Kyrillisch-Modus
  konfigurieren. Einfacher ist es das Paket <with|font-family|tt|xruskb>
  herunterzuladen und zu Beginn der X-Session, den Befehl

  <verbatim| \ \ \ xrus jcuken-koi8>

  zu benutzen. das setzt das Tastatur-Layout auf jcuken (siehe unten) und den
  Tastatur-Code auf koi8-r für den kyrillischen Modus. Auÿerdem sollten Sie
  die Option \ <menu|Edit|Preferences|Keyboard|Cyrillic input method|Koi8-r>
  einstellen.

  Man kann auch die Windows cp1251 Codierung verwenden anstelle von koi8-r,
  obwohl das unter UNIX nur selten geschieht. Wenn Sie
  <with|font-family|tt|xrus jcuken-cp1251> benutzen wollen, wählen Sie
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|Cp1251>.\ 

  Alle diese Methoden benötigen zusätzliche Aktionen, um eine kyrillische
  Tastatur zu erhalten. Das ist nicht schwer. Lesen Sie das Cyrillic-HOWTO
  oder die neue Version

  <verbatim|http://www.inp.nsk.su/<with|font-family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Auÿerdem beeinflussen sie alle X-Anwendungen: Text-Editoren, wie emacs,
  nedit, kedit usw., xterms, <TeXmacs> usw..

  Wenn Sie kyrillische Schriftzeichen nur einmal bzw. sehr selten benötigen,
  kann dieser ganze Aufwand die Sache nicht wert sein. Darum hat <TeXmacs>
  noch eine weitere Möglichkeit, kyrillische Buchstaben einzugeben und die
  ohne jegliche Vorbereitung. Natürlich beeinflusst diese Methode
  ausschlieÿlich <TeXmacs> und keine andere Anwendung. Dazu wählen Sie auf
  einer Standard US-Stil-Tastatur den Menübefehl
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|Translit>. Auÿerdem
  müssen Sie für Ihr Textstück die richtige Schriftart wählen:
  <menu|Format|Font-name|Foreign|Cyrillic>. Die Eingabe eines Buchstabens
  wird dann einen ähnlichen kyrillischen Buchstaben erzeugen. Für einige
  Buchstaben müssen Sie Zwei- oder Drei-Buchstaben-Kombinationen
  verwenden:<vspace|0.5fn>

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Eingabe>|<cell|für>|<cell|Eingabe(n)>|<cell|für>>|<row|<cell|<key|text
  " e>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|accent:umlaut
  E>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|j
  var>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|J
  var>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|÷>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|×>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|ø>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|Ø>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|ù>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|Ù>>>|<row|<cell|<key|e
  var>>|<cell|<with|language|russian|font|cyrillic|ý>>|<cell|<key|E
  var>>|<cell|<with|language|russian|font|cyrillic|Ý>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|þ>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|Þ>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|ÿ>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|ß>>>>>>|Kyrillische
  Buchstaben mit \Rlateinischer`` Tastatur.>

  Die Verwendung von Mehrfach-Kombinationen führt manchmal zu unerwünschten
  Ergebnissen. Versuchen Sie es dann mit <key|/> als Trennzeichen. Wenn Sie
  <key|s h> eingeben, erhalten Sie \R<with|language|russian|font|cyrillic|ø>'';
  um das gewünschte \R<with|language|russian|font|cyrillic|ñõ>'' zu erhalten,
  müssen Sie \ <key|s / h> eingeben. Es gibt keine eindeutige Weise
  lateinische Buchstaben kyrillischen Zuzuordnen. Schauen Sie sich deshalb
  die mitgelieferten Tastatur-Dateien an und, wenn Sie ihnen nicht gefallen,
  passen Sie sie in Ihrer Initialisierungs-Datei
  <with|font-family|tt|~/.TeXmacs/progs/my-init-texmacs.scm> Ihren Wünschen
  entsprechend an.

  Wenn Sie \Rjcuken`` anstelle von \Rtranslit`` wählen, erhalten Sie das
  offizielle russische Tastatur-Layout. Die Tasten der oberen Reihe
  \Rqwerty'' erzeugen dann \R<with|language|russian|<with|font|cyrillic|éöóêåí>>``.
  Diese Methode ist dann nützlich, wenn Sie eine russische Tastatur mit der
  richtigen Beschriftung besitzen oder eine andere passend beschriften.
  Vielleicht können Sie ja auch kyrillisch Blindschreiben.

  Diejenigen, die keine russische Tastatur besitzen, bevorzugen oft das
  \Ryawerty``-Layout, bei dem die Tasten \Rqwerty'' die Ausgabe
  \R<with|language|russian|font|cyrillic|ÿâåðòû>`` und zusätzliche
  kyrillische Schriftzeichen mit der Umschalt-Taste, <prefix|S->, erzeugt
  werden können. <TeXmacs> hat ein etwas modifizierte \Ryawerty\R-Layout,
  denn es ändert nicht die Tasten <key|$>, <render-key|¿>, <key|\\>, da diese
  für <TeXmacs> wichtig sind sind und eine spezielle Bedeutung haben. Die
  dazugehörigen kyrillischen Buchstaben können durch bestimmte Kombinationen
  mit der Umschalt-Taste erzeugt werden.

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