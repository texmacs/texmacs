<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Anmerkungen für Anwender aus Russland und der Ukraine>

  Um russischen (und ebenso ukrainischen) Text zu schreiben, haben Sie
  mehrere Optionen:

  <\itemize-dot>
    <item>Wählen Sie Russisch als Standardsprache unter
    <menu|Edit|Preferences|Language|Russian>. Wenn <TeXmacs> mit russischen
    Menüs startet, wird dies automatisch getan wenn die russische
    Umgebungsvariable gesetzt ist.

    <item>Wählen Sie Russisch als Sprache für ein Dokument mit
    <menu|Document|Language|Russian>.

    <item>Wählen Sie Russisch als Sprache für einen Textabschnitt in einem
    anderssprachigen Dokument über <menu|Format|Language|Russian>.
  </itemize-dot>

  Wenn Ihr X-Server die xkb-Erweiterung benutzt und eingestellt ist zwischen
  den lateinischen und russischen Tastatur-Modi zu wechseln, müssen Sie
  nichts Weiteres mehr unternehmen. Schalten Sie einfach Ihre Tastatur in den
  russischen Modus um und fahren Sie fort. Alle Software die hierzu benötigt
  wird ist in modernen Linux-Distributionen bereits enthalten und die
  xkb-Erweiterung ist standardmäÿig aktiviert in der <kbd|XF86Config>. Mit
  der xkb-Erweiterung sind die Keysyms 2 Byte groÿ und die russischen
  Buchstaben liegen bei 0x6??. Die Tastatur wird über <kbd|setxkbmap>
  konfiguriert. Wenn X startet, verbindet es dieses Kommando mit der
  systemweiten Xkbmap-Datei (normalerweise finden Sie diese unter
  <kbd|/etc/X11/xinit>) wenn Sie existiert, und mit der Benutzerdatei
  <kbd|~/.Xkbmap>, falls diese existiert. Eine typische <kbd|~/.Xkbmap> sieht
  möglicherweise wie folgt aus:

  <kbd| \ \ \ ru basic grp:shift_toggle>

  Dies bedeutet dass der Tastaturmodus über <key|l-shift r-shift>
  umgeschaltet wird. Andere beliebte Möglichkeiten sind <key|strg shift> oder
  <key|strg alt>, sehen Sie nach in <kbd|/usr/X11R6/lib/X11/xkb/> um weitere
  Informationen zu erhalten. Dies ist das bevorzugte Tastatur-Setup für
  moderne Linux-Systeme wenn Sie beabsichtigen, oft russisch zu schreiben.

  Auf älteren Linux-Systemen ist die xkb-Erweiterung des Öfteren deaktiviert.
  Die Keysyms sind 1-Byte groÿ und werden über <kbd|xmodmap> konfiguriert.
  Beim Start von X wird dieses Kommando mit dem systemweiten <kbd|Xmodmap>
  verbunden (diese finden Sie üblicherweise unter <kbd|/etc/X11/xinit>) wenn
  es existiert; danach mit der Benutzerdatei <kbd|/.Xmodmap> falls diese
  ebenso existiert. Man kann eine Tastaturkombination einrichten um den Modus
  zu wechseln und eine 1-Byte russische Kodierung (wie z.B. koi8-r) im
  russischen Modus zu benutzen. Einfacher geht es wenn Sie das Paket
  <kbd|xruskb> herunterladen und beim Start der X-Session

  <\kbd>
    \ \ \ xrus jcuken-koi8
  </kbd>

  ausführen. Dies setzt das Tastaturlayout auf jcuken (siehe unten) und die
  Kodierung auf koi8-r für den russischen Modus. Wenn Sie solch ein
  Tastaturlayout benutzen, sollten Sie die Option
  <with|mode|math|\<rightarrow\>> international keyboard
  <with|mode|math|\<rightarrow\>> russian <with|mode|math|\<rightarrow\>>
  koi8-r setzen.

  Es ist auÿerdem noch möglich, die Windows cp1251 Kodierung statt koi8-r zu
  benutzen, was aber unter <name|Unix> eher unüblich ist. Wenn Sie <kbd|xrus
  jcuken-cp1251> nutzen, wählen Sie cp1251 statt koi8-r.

  Alle oben beschriebenen Vorgehensweisen erfordern spezielle Aktionen um die
  Tastatur zur russifizieren``. Das ist alles nicht kompliziert, schauen Sie
  sich das Cyrillic-HOWTO an, oder besser noch seine aktualisierte Version

  <\kbd>
    http://www.inp.nsk.su/~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html
  </kbd>

  Alle hier beschrieben Vorgehensweisen wirken sich auf alle X-Anwendungen
  aus: Texteditoren (emacs, nedit, kedit,...), xterms, <TeXmacs> usw..

  Wenn Sie nur einmal oder sehr selten russisch Schreiben möchten, erfordert
  ein komplettes Tastatur-Setup mehr Arbeit als die Sache letztendlich Wert
  ist. Als Vereinfachung für solche Gelegenheitsnutzer bietet <TeXmacs>
  einige Methoden für die Eingabe in Russisch die keine vorhergehende Arbeit
  benötigen. Diese Methoden beeinflussen ausschlieÿliche <TeXmacs> und keine
  anderen Anwendungen.

  Der einfachste Weg um russisch auf einer Standardtastatur ohne
  Software-Setup zu schreiben führt über die Option
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|translit>. Danach
  wird bei Eingabe eines lateinischen Buchstaben der ähnlichste russische
  Buchstabe erzeugt. Um bestimmte russische Buchstaben zu bekommen, muss man
  2- oder 3-Buchstabenkombinationen benutzen:

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Tastenkombination>|<cell|für>|<cell|Tastenkombination>|<cell|für>>|<row|<cell|<kbd-text|"
  e>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<kbd-text|"
  E>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|¼>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|œ>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|j
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|æ>>|<cell|<key|J
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|Æ>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|÷>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|×>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|ø>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|Ø>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|ù>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|Ù>>>|<row|<cell|<key|e
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|ý>>|<cell|<key|E
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|Ý>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|þ>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|Þ>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|ÿ>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|ß>>>>>>|Kyrillischen Text
  auf einer lateinischen Tastatur schreiben>

  Wenn Sie beispielsweise <with|language|russian|font|cyrillic|ñõ>``, und
  nicht <with|language|russian|font|cyrillic|ø>`` möchten, müssen Sie <key|s
  / h> tippen. Selbstverständlich ist die optimale`` Verbindung zwischen
  lateinischen und russischen Buchstaben nicht allgemein gültig. Sie können
  das von <TeXmacs> mitgelieferte Schema untersuchen und, falls Ihnen etwas
  nicht zu sagt, es in der Datei <kbd|~/TeXmacs/progs/my-init-texmacs.scm>
  entsprechend ändern.

  Wenn Sie jcuken statt translit verwenden bekommen Sie das offizielle``
  russische Schreibmaschinenlayout. Es wird so genannt weil die Tasten
  qwertz`` die Buchstabenkombination <with|language|russian|<with|font|cyrillic|éöóêåÿ>>``
  erzeugen. <with|language|german|Diese Eingabemethode ist am sinnvollsten
  wenn sie eine richtige russische Tastatur besitzen, bei der zusätzliche
  russische Buchstaben in Rot im jcuken-Layout auf die Tasten geschrieben
  sind (ein ähnlicher Effekt kann bei einer herkömmlichen deutschen Tastatur
  erzeugt werden in dem man transparente Aufkleber mit den roten russischen
  Buchstaben auf die Tasten klebt). Das ist auÿerdem nützlich wenn Sie ein
  erfahrener russischer Schreiber sind und Ihre Finger bereits an das Layout
  gewöhnt sind.>

  Diejenigen die keine russischen Buchstaben auf ihrer Tastatur dargestellt
  haben, bevorzugen meistens das yawerty Layout, bei dem die Tasten qwertz``
  die Buchstabenkombination <with|language|russian|<with|font|cyrillic|ÿâåðò>>``
  ergeben. Jeder lateinische Buchstabe ist einem gleichwertigen russischen
  Buchstaben zugeordnet; zusätzliche russische Buchstaben werden durch
  <key|shift>-Kombinationen erzeugt. <TeXmacs> kommt mit einem leicht
  modifizierten yawerty Layout welches die Tastenfunktion für <key|$>,
  <key|¿>, <key|<with|mode|math|\<backslash\>>> nicht verändert weil diese
  für <TeXmacs> sehr wichtig sind. Die entsprechenden russischen Buchstaben
  werden stattdessen durch verschiedene <key|shift>-Kombinationen erzeugt.

  <tmdoc-copyright|1998-2004|Joris van der Hoeven, Christoph Strobel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-type|a4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-bot|30mm>
    <associate|page-odd|30mm>
    <associate|language|german>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Bearbeiten>|<with|font-family|<quote|ss>|Einstellungen>|<with|font-family|<quote|ss>|Sprache>|<with|font-family|<quote|ss>|Russisch>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Dokument>|<with|font-family|<quote|ss>|Sprache>|<with|font-family|<quote|ss>|Russisch>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Formate>|<with|font-family|<quote|ss>|Sprache>|<with|font-family|<quote|ss>|Russisch>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>