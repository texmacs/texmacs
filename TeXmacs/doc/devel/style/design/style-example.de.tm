<TeXmacs|1.0.4.5>

<style|<tuple|tmdoc|>>

<\body>
  <tmdoc-title|Ein einfaches Stil-Paket schreiben>

  Lassen Sie uns an einem einfachen Beispiel erklären, wie man ein einfaches
  Stil-Paket schreibt.

  Wenn Sie das Beispiel direkt am Rechner nachvollziehen wollen, erleichtern
  Sie sich die Arbeit, wenn Sie ein zweites <TeXmacs> parallel eventuell in
  einem anderen virtuellen Fenster starten und zwischen den beiden Instanzen
  wechseln.

  Zuerst einmal müssen Sie einen neuen Puffer, d.h. eine neue leere
  Textdatei, erzeugen. Dazu wählen Sie das Menü \ <menu|Datei|Neu> und wählen
  den <tmstyle|Quellcode> Basis-Stil unter
  <menu|Dokument|Basis-Stil|Quellcode>-Menü. Dann speichern Sie mit einem
  aussagekräftigen Namen und der Datei-Ergänzung <verbatim|.ts> in Ihr
  Paket-Verzeichnis:

  <verbatim| \ \ \ $HOME/.TeXmacs/packages>

  Beachten Sie bitte, dass der Knopf <em|<menu|Texte>> im Datei-Browser dem
  Verzeichnis\ 

  <verbatim| \ \ \ $HOME/.TeXmacs/texts>

  entspricht. Daher können Sie durch Doppelklick auf \ .. und danach auf
  <verbatim|packages> schnell in dieses Verzeichnis wechseln. Ganz
  entsprechend enthält das Verzeichnis

  <verbatim| \ \ \ $HOME/.TeXmacs/styles>

  Ihre persönlichen Basis-Stil-Dateien. Nach dem Sichern mit der
  Dateiergänzung <verbatim|.ts> sollte das leere Stil-Paket automatisch in
  dem Menü <menu|Dokument|Paket einfügen > erscheinen. Wenn Sie ein
  Unterverzeichnis im Verzeichnis <verbatim|$HOME/.TeXmacs/packages>
  erstellen, erzeugen Sie automatisch ein neues Untermenü und wenn Sie da
  hinein ein Stilpaket speichern einen neuen Menüpunkt in dem entsprechenden
  Untermenü.

  Lassen Sie uns nun ein einfaches Makro <markup|hi> erzeugen, das \RHello
  world'' auf dem Bildschirm ausgibt. Zuerst tippen Sie <shortcut|(make 'assign)> oder
  <key|inactive =>, um eine Zuordnung, engl. \Rassignment'', zu erzeugen. Sie
  sollten nun auf dem Bildschirm Folgendes sehen

  <\tm-fragment>
    <inactive*|<assign||>>
  </tm-fragment>

  Geben Sie nun ``hi'' als erstes Argument ein, gehen zum zweiten Argument
  und tippen <shortcut|(make 'macro)> oder <key|inactive m> um ein Makro einzufügen. Jetzt sollte
  es so aussehen:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|>>>
  </tm-fragment>

  Schlieÿlich schreiben Sie \RHello world'' in den Rumpf des Makros. Ihr
  Dokument sollte nun aus folgender Zeile bestehen:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world>>>
  </tm-fragment>

  Nachdem Sie Ihr Stil-Paket unter einem Namen gespeichert haben, können Sie
  das Makro verwenden, z.B., indem Sie ein neues Dokument erstellen und es
  mit Ihrem Stil-Paket mit <menu|Dokument|Paket hinzufügen> verbinden. Sie
  benutzen das Makro <markup|hi> durch Eintippen von \ <key|\\ h i> mit
  nachfolgendem Drücken der Eingabetaste, <shortcut|(kbd-return)>.

  Analog können Sie Makros mit Argumenten erzeugen, die Sie zur Laufzeit
  eingeben und im Makro auswerten können. Wenn Sie z.B. in gleicher Weise ein
  Makro <markup|hello> erzeugt haben, können Sie mit der Tastenkombination
  <shortcut|(structured-insert-left)> oder <key|inactive links> im Makrorumpf ein zusätzliches Argument
  auf der linken Seite des Cursors einfügen. \Rlinks`` steht dabei für die
  linke Pfeiltaste. Nachdem Sie mit dem Cursor im Makrorumpf <shortcut|(structured-insert-left)>
  oder <key|inactive links> eingetippt haben, geben Sie dem Argument einen Namen,
  z.B. \Rname'', um anschlieÿend darauf zugreifen zu können. Sie sollten nun
  Folgendes sehen:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|>>>
  </tm-fragment>

  In die zweite Argumentposition des Makrorumpfes tippen Sie nun Ihren Text
  z.B. \RHallo``, dann um das mit dem Namen \Rname`` bezeichnete erste
  Argument einzusetzen, drücken Sie die Kombinationen \ <shortcut|(make 'arg)> oder
  <key|inactive #> tippen dann schlieÿlich \Rname'', drücken <key|rechts>, das ist
  die rechte Pfeiltaste \ und geben weiter Text ein z.B. \R, wie geht es
  Ihnen?''. Das sieht dann so aus:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hallo <arg|name>, wie geht es
    Ihnen?>>>
  </tm-fragment>

  Die Kurzbefehlkombination <shortcut|(make 'arg)> bzw. <key|inactive #> wird zum Zugriff auf
  das Makroargument, hier \ <src-arg|name>, verwendet. Anstatt \ <shortcut|(make 'arg)>
  bzw. <key|inactive #> zu benutzen, dann \Rname'' and <key|right>
  einzutippen, können Sie auch die <key|\\>-Taste benutzen und \ <key|\\ n a
  m e> gefolgt von der Eingabetaste <shortcut|(kbd-return)> eintippen. Nachdem Sie
  Ihr Stil-Paket gesichert haben, können Sie Ihr neues Makro in jedem
  Dokument, dem Sie dieses Paket zugefügt haben, benutzen, indem Sie
  \ <key|\\ h e l l o> eingeben und die <key|return>-Taste betätigen.

  Intern werden alle Makrodefinitionen in der \R<TeXmacs>
  typesetter``-Umgebung gespeichert. Daneben werden dort auch normale
  Kontextvariablen wie Absatzzähler (section counters) oder Schriftgröÿe
  (font size) abgelegt. Die Kontextvariablen können global mit dem
  <markup|assign>-Konstrukt oder lokal mit dem <markup|with>-Konstrukt
  gesetzt werden. Wenn z.B. die folgenden Zeile\ 

  <\tm-fragment>
    <inactive*|<assign|section-nr|-1>>
  </tm-fragment>

  in Ihrem Paket enthalten ist und Sie als Basis-Stil <tmstyle|Artikel>
  verwenden, dann erhält der erste Abschnitt die Abschnittnummer
  <no-break><with|mode|math|0>.

  Die folgende Variante

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello
    <with|font-shape|small-caps|<arg|name>>!>>>
  </tm-fragment>

  <markup|hello>-Makros bringt den Namen in \ in
  <with|font-shape|small-caps|kleinen groÿbuchstaben> auf den Bildschirm.
  Beachten Sie, daÿ Sie mit dem <markup|with>-Konstrukt auch ein Makro lokal
  umdefinieren können. Dies wird beispielsweise in den Standardumgebungen für
  Listen benutzt, wo das Makro, das die Listensymbole liefert innerhalb des
  Listenrumpfes modifiziert wird.

  Eine weitere Variante des <markup|hello>-Makros benutzt das
  <markup|person>-Makro des Standard-Stils:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello <person|<arg|name>>!>>>
  </tm-fragment>

  Um in die Makrodefinition <inactive*|<person|<arg|name>>> einzufügen,
  müssen Sie zuerst an seiner Stelle ein Leerkonstrukt (compound) erzeugen.
  Dazu benutzen Sie <shortcut|(make 'compound)> oder <key|inactive c>, tippen dann \Rperson'', fügen
  ein Argument mit \ <shortcut|(structured-insert-right)> oder <key|inactive rechts> hinzu, und tippen
  schlieÿlich den Namen des Arguments <src-arg|name>. Schhlieÿlich drücken
  Sie \ <shortcut|(kbd-return)>, um das <with|color|blue|<translate|compound|english|german>>
  in ein <markup|person>-Makro umzuwandeln. Alternativ können Sie <key|\\>,
  ``person'', <shortcut|(structured-insert-right)> und \Rname'' tippen.

  Durch Kombination der vorgehend beschriebenen Vorgehensweisen sollte der
  Durchschnittsanwender bereits Stil-Pakete für alle häufig vorkommenden
  Anwendungsfälle zu schreiben können.\ 

  Ein interessane Technik, mit der sich Makros schreiben lassen, die
  komplizierte mathematische Formeln enthalten, die wiederum von variablen
  Formeln abhängen, ist die Folgende:\ 

  <\enumerate>
    <item>Schreibe die Formel z.B. <with|mode|math|(a<rsub|1>,\<ldots\>,a<rsub|n>)>
    in ein gewöhnliches Dokument.

    <item>Erzeuge ein Makroskelett in ihrem Stil-Paket:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|>>>
    </tm-fragment>

    <item>Kopiere die Formel und füge sie in den Rumpf des Makros ein:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|(a<rsub|1>,\<ldots\>,a<rsub|n>)>>>
    </tm-fragment>

    <item>Ersetze die die Variablen, die parametrisiert werden sollen, durch
    Makro-Argumente:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|(<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>)>>>
    </tm-fragment>

    <item>Nach dem Speichern können Sie das neue Makro in Dokumenten
    einsetzen, die Ihr Paket verwenden, z.B.:

    <\equation*>
      <with|n-tuple|<macro|a|(<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>)>|<n-tuple|a>=<n-tuple|b>.>
    </equation*>
  </enumerate>

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
    <associate|language|english>
    <associate|preamble|false>
  </collection>
</initial>