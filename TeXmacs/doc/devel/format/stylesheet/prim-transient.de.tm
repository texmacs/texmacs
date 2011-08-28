<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Darstellung und Aktivität von Stil-Konstrukten>

  Die Befehle, die in diesem Abschnitt beschrieben werden, dienen zur
  Steuerung der Darstellung von Stildefinitionen und Stil-Elementen. Sie
  enthalten sowohl Befehle zur Darstellung wie als auch zur Aktivierung und
  Desaktivierung von Code.

  <\explain>
    <explain-macro|active|content>

    <explain-macro|active*|content>

    <explain-macro|inactive|content>

    <explain-macro|inactive*|content><explain-synopsis|Aktivierung/Desaktivierung
    von Inhalten>
  <|explain>
    Diese Befehle (Tags) dienen dazu, die Aktivität von Inhalten
    <src-arg|content> vorübergehend oder permanent zu ändern. In gewöhnlichen
    Dokumenten sind Befehle aktiv gemäÿ Voreinstellung. In Stildefinitionen
    dagegen ist die Voreinstellung inaktiv. Inaktiv wird
    <inactive*|<frac|1|2>> als <inactive*|<frac|1|2>> dargestellt, aktiv
    <frac|1|2>.

    <markup|active> und <markup|inactive> aktivieren oder deaktivieren nur
    die Wurzel des Inhalts <src-arg|content>. Gewöhnlich kann ein Tag, der
    verborgene Informationen enthält wie z.B. <markup|hlink> dadurch
    deaktiviert werden, dass der Cursor direkt dahinter positioniert wird und
    dann die <key|backspace>-Taste gedrückt wird. Damit wird der
    Hyperlink in einen inaktiven Tag der Form
    \ <explain-macro|inactive|<with|font-shape|right|<explain-macro|hlink|body|ref>>>
    transformiert.

    Die Varianten <markup|active*> und <markup|inactive*> dienen zum
    Aktivieren bzw. Deaktivieren des gesamten Inhalts <src-arg|content>,
    auÿer, wenn sich weitere Aktivierung- bzw. Deaktivierung-Tags innerhalb
    von <src-arg|content> befinden. <markup|inactive*> wird häufig in der
    vorliegenden Dokumentation benutzt, um die inaktive Darstellung von
    <TeXmacs>-Code zu zeigen. Manchmal ist es notwendig, einen bestimmten
    Unter-Baum innerhalb von inaktivem Inhalt zu aktivieren, was man mit
    <markup|active*> machen kann. Z.B. enthält das folgende Stück mit
    <markup|inactive*> inaktivierten Codes

    <\tm-fragment>
      <inactive*|<assign|love|<macro|from|<active*|<with|mode|math|<with|color|red|\<heartsuit\>\<heartsuit\>\<heartsuit\>>>>
      from <arg|from>.>>>
    </tm-fragment>

    den mit <markup|active*> reaktivierten Ausdruck
    <with|mode|math|<with|color|red|\<heartsuit\>\<heartsuit\>\<heartsuit\>>>.\ 

    \;
  </explain>

  <\explain>
    <explain-macro|inline-tag|name|arg-1|<with|mode|math|\<cdots\>>|arg-n><explain-synopsis|Darstellung
    von Zeilen-Befehlen>
  <|explain>
    Dieser Befehl dient zur Vorgabe der Darstellung eines inaktiven Tags mit
    dem Namen <src-arg|name> und den Argumenten <src-arg|arg-1> bis
    <src-arg|arg-n>. \ <inactive*|<inline-tag|foo|x|y>> erzeugt z.B.
    <inline-tag|foo|x|y>. Der Darstellungsstil kann im Menü
    <menu|Document|View|Source tags> oder durch Modifizierung der
    Kontext-Variablen <src-var|src-style>, <src-var|src-special>,
    <src-var|src-compact> und <src-var|src-close> angepasst werden.
  </explain>

  <\explain>
    <explain-macro|open-tag|name|arg-1|<with|mode|math|\<cdots\>>|arg-n>

    <explain-macro|middle-tag|name|arg-1|<with|mode|math|\<cdots\>>|arg-n>

    <explain-macro|close-tag|name|arg-1|<with|mode|math|\<cdots\>>|arg-n><explain-synopsis|Darstellung
    mehrzeiliger Konstrukte>
  <|explain>
    Diese Befehle haben ähnliche Aufgaben wie <markup|inline-tag> allerdings
    für den Fall, das sich die Argumente über mehrere Zeilen erstrecken.
    Typische HTML-ähnliche Tags entsprechen <explain-macro|open-tag|name> und
    <explain-macro|close-tag|name>. Da aber <TeXmacs>-Makros mehr als ein
    Argument haben können, gibt es den <markup|middle-tag>-Befehl für die
    Darstellung von Argumente. Auÿerdem können diese \ Befehle zusätzliche
    Zeilen-Argumente annehmen. Beispielsweise wird der Code

    <\tm-fragment>
      <\inactive*>
        <open-tag|theorem>

        <indent|Das Wetter sollte heute schön werden.>

        <close-tag|theorem>
      </inactive*>
    </tm-fragment>

    so dargestellt

    <\tm-fragment>
      <open-tag|theorem>

      <indent|Das Wetter sollte heute schön werden.>

      <close-tag|theorem>
    </tm-fragment>

    Die Darstellung kann analog zu <markup|inline-tag> gesteuert werden.
  </explain>

  <\explain>
    <explain-macro|style-with|var-1|val-1|<with|mode|math|\<cdots\>>|var-n|val-n|body>

    <explain-macro|style-with*|var-1|val-1|<with|mode|math|\<cdots\>>|var-n|val-n|body><explain-synopsis|Änderung
    der Darstellung von Stildefinitionen>
  <|explain>
    Dieser Befehl dient zur zeitweiligen Änderung von inaktiven Befehlen,
    indem lokal innerhalb des Schriftsatzes des Rumpfes, <src-arg|body>, die
    Variablen <src-arg|var-i> auf <src-arg|val-i> gesetzt werden. Wenn eine
    Stil-Definition importiert wird, wird jeder
    <markup|style-with>/<markup|style-with*>-Befehl durch seinen Rumpf.
    <src-arg|body>, ersetzt. Bei <markup|style-with> ist die modifizierte
    Darstellung auf die Wurzel des Rumpfes beschränkt. Im Fall von
    <markup|style-with*> erstreckt die Wirkung auf den ganzen Rumpf
    <src-arg|body>.
  </explain>

  <\explain>
    <explain-macro|style-only|<with|font-shape|right|<explain-macro|foo|content>>>

    <explain-macro|style-only*|<with|font-shape|right|<explain-macro|foo|content>>><explain-synopsis|Inhalt
    für Stildefinitionen>
  <|explain>
    Die Befehl dient dazu, einen inaktiven Befehl so darzustellen, als ob das
    Makro <markup|foo> darauf angewendet worden sei. Wenn eine
    Stil-Definition importiert wird, wird jeder
    <markup|style-only>/<markup|style-only*>-Befehl durch seinen
    <src-arg|content> ersetzt. Bei <markup|style-only> ist die modifizierte
    Darstellung auf die Wurzel von <src-arg|content> beschränkt. Im Fall von
    <markup|style-only*> erstreckt die Wirkung auf den ganzen
    <src-arg|content>.
  </explain>

  <\explain>
    <explain-macro|symbol|symbol>

    <explain-macro|latex|cmd>

    <explain-macro|hybrid|cmd>

    <explain-macro|hybrid|cmd|arg><explain-synopsis|Hilfsbefehle zur Eingabe
    von speziellem Inhalt>
  <|explain>
    Die Befehle sind werden nur während der Eingabe von speziellen Inhalten
    verwendet.

    Wenn <shortcut|(make 'symbol)> gedrückt wird, wird ein <markup|symbol>-Befehl erzeugt.
    Nachdem der Name eines Symbols oder sein ASCII-Code eingegeben wurde und
    die <key|Rücklauf>-Taste gedrückt wurde, wird der Symbol-Befehl durch das
    entsprechende Symbol ersetzt, das ist normalerweise eine Zeichenkette in
    spitzen Klammern <verbatim|\<less\>\<gtr\>>.

    Wenn man <key|\\> eingibt, wird ein <markup|hybrid>-Befehl erzeugt.
    Nachdem eine Zeichenkette eingegeben und die <key|Rücklauf>-Taste
    gedrückt wurde, wird festgestellt, ob es sich bei der Zeichenkette um
    einen <LaTeX>-Befehl, einen Makro-Befehl oder eine Kontext-Variable
    handelt (in dieser Reihenfolge). Ist dies der Fall, so wird der
    <markup|hybrid>-Befehl durch den zutreffenden Inhalt ersetzt. Wenn man
    <key|\\> eingibt, während eine Auswahl aktiv ist, wird die Auswahl
    automatisch das Argument des <markup|hybrid>-Befehls oder der Befehl
    selbst, wenn er erkannt wird.

    Der <markup|latex>-Befehl arbeitet ähnlich wie der
    <markup|hybrid>-Befehl, nur dass der Befehl ausschlieÿlich
    <LaTeX>-Befehle erkennt.
  </explain>

  Die Darstellungs-Makros für Quell-Code sind fest in <TeXmacs> eingebaut.
  Aber sie sollten eigentlich nicht als fundamentale Konstrukte betrachtet
  werden. Sie sind aber kein Teil irgendeiner Stil-Definition.

  <\explain>
    <explain-macro|indent|body><explain-synopsis|Einzüge>
  <|explain>
    Setze <src-arg|body> mit Einzug.
  </explain>

  <\explain>
    <explain-macro|rightflush><explain-synopsis|Blockkontext bis zum rechten
    Rand verbreitern>
  <|explain>
    Das Makro sorgt dafür, dass ein Block-Kontext, nicht notwendigerweise der
    sichtbare Inhalt, sich über die ganze verfügbare Breite erstreckt. Das
    gibt ein besseres Layout für die Informationboxen im Editor und hilft
    beim Positionieren des Cursors.
  </explain>

  <\explain>
    <explain-macro|src-macro|macro-name>

    <explain-macro|src-var|variable-name>

    <explain-macro|src-arg|argument-name>

    <explain-macro|src-tt|verbatim-content>

    <explain-macro|src-integer|interger>

    <explain-macro|src-length|length>

    <explain-macro|src-error|message><explain-synopsis|Syntaktische
    Hervorhebungen>
  <|explain>
    Diese Makros dienen zur syntaktischen Hervorhebung von Code. Sie
    bestimmen, wie Unter-Bäume, die Makro-Namen, Variablen-Namen,
    Argument-Namen, wörtlichem Inhalt, Ganzzahlen, Längen und Fehlermeldungen
    dargestellt werden sollen.
  </explain>

  <\explain>
    <explain-macro|src-title|title>

    <explain-macro|src-style-file|name|version>

    <explain-macro|src-package|name|version>

    <explain-macro|src-package-dtd|name|version|dtd|dtd-version><explain-synopsis|administrative
    Stil-Funktionen>
  <|explain>
    Diese Makros dienen zur Identifikation von Stil-Definitionen, -Paketen
    und der zugehörigen <abbr|D.T.D.>s. <markup|src-title> ist ein Container
    für <markup|src-style-file>, <markup|src-package>,
    <markup|src-package-dtd>, <markup|src-license> und
    <markup|src-copyright>.

    <markup|src-style-file> spezifiziert den Namen <src-arg|name> und Version
    <src-arg|version> einer Stil-Definition und setzt die Kontext-Variable
    <src-var|<src-arg|name>-style> auf <src-arg|version>.
    <markup|src-package-dtd> spezifiziert den Namen <src-arg|name> und die
    Version \ <src-arg|version> eines Pakets sowie die zugehörige D.T.D.
    <src-arg|dtd> und seine Version <src-arg|dtd-version>. Es setzt die
    Kontext-Variable <src-var|<src-arg|name>-package> auf <src-arg|version>
    und <src-var|<src-arg|dtd>-dtd> auf <src-arg|dtd-version>. Der
    <markup|src-package> -Befehl ist eine Kurzversion von
    <markup|src-package-dtd> für alle Fälle, in denen der <abbr|D.T.D.>-Name
    mit dem Namen des Pakets übereinstimmt.\ 
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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