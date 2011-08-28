<TeXmacs|1.0.7.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Allgemeine Kontextvariablen>

  <\explain>
    <var-val|mode|text><explain-synopsis|Hauptmodus>
  <|explain>
    Diese sehr wichtige Kontextvariable definiert den aktuellen <em|Modus>.
    Es gibt vier mögliche Modi: <verbatim|text> (Text-Modus), <verbatim|math>
    (Mathematik-Modus), <verbatim|prog> (Programmier-Modus) und
    <verbatim|src> (Quellcode-Modus). Die Verhaltensweise des Editors
    \ (Menüs, Kurzbefehle, Schriftsatz, <abbr|usw.>) hängt empfindlich vom
    Modus ab. So kann beispielsweise der folgende Code zur Darstellung einer
    mathematischen Formel innerhalb eines Textes verwendet werden:

    <\tm-fragment>
      Die Formel <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>> ist gut
      bekannt.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|Die Formel <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>
      ist gut bekannt.>
    </tm-fragment>

    Einige andere Kontextvariablen (hauptsächlich Sprache und Schriftart)
    hängen auch von dem aktuellen Modus ab. Dabei benimmt sich der
    Quellcode-Modus immer ähnlich wie der text-Modus. Bei Kopier- und
    Einfügungs-Vorgängen versucht <TeXmacs> den Modus zu erhalten.
  </explain>

  <\explain>
    <var-val|language|english>

    <var-val|math-language|texmath>

    <var-val|prog-language|scheme><explain-synopsis|Sprache>
  <|explain>
    Eine weitere wichtige Variable ist die<em| aktuelle Sprache>. Tatsächlich
    sind es drei solche Variablen, eine für jeden Modus, wobei Text und
    Quellcode gleichgesetzt werden. Die Sprache, in der der Inhalt
    geschrieben ist, bestimmt die Semantik. Diese wird für verschiedene
    Zwecke gebraucht:

    <\itemize>
      <item>Die Sprache spezifiziert Regeln für den Schriftsatz z.B.
      sprachspezifische Interpunktions- und Trennungsregeln und
      <src-var|math-language> sorgt für adäquate Abstandsregeln bei
      mathematischen Operatoren.

      <item>Verschiedene Editiervorgänge hängen von der Spracheinstellung ab,
      z.B. Suchoperationen, <TeXmacs> ist sowohl Modus- wie Sprach-abhängig.
      Auch bestimmt die Spracheinstellung, welches Wörterbuch bei der
      Rechtschreibprüfung verwendet wird.\ 

      <item>Die Spracheinstellung regelt zusammen mit dem Modus und
      Stilvorgaben wie der Inhalt bei dem Wechsel von einem Modus zum anderen
      zu konvertieren ist.

      Derzeit sind noch keine wirklichen sprachabhängigen Konvertierungen
      implementiert. Für die Zukunft kann man sich aber vorstellen, dass ein
      Textfragment, dass aus einem englischen Text ausgeschnitten wird, in
      ein französisches Dokument übersetzt eingefügt wird. Auch könnte in
      mathematischen Dokumenten beispielsweise eine automatische
      Konvertierung von infix zu postfix-Notation vorgenommen werden.

      <item>Die Programmiersprache bestimmt die aktuell benutzte
      Skriptsprache. Andere Skriptsprachen als <scheme> können derzeit nur in
      interaktiven Sitzungen verwendet werden. In der Zukunft könnten
      Konstrukte wie <markup|extern> Programmiersprachen-abhängig werden.
    </itemize>

    Im Moment wird die aktuelle Sprache als ein Hinweis auf die Semantik von
    Text verwendet. Es ist nicht erforderlich, dass ein Text, der
    beispielsweise in Englisch geschrieben ist, keine Rechtschreibfehler
    enthält, oder dass eine mathematische Formel mathematisch oder semantisch
    korrekt ist. Jedoch ist es beabsichtigt, den Editor mehr und mehr dazu zu
    bringen, für korrekten Inhalt zu sorgen.

    Global kann die Sprache im Menü <menu|Document|Language> definiert
    werden, lokal mit <menu|Format|Language>.
  </explain>

  <\explain>
    <var-val|prog-session|default><explain-synopsis|Name der
    Programmiersitzung>
  <|explain>
    Die Kontextvariable wird zusätzlich zur <src-var|prog-language>-Variablen
    benutzt, um die konkrete Implementierung und Version der
    Programmiersprache zu kennzeichnen. Im Fall von
    <hlink|<name|Maxima>|../../../main/interface/cas/man-maxima.en.tm> können
    unterschiedliche <name|Lisp>-Versionen Verwendung finden. Manchmal möchte
    man auch unterschiedliche <name|Maxima>-Versionen parallel einsetzen.
  </explain>

  <\explain>
    <label|magnification><var-val|magnification|1><explain-synopsis|Vergröÿerungsfaktor>
  <|explain>
    Diese Variable bestimmt, welche Vergröÿerung bei der Darstellung auf den
    ganzen Inhalt angewendet werden soll. Vergröÿerungsfaktoren über 1 werden
    typischerweise bei Präsentationen verwendet z.B. bei Präsentationen mit
    Beamer von einem Laptop.

    <\tm-fragment>
      normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>
    </tm-fragment>

    <\tm-fragment>
      <inactive*|normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>>
    </tm-fragment>

    Der Vergröÿerungsfaktor sollte nicht mit der Schriftgröÿe (<hlink|font
    size|env-font.en.tm#font-base-size>) verwechselt werden. Im Gegensatz zur
    Vergröÿerung verändert die Schriftgröÿe die Form der Buchstaben. Der
    Vergröÿerungsfaktor wir normalerweise für das ganze Dokument im Menü
    <menu|Document|Magnification> eingestellt.
  </explain>

  <\explain>
    <var-val|bg-color|white><explain-synopsis|Hintergrundfarbe>
  <|explain>
    Die Hintergrundfarbe des Dokuments wird im Menü
    <menu|Document|Color|Background> festgelegt.
  </explain>

  <\explain>
    <var-val|color|black><explain-synopsis|Vordergrundfarbe>
  <|explain>
    Die Vordergrundfarbe für Text und Graphik kann global im Menü
    <menu|Document|Color|Foreground> oder lokal im Menü <menu|Format|Color>
    eingestellt werden.
  </explain>

  <\explain>
    <var-val|preamble|false><explain-synopsis|Quellcode-Modus?>
  <|explain>
    Dieses Flag steuert, ob eine normales Textdokument oder eine
    Stil-Definition editiert wird. Der Quellcode-Modus (preamble mode) kann
    im Menü <menu|Document|View|Edit source tree> ausgewählt werden.
  </explain>

  <\explain>
    <var-val|info-flag|short><explain-synopsis|Darstellung von
    Informations-Marken>
  <|explain>
    Diese Variable steuert die Darstellung von Informations-Marken, die in
    den Text eingefügt werden, um sonst unsichtbare Marken oder
    Schriftsatz-Konstrukte erkennbar zu machen. Der <src-var|info-flag> kann
    die Werte <verbatim|none>, <verbatim|short> und <verbatim|detailed>
    annehmen:

    <\tm-fragment>
      <with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|<with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.>
    </tm-fragment>

    Normalerweise wird die Darstellung von Informations-Marken global im Menü
    <menu|Document|View|Informative flags> eingestellt.
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