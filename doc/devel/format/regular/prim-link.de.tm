<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Link-Konstrukte>

  <\explain>
    <explain-macro|label|name><explain-synopsis|Referenzziel>
  <|explain>
    Der Operand muss zu einer Zeichenkette evaluieren. Dieser wird als
    Zielname einer der folgenden Referenztypen, <markup|reference>,
    <markup|pageref> und <markup|hlink> verwendet.

    Der Name eines Labels sollte eindeutig innerhalb eines Dokuments sein und
    darf daher nur einmal vergeben werden.

    Beispiele in diesem Abschnitt verweisen auf das Label, <markup|label>,
    mit dem Namen \Rthere'' <active*|><label|there>.

    <\tm-fragment>
      <inactive*|<label|there>>
    </tm-fragment>

    \;
  </explain>

  <\explain>
    <explain-macro|reference|name><explain-synopsis|Verweis auf einen Namen>
  <|explain>
    Das Argument ist eine Zeichenkette, ein Label, das in einem
    <markup|label>-Konstrukt im aktuellen oder einem anderen zum aktuellen
    Projekt gehörigen Dokument definiert worden ist.\ 

    <\tm-fragment>
      <inactive*|<reference|there>>
    </tm-fragment>

    Der Verweis, <markup|reference>, wird beim Schriftsetzen durch den Wert
    der Variablen <src-var|the-label> am Punkt des Ziels <markup|label>. Die
    Variable <src-var|the-label> wird in vielen unterschiedlichen Strukturen
    wie Abschnitte, Abbildungen, nummerierten Gleichungen usw. gesetzt.

    Eine Referenz, ein Verweis, <markup|reference>, reagiert auf Mausklicks
    wie ein Hyperlink.
  </explain>

  <\explain>
    <explain-macro|pageref|name><explain-synopsis|Seitenzahl>
  <|explain>
    Das Argument muss zu einer Zeichenkette evaluieren, die als Label mit dem
    Konstrukt <markup|label> innerhalb des aktuellen Dokuments oder einem
    Dokument des aktuellen Projektes definiert wurde.

    <\tm-fragment>
      <inactive*|<pageref|there>>
    </tm-fragment>

    <markup|><markup|pageref> wird im Schriftsatz durch die Seitenzahl der
    Seite ersetzt, die das Ziellabel, <markup|label>, enthält. Man beachte,
    dass Seitenzahlen nur berechnet werden können, wenn das Dokument mit
    Seitenumbruch gesetzt wird. Das ist nicht der Fall in den Seitentypen
    \R<translate|automatic|english|german>'' oder
    \R<translate|papyrus|english|german>''.

    <markup|pageref> reagiert auf Mausklicks wie ein Hyperlink.
  </explain>

  <\explain>
    <explain-macro|hlink|content|url><explain-synopsis|Hyperlink>
  <|explain>
    Dieses Konstrukt erzeugt einen Hyperlinks mit dem sichtbaren Text
    <src-arg|content>, der auf <src-arg|url> zeigt. <src-arg|url> muss zu
    einer Zeichenkette in <abbr|URL>-Syntax evaluieren und zu einem lokalen
    Dokument oder einem Dokument auf einem anderen Rechner verweisen.
    Positionen innerhalb eines Dokuments können mit Labeln definiert werden.

    Die folgenden Beispiele verweisen auf das gleiche Dokument, ein Dokument
    im gleichen Verzeichnis und einem Web-Dokument.

    <\tm-fragment>
      <inactive*|<hlink|Dieses Dokument|./#there>>

      <inactive*|<hlink|gleiches Verzeichnis|file.tm#there>>

      <inactive*|<hlink|Im Web|http://example.org/#there>>
    </tm-fragment>

    Das erste Beispiel wird im laufenden Text so <hlink|Dieses
    Dokument|./#there> dargestellt. Wenn das Dokument nicht editierbar ist,
    wird es mit einem einfachen Klick erreicht. Für editierbare Dokumente ist
    dagegen ein Doppelklick erforderlich.
  </explain>

  <\explain>
    <explain-macro|include|url><explain-synopsis|ein anderes Dokument
    einbinden>
  <|explain>
    <src-arg|url> muss zu einer Zeichenkette in <abbr|URL>-Syntax evaluieren
    und zu einem lokalen Dokument oder einem Dokument auf einem anderen
    Rechner verweisen. Er wird als Dateiname interpretiert und der Inhalt der
    Datei wird anstelle des <markup|include>-Konstrukts eingefügt. Dafür muss
    dieser in einem Blockkontext liegen.
  </explain>

  <\explain>
    <explain-macro|action|content|script><explain-synopsis|ein ausführbares
    Skript einbinden>
  <|explain>
    Ein ausführbares <value|scheme>-Skript, <src-arg|script> einbinden, dass
    bei einem doppelten Mausklick auf <src-arg|content> ausgeführt wird.
    Beispielsweise, wenn Sie auf <action|hier|(system "xterm &")>
    doppelklicken, erzeugen Sie ein neues <verbatim|xterm>. Der Code dafür
    ist <inactive*|<action|hier|(system "xterm &")>>.

    Aus Sicherheitsgründen wird vom Nutzer normalerweise, wenn ausführbare
    Skripte eingeleitet werden sollen, eine Bestätigung verlangt. Das
    Sicherheitsniveau kann im Menü <menu|Edit|Preferences|Security>
    eingestellt werden. Programmierer können auch bestimmte
    <value|scheme>-Routinen als \Rsicher'' erklären.
    <value|scheme>-Programme, die nur sichere Routinen enthalten werden ohne
    Rückfrage ausgeführt.
  </explain>

  <\explain>
    <explain-macro|mutator|content|script><explain-synopsis|ein Konstrukt,
    das sich selbst modifizieren kann>
  <|explain>
    Der Inhalt, <src-arg|content>, eines <markup|mutator>-Konstrukts wird
    automatisch durch das <value|scheme>-Skript, <src-arg|script>, erzeugt.
    Genauer, <TeXmacs> ermittelt periodisch, welche
    <markup|mutator>-Konstrukte geöffnet sind. Für jeden setzt es den
    <em|Mutatorpfad> auf den Pfad zu dem Inhalt, <src-arg|content>, und ruft
    dann das zugehörige <value|scheme>-Skript. Dieses Skript kann den Inhalt,
    <src-arg|content>, modifizieren oder auch andere Stellen des Dokuments.
    Letzteres ist aber wenig effizient und wird deshalb nicht empfohlen. Um
    den <em|Mutatorpfad> aus <value|scheme> zu extrahieren, sollte man den
    <value|scheme>-Befehl <verbatim|(get-mutator-path)>benutzen.

    Mutatoren sind sehr nützlich, wenn <TeXmacs> mit externen Programmen
    kommuniziert. Die derzeitige Implementierung von Computeralgebra und
    anderen \RSessions`` benutzt Mutatoren, damit man an einem Dokument
    arbeiten kann, während extern Berechnungen durchgeführt werden. Da
    Mutatoren vom Editor automatisch gefunden werden, bleibt ihr Verhalten
    auch dann korrekt, wenn die Position des Mutators im Dokument geändert
    wird. Das geschieht beispielsweise, wenn man einen neuen Absatz am Beginn
    des Dokuments einfügt.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

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