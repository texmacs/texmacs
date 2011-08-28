<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs>-Dokumente>

  <TeXmacs>-Dokumente sind natürlich <TeXmacs>-Bäume, diese haben allerdings
  eine spezielle Form, die im folgenden beschrieben wird. Unter einem
  <TeXmacs>-Dokument wird hier eine Datei bezeichnet, die unter <TeXmacs>
  erstellt, sich im Speicher (Puffer) befindet oder auf einem Speichermedium
  gespeichert wurde. Textstücke, also Teile von Dokumenten, sind zwar auch
  <TeXmacs>-Bäume, aber normalerweise keine Dokumente.

  Die Wurzel eines Dokuments muss der Standard-Operator <markup|document>
  sein. Er hat notwendigerweise die beiden Kinder\ 

  <\explain|<explain-macro|TeXmacs|Version><explain-synopsis|<TeXmacs>
  Version>>
    Dieser Operator ist notwendig und spezifiziert die Version von <TeXmacs>,
    die zum abspeichern des Dokuments benutzt wurde.
  </explain>

  <\explain|<explain-macro|body|Inhalt><explain-synopsis|Dokumenten-Rumpf >>
    Dieser notwendige Operator spezifiziert den Rumpf des Dokuments, also im
    Endeffekt den Inhalt.
  </explain>

  Zu diesen beiden notwendigen Operatoren können weitere Operatoren/Kinder
  hinzutreten, sofern diese aus den folgenden ausgewählt werden:

  <\explain>
    <explain-macro|style|Version>

    <explain-macro|style|<with|font-shape|right|<explain-macro|tuple|style|pack-1|<with|mode|math|\<cdots\>>|pack-n>>><explain-synopsis|<translate|style|english|german>
    und <translate|packages|english|german>>
  <|explain>
    Ein <translate|style|english|german> und zusätzliche
    <translate|packages|english|german>.
  </explain>

  <\explain|<explain-macro|project|Verweis><explain-synopsis|Projekthinweis>>
    Damit kann ein Verweis auf ein Projekt hinzugefügt werden, zu dem das
    Dokument gehört.\ 
  </explain>

  <\explain|<label|initial-env><explain-macro|initial|Tabelle><explain-synopsis|Start-Kontext>>
    Hiermit kann ein Kontext festgelegt werden, mit dem das Dokument
    gestartet wird: z.B. Seitengröÿe, Randmaÿe, usw.. Die <src-arg|Tabelle>
    hat die Form <explain-macro|collection|Bindung-1|<with|mode|math|\<cdots\>>|Bindung-n>.
    Jede <src-arg|Bindung-<no-break>i> hat die Form
    <explain-macro|associate|Variable-i|Wert-i> und setzt die Kontextvariable
    <src-arg|Variable-i> auf den Startwert <src-arg|Wert-i>. Startwerte für
    Kontextvariable, die nicht in der Tabelle enthalten sind, werden durch
    den Basis-Stil und die Stilpakete festgelegt.\ 
  </explain>

  <\explain|<explain-macro|references|Tabelle><explain-synopsis|Verweise>>
    Wahlweise eine Liste aller gültigen Verweise auf eine Marke \RLabel'' in
    dem Dokument. Auch wenn diese Information von <TeXmacs> automatisch
    erzeugt werden kann, ist es sinnvoll eine solche Tabelle abzuspeichern,
    denn es braucht mehrere Durchgänge, um eine solche Tabelle automatisch zu
    erzeugen. Um anwenderfreundliches Verhalten des Editors zu erreichen,
    werden Verweise zusammen mit dem Dokument gespeichert.

    Die <src-arg|Tabelle> hat eine ähnliche Form wie in der vorgehenden
    Operation. Hier wird ein \RTuple`` mit jedem Verweis verbunden. Dieses
    Tuple hat entweder die Form <explain-macro|tuple|Inhalt|Seiten-Nr> oder
    <explain-macro|tuple|Inhalt|Seiten-Nr|Datei>. Der <src-arg|Inhalt>
    enthält den Text, der gezeigt wird, wenn der Verweis angesprochen wird,
    und <src-arg|Seiten-Nr> enthält die dazugehörige Nummer der Seite. Die
    Option \R<src-arg|Datei>'' gibt die Datei an, in welcher die Marke
    \RLabel'' definiert wurde. Das ist nur notwendig, wenn das Dokumente zu
    einem Projekt gehört, das aus mehreren Dateien besteht.
  </explain>

  <\explain|<explain-macro|auxiliary|Tabelle><explain-synopsis|zusätzliche
  Hilfsdaten zu einem Dokument>>
    Dies ist eine wahlweise Tabelle, die zusätzliche Hilfsdaten abspeichert.
    Wie oben können diese Daten automatisch aus dem Dokument selbst berechnet
    werden, diese Berechnungen können aber aufwändig sein und unter Umständen
    auch Werkzeuge benötigen, die möglicherweise auf Ihrem System nicht
    vorhanden sind. Die <src-arg|Tabelle>, wird ähnlich wie oben definiert,
    assoziiert die Hilfsdaten mit einem Schlüssel. Standard-Schlüssel sind
    u.a. <verbatim|bib>, <verbatim|toc>, <verbatim|idx>, <verbatim|gly> usw..
  </explain>

  <\example>
    Ein Beispiel für ein Dokument, das nur den einfachen Text \RHallo Welt!''
    enthält, liefert den folgenden Baum,

    <\equation*>
      <tree|<with|mode|text|<markup|document>>|<tree|<with|mode|text|<markup|TeXmacs>>|<with|mode|text|<TeXmacs-version>>>|<tree|<with|mode|text|<markup|style>>|article>|<tree|<with|mode|text|<markup|body>>|<tree|<with|mode|text|<markup|document>>|Hallo
      Welt!>>>
    </equation*>
  </example>

  Wie Sie sehen, kann unterhalb der Dokument-Wurzel mit Standard-Operator
  <markup|document> der Standard-Operator <markup|document> erneut auftreten.
  Diese Äste sind in der Regel keine Dokumente, das die Kinder
  <explain-macro|TeXmacs|Version> und <explain-macro|body|Inhalt> fehlen.

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