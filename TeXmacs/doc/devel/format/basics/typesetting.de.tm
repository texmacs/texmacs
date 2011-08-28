<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Der Schriftsatz-Prozess>

  Um das <TeXmacs>-Format zu verstehen, sollte man wissen, wie der
  Schriftsatz von Dokumenten im Editor durchgeführt wird. Der Prozess setzt
  die logischen <TeXmacs>-Baum-Strukturen in physikalische <em|Boxen> um, die
  auf dem Bildschirm dargestellt werden können. Man muss sich darüber klar
  sein, dass diese Boxen auÿer den Informationen, die zur Darstellung auf dem
  Bildschirm erforderlich sind, weitere Informationen enthalten, so z.B. wie
  der Cursor innerhalb der Box zu positionieren ist oder wie wie Text
  ausgewählt wird.\ 

  Der Prozess, der den Schriftsatz durchführt, der <em|Schriftsetzer>, führt
  zwei verschiedene Abläufe aus: die Auswertung des <TeXmacs>-Baums und den
  eigentlichen Schriftsatz. Diese beiden Vorgänge laufen zur Zeit in einem
  Schritt ab, das kann sich aber in Zukunft ändern.

  Die <hyper-link|Schriftsatz-Konstrukte|../regular/regular.de.tm> sind in
  den Editor integriert und auf Schnelligkeit optimiert. So gibt es z.B.
  Konstrukte zur horizontalen Verkettung \ (<markup|concat>), für den
  Seitenumbruch (<markup|page-break>), für mathematische Brüche
  (<markup|frac>), für Hyperlinks (<markup|hlink>) usw.. Die genaue
  Darstellungsweise vieler Schriftsatz-Konstrukte kann mittels
  <hyper-link|vordefinierter Kontextvariablen|../environment/environment.de.tm><with|language|german|>
  gesteuert werden. Zum Beispiel spezifiziert die Kontextvariable
  <src-var|color> die aktuelle Farbe eines Objekts, <src-var|par-left>
  dagegen den linken Seitenabstand von Absätzen usw..

  <hyper-link|Die Sprache für Stil-Definitionen|../stylesheet/stylesheet.de.tm>
  erlaubt dem Benutzer, neue Konstrukte (Makros) zu definieren und
  vordefinierte zu modifizieren. Sie enthält die Konstrukte zur
  Makrodefinition, Ablaufsteuerung, Berechnungen, bedingte und verzögerte
  Ausführung usw.. \ Die Sprache für Stil-Definitionen besitzt auÿerdem den
  besonderen Befehl <markup|extern>, mit dem sich <value|scheme>-Programme
  einbinden lassen.\ 

  Man beachte, dass benutzerdefinierte Makros zwei verschiedene Aufgaben
  durchführen.Eine Aufgabe besteht in einfachen Ersetzungen. Zum Beispiel ist
  das folgende Makro eine Kurzform von so etwas wie
  \ <with|mode|math|a<rsub|1>,\<ldots\>,a<rsub|n>>.

  <\tm-fragment>
    <inactive*|<assign|seq|<macro|var|from|to|<active*|<with|mode|math|<inactive*|<arg|var>><rsub|<inactive*|<arg|from>>>,\<ldots\>,<inactive*|<arg|var>><rsub|<inactive*|<arg|to>>>>>>>>
  </tm-fragment>

  Wenn Makros nur aus Ersetzungen bestehen, dann sind sie und ihre Kinder
  \Rerreichbar``, <em|accessible> im Editor. Im Beispiel oben sind z.B. die
  Argumente <src-arg|var>, <src-arg|from> und <src-arg|to> die Kinder von
  <markup|seq>. Sie können also den Cursor hinein setzen und diese ändern.
  Die andere Aufgabe sind Berechnungen und synthetische Aufgaben. Das ist
  beispielsweise bei den Punkten im obigen Beispiel der Fall. Diese sind
  nicht erreichbar. Sie können sie nicht verändern. Das Makro

  <\tm-fragment>
    <inactive*|<assign|square|<macro|x|<times|<arg|x>|<arg|x>>>>>
  </tm-fragment>

  ist ein reines Rechenmakro. Generell gilt rein, dass synthetische Makros
  leichter zu schreiben sind, dass aber die Editierbarkeit von Makros
  leichter und natürlicher wird, je mehr man auf den Erhalt der
  Erreichbarkeit achtet.

  <TeXmacs> produziert eine Reihe von Hilfsdaten während des Schriftsetzens,
  beispielsweise die \ Werte von Verweisen, Seitenzahlen,
  Inhaltsverzeichnisse, Indexe usw.. Diese werden im Zuge des Schriftsetzens
  ermittelt und gespeichert. Diese Hilfsdaten können prinzipiell aus dem
  Dokument berechnet werden. Das kann aber zeitaufwendig sein und im Fall,
  dass ein externes Plugin die Berechnungen durchführt, kann es passieren,
  dass dieses Plugin auf anderen Rechnern nicht zur Verfügung steht, die
  Berechnung also nicht durchführbar ist. \ Deshalb werden die gespeicherten
  Hilfsdaten mit dem Dokument zusammen abgespeichert, sobald sie es auf der
  Festplatte sichern.

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
  </collection>
</initial>