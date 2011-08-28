<TeXmacs|1.0.4.2>

<style|<tuple|tmdoc|number-europe>>

<\body>
  <tmdoc-title|Zähler und Zählergruppen>

  <TeXmacs> benutzt zum automatischen Nummerieren von Abschnitten, Sätzen
  usw. Zähler \Rcounter''. Solche Zähler können individuelle Zähler sein wie
  z.B. <src-var|equation-nr> oder zu einer Gruppe ähnlicher Zähler gehören
  wie etwa <src-var|theorem-nr>. <TeXmacs> erlaubt die Anpassung von Zählern
  auf individueller oder auf Gruppenbasis. Man kann die Darstellung des
  Zählers ändern, diesen beispielsweise mit römischen Zahlen ausgeben, oder
  man kann spezielle Aktionen durchführen, dem Zähler z.B. erhöhen oder einen
  Unterzähler neu initialisieren.

  Neue individuelle Zähler können mit dem folgenden Makro definiert werden:

  <\explain|<explain-macro|new-counter|x>>
    definiert einen neuen Zähler mit dem Namen <src-arg|x>. Der Zähler wird
    in dem numerischen Kontext <src-var|x-nr> gespeichert. Gleichzeitig
    werden die folgenden Makros erzeugt:

    <\explain|<explain-macro|the-<em|x>>>
      Greife auf den Zähler zu, so wie er auf dem Bildschirm erscheint.
    </explain>

    <\explain|<explain-macro|reset-<em|x>>>
      Setze den Zähler auf 0.
    </explain>

    <\explain|<explain-macro|inc-<em|x>>>
      Inkrementiere den Zähler um 1. Dieses Makro kann so geändert werden,
      dass es auch andere Zähler inkrementiert, auch wenn das so in den
      Standard-Stil-Definitionen nicht gemacht wird.
    </explain>

    <\explain|<explain-macro|next-<em|x>>>
      Inkrementiere den Zähler um 1, zeige den Zähler auf dem Bildschirm und
      setze das aktuelle Label.
    </explain>

    Um Anpassungen zu erleichtern, definiert <markup|new-counter> zusätzlich
    die folgenden beiden Makros:

    <\explain|<explain-macro|display-<em|x>|nr>>
      Dieses Makro transformiert den numerischen Wert des Zählers in
      denjenigen, der auf dem Bildschirm gezeigt wird.
    </explain>

    <\explain|<explain-macro|counter-<em|x>|x>>
      Dieses interne Makro gibt den Namen desjenigen Kontexts zurück, dem der
      Zähler angehört. Normalerweise ist dies \Rnr-x'', es kann aber auch
      umdefiniert worden sein, wenn der Zähler zu einer Gruppe gehört.
    </explain>
  </explain>

  Wie bereits gesagt benutzt <TeXmacs> <em|Zählergruppen>, um ähnliche Zähler
  auf ähnliche Weise zu behandeln. Z.B. gehören zu Zählergruppe
  <verbatim|theorem-env> die Zähler <verbatim|theorem>,
  <verbatim|proposition>, <verbatim|lemma>, <abbr|usw.>. Man definiert neue
  Zählergruppen werden mit:

  <\explain|<explain-macro|new-counter-group|g>>
    erzeugt eine neue Zählergruppe mit dem Namen <src-arg|g> und gleichzeitig
    folgende Makros:

    <\explain>
      <explain-macro|display-in-<em|g>|x|nr>

      <explain-macro|counter-in-<em|g>|x>
    <|explain>
      Diese Makros verhalten sich analog zu den oben beschriebenen Makros
      <markup|display-<em|x>> und <markup|counter-<em|x>> jedoch für Zähler
      der Gruppe <src-arg|g>. Sie übernehmen als Argument den Namen
      <src-arg|x> des Zählers.
    </explain>
  </explain>

  Neue Zähler können zur Gruppe zugefügt werden mit:

  <\explain|<explain-macro|add-to-counter-group|x|g>>
    Damit wird ein neuer Zähler <src-arg|x> definiert und zur Gruppe
    <src-arg|g> hinzugefügt. Die Rolle der Makros \ <markup|display-<em|x>>
    und <markup|counter-<em|x>> wird bei Gruppen von den Makros
    <markup|display-in-<em|g>> und <markup|counter-in-<em|g>> übernommen.
    Zusätzlich werden aber zwei weitere Makros definiert
    <markup|ind-display-<em|x>> und <markup|ind-counter-<em|x>>, die die
    Rolle von <markup|display-<em|x>> und <markup|counter-<em|x>> in
    denjenigen Fällen übernimmt, in denen die Gruppe aus individuellen
    Zählern besteht.
  </explain>

  Jederzeit kann man entscheiden, ob die Zähler einer Gruppe einen
  gemeinsamen Gruppenzähler oder sie individuelle Zähler benutzt. Dies wird
  z.B. gebraucht, wenn man zwischen dem Amerikanischen Nummerierungsstil und
  dem europäischen wechselt:

  <\explain|<explain-macro|group-common-counter|g>>
    Benutze einen gemeinsamen Zähler für die Zählergruppe. Dieser wird in der
    Kontextvariablen <src-var|g-nr> gespeichert.
  </explain>

  <\explain|<explain-macro|group-individual-counters|g>>
    Benutze einen individuellen Zähler für jedes Gruppenmitglied. Das ist die
    Vorgabe.
  </explain>

  Es sei darauf hingewiesen, dass Gruppenzähler rekursiv zu Obergruppen
  gehören können. Das zeigen z.B. die folgenden Ausdrücke aus
  <verbatim|env-base.ts>:

  <\tm-fragment>
    <\inactive*>
      <new-counter-group|std-env>
    </inactive*>

    <inactive*|<new-counter-group|theorem-env>>

    <inactive*|<add-to-counter-group|theorem-env|std-env>>

    <inactive*|<group-common-counter|theorem-env>>
  </tm-fragment>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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