<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Literaturverzeichnisse>

  Die folgenden Makros dienen als Marken im Haupt-Text, die auf ein
  Zitat-Eintrag in einer bibliographischen \RDatenbank'' verweisen:

  <\explain|<explain-macro|cite|ref-1|<with|mode|math|\<cdots\>>|ref-n>>
    Jedes Argument <src-arg|ref-i> ist ein Zitat, das zu einem Eintrag in
    einer BiB-<TeX>-Datei gehört. Die Zitate werden genauso dargestellt, wie
    die zugehörigen Einträge in der Datei stehen. Auÿerdem stellen sie
    Hyperlinks zu den Referenzen dar. Wenn der Referenz-Eintrag fehlt, dann
    erscheint anstelle des Zitats ein Fragezeichen. Kann mit
    <menu|Insert|Link|Citation|Visible> eingefügt werden.
  </explain>

  <\explain|<explain-macro|nocite|ref-1|<with|mode|math|\<cdots\>>|ref-n>>
    Ähnlich zu <markup|cite>, aber die Zitate erscheinen nicht im Haupt-Text.
    Kann mit <menu|Insert|Link|Citation|Invisible> eingefügt werden.
  </explain>

  <\explain|<explain-macro|cite-detail|ref|info>>
    Eine ähnliche bibliographische Referenz wie oben. Zusätzlich zur Marke
    <src-arg|ref> können weitere Informationen <src-arg|info> gegeben werden,
    z.B. Kapitel- oder Seiten-Nummer. Kann mit
    <menu|Insert|Link|Citation|Detailed> eingefügt werden.
  </explain>

  Die folgenden Makros können redefiniert werden, wenn Sie die Darstellung
  von Zitaten im Text oder in dem erzeugten Verzeichnis ändern wollen:

  <\explain|<explain-macro|render-cite|ref>>
    Ein Makro zur Darstellung des Zitates <src-arg|ref> am Ort, wo mit
    <markup|cite> zitiert wurde. Der Inhalt kann ein einfaches Zitat wie
    \RTM98'' sein, oder ein Liste von Referenzen wie z.B. \REuler1, Gauss2''.
  </explain>

  <\explain|<explain-macro|render-cite-detail|ref|info>>
    Ähnlich <markup|render-cite>, aber für ausführliche Zitate mit
    Zusatzinformation, die mit <markup|cite-detail> erstellt wurden.
  </explain>

  <\explain|<explain-macro|render-bibitem|content>>
    Momentan werden Bibliographien mit Bib<TeX> gemacht und nach <TeXmacs>
    importiert. Die so erzeugten Verzeichnisse sind Listen von
    bibliographischen Punkten, die auf speziellen <LaTeX>-spezifischen Makros
    (<markup|bibitem>, <markup|block>, <markup|protect>,
    <abbr|<localize|etc>.>) basieren. Diese Makros sind intern in <TeXmacs>
    definiert und rufen im Endeffekt <markup|render-bibitem> auf, welches
    ähnlich wie <markup|item*> arbeitet und welches vom Anwender umdefiniert
    werden kann.
  </explain>

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