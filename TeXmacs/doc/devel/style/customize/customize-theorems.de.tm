<TeXmacs|1.0.4.5>

<style|<tuple|tmdoc|number-europe>>

<\body>
  <tmdoc-title|Nummerierte Text-Kontexte>

  <TeXmacs> verfügt über drei verschiedene nummerierte Standard-Kontexte für
  Text: <translate|theorem|english|german>-ähnliche,
  <translate|remark|english|german>-ähnliche und
  <translate|exercise|english|german>-ähnliche. Die folgenden Aspekte können
  leicht angepasst werden:

  <\itemize>
    <item>Neue Konstrukte erzeugen.

    <item>Die Darstellung ändern.

    <item>Die Nummerierung ändern.
  </itemize>

  <paragraph*|Neue Konstrukte erzeugen>

  Mit den Meta-Makros <markup|new-theorem>, <markup|new-remark> und
  <markup|new-exercise> können neue nummerierte Kontexte erzeugt werden. Sie
  alle haben zwei Argumente, den Namen des neuen Konstrukts und die
  Bezeichnung, die in der Darstellung auf dem Bildschirm verwendet werden
  soll. Wenn Sie beispielsweise einen Kontext für Experimente erzeugen
  wollen, der mit Experiment 1 usw. fortlaufend beschriftet wird, dann
  definieren Sie <markup|experiments> mit

  <\tm-fragment>
    <inactive*|<new-theorem|experiments|Experiment>>
  </tm-fragment>

  Wenn Experiment in dem geeigneten <TeXmacs>-Wörterbuch enthalten ist, wird
  der Text \RExperiment'' automatisch übersetzt. Im Abschnitt
  <hyper-link|Definition neuer Kontexte|../../../main/styles/std-dtds/env-base-dtd.de.tm>
  wird u.a. beschrieben, wie man neue nummerierte Kontexte schreiben kann,
  die nicht zu den <translate|theorem|english|german>-ähnlichen,
  <translate|remark|english|german>-ähnlichen und
  <translate|exercise|english|german>-ähnlichen gehören.

  <paragraph*|Die Darstellung ändern>

  Die Darstellung dieser Kontexte kann mit den Konstrukten
  <markup|render-theorem>, <markup|render-remark> und
  <markup|render-exercise> beeinflusst werden. Diese Makros haben als
  Argumente den Namen des Kontexts (z.B. \Rtheorem 1.2'') und seinen Rumpf.
  Eine <translate|remark|english|german> wird gemäÿ Vorgabe genau wie ein
  <translate|theorem|english|german> gesetzt \ allerdings in der Schriftform
  \R<translate|upright|english|german>``. Daher beeinflussen Änderungen der
  Definition von <markup|render-theorem> auch die Darstellung einer
  <translate|remark|english|german>. Wenn man beipielsweise von einem
  <translate|theorem|english|german> verlangt, dass er etwas eingerückt und
  <translate|slanted|english|german> dargestellt wird, dann kann man
  <markup|render-theorem> so umdefinieren:

  <\tm-fragment>
    <inactive*|<assign|render-theorem|<macro|which|body|<style-with|src-compact|none|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<right-flush><vspace|1fn>|<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>>>>
  </tm-fragment>

  Das führt zu folgender Darstellung:

  <\with|render-theorem|<macro|which|body|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<right-flush><vspace|1fn>|<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>>
    <\theorem>
      Das ist ein <translate|theorem|english|german>, der
      <translate|slanted|english|german> und eingerückt dargestellt wird.
    </theorem>

    <\remark>
      Die Darstellung einer <translate|remark|english|german> basiert auf der
      Darstellung des <translate|theorem|english|german>-Kontexts nur, dass
      die Schriftform \R<translate|upright|english|german>`` verwendet wird.
    </remark>
  </with>

  Manchmal möchte man aber nur die Darstellung der Bezeichnung oder des
  Trennzeichens zwischen Bezeichnung und Text-Rumpf ändern. Wie man im
  vorstehenden Beispiel erkennen kann, werden diese Aspekte durch die Makros
  <markup|theorem-name> und <markup|theorem-sep> gesteuert. Beispielsweise
  wird mit

  <\tm-fragment>
    <inactive*|<assign|theorem-name|<macro|name|<with|color|dark
    red|font-series|bold|<arg|name>>>>>

    <inactive*|<assign|theorem-sep|<macro|: >>>
  </tm-fragment>

  \ eine <translate|proposition|english|german>, wie folgt, dargestellt:\ 

  <\with|theorem-name|<macro|name|<with|color|dark
  red|font-series|bold|<arg|name>>>|theorem-sep|<macro|: >>
    <\proposition>
      Diese <translate|proposition|english|german> hat eine ungewöhnliche Art
      der Darstellung.
    </proposition>
  </with>

  <paragraph*|Die Nummerierung ändern>

  In den Abschnitten über <hyper-link|Zähler und
  Zählergruppen|../../../main/styles/std/std-counter-dtd.de.tm> wird erklärt,
  wie man Zähler zu einen bestimmten Zweck ändern kann. Beispielsweise können
  Sie für den Kontext \R<localize|corollary>`` den Zähler zurücksetzen, indem
  Sie <markup|inc-theorem> neu definieren:

  <\tm-fragment>
    <inactive*|<style-with|src-compact|none|<quasi|<style-with|src-compact|none|<assign|inc-theorem|<macro|<compound|<unquote|<value|inc-theorem>>><reset-corollary>>>>>>>
  </tm-fragment>

  Beachten Sie den Trick mit <markup|quasi> und <markup|unquote>, um alle
  Aktionen zu berücksichtigen, die von den früheren Werten des Makros
  <markup|inc-theorem> stammen können.

  Der folgende Code von <verbatim|number-long-article.ts> dient dazu, allen
  Standard-Kontexten die Nummer des aktuellen Abschnitts als Praefix
  voranzustellen.

  <\tm-fragment>
    <inactive*|<assign|section-clean|<macro|<reset-subsection><reset-std-env>>>>

    <inactive*|<assign|display-std-env|<macro|nr|<section-prefix><arg|nr>>>>
  </tm-fragment>

  \;

  Beachten Sie auch, dass mit den Paketen <verbatim|number-europe.ts>,
  <verbatim|number-long-article.ts>, <verbatim|number-us.ts>,
  structured-list.ts (<localize|number-europe, number-long-article,
  number-us, structured-list und structured-section>) die Nummerierung im
  Menü <menu|View|Add package|Customize> angepasst werden kann.

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
    <associate|preamble|false>
  </collection>
</initial>