<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Listenkontexte anpassen>

  Listen bestehen aus zwei verschiedenen Bestandteilen: der äuÿeren
  Listenstruktur z.B. Ränder und der inneren Struktur, den einzelnen Punkten.
  Die Listenkontexte können durch Um- oder Neudefinition der
  Darstellungsmakros oder durch Definition zusätzlicher Makros, die zur
  gleichen abstrakten Schnittstelle (D.T.D.) passen, geändert werden.

  Die Darstellung der äuÿeren Listenstruktur wird durch das
  <markup|render-list>-Makro gesteuert, das den Rumpf der Liste als Argument
  übernimmt. Betrachten Sie die folgenden Umdefinition von
  <markup|render-list>:

  <\tm-fragment>
    <inactive*|<assign|render-list|<macro|body|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|0.5fn>|<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|<arg|body>>>>>>>
  </tm-fragment>

  Diese Umdefinition verändert die Darstellung aller Listen
  (<translate|itemize|english|german>, <translate|enumerate|english|german>,
  usw.), indem der rechte Rand um <verbatim|3fn> reduziert wird:

  <\with|render-list|<macro|body|<surround|<no-page-break*><vspace*|0.5fn>|<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|<arg|body>>>>>
    <\itemize>
      <item>Dieser Text, der zu lang ist, um auf eine einzelne Zeile zu
      passen, wird auf der rechten Seite um <verbatim|3fn> eingezogen.

      <\enumerate>
        <item>Dieser Text wird zusätzlich um <verbatim|3fn> eingezogen, weil
        er sich in einer Unterliste befindet.
      </enumerate>

      <item>Und wiederum: Dieser Text, der zu lang ist, um auf eine einzelne
      Zeile zu passen, wird auf der rechten Seite um <verbatim|3fn>
      eingezogen.
    </itemize>
  </with>

  In ähnlicher Weise kann man die innere Listenstruktur, die einzelnen
  Punkte, konfigurieren, indem man die Makros <markup|aligned-item> und
  <markup|compact-item> benutzt. Beide Makros haben ein Argument, das die
  Kennzeichnung (kennzeichnenden Text) des Listenpunktes übergibt.
  <markup|aligned-item> für \Rausgerichtete'' Punkte, setzt an einer festen
  Stelle in Bezug auf den Seitenrand die Trennmarke, stellt die Kennzeichnung
  rechtsbündig von der Trennmarke dar. Dadurch ist der Platz für die
  Kennzeichnung beschränkt. Dann positioniert es den Text rechts von der
  Trennmarke. Dagegen positioniert <markup|compact-item> die Kennzeichnung
  linksbündig an den Seitenrand, dann die Trennmarke und fügt den Textinhalt
  daran rechts anschlieÿend ein. So sind lange Kennzeichnungen möglich. Die
  folgende Umdefinition von <markup|aligned-item>

  <\tm-fragment>
    <inactive*|<assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<with|color|red|<arg|x>>|r-2.5fn||r+0.5fn|>>>>>
  </tm-fragment>

  stellt die Kennzeichnung aller Listen-Kontexte mit Punkten des
  \Rkompakten`` Typs rot dar:

  <\with|aligned-item|<macro|x|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<with|color|red|<arg|x>>|r-2.5fn||r+0.5fn|>>>
    <\itemize>
      <item>Diese Liste und alle Listen mit \Rausgerichteten`` Punkten haben
      rote Kennzeichnung.

      <\description-aligned>
        <item*|C1>Erste Bedingung.

        <item*|C2>Zweite Bedingung.
      </description-aligned>

      <item>Die folgenden Punkte mit \Rkompakten`` Punkten benutzen
      <markup|compact-item> und bleiben unverändert.

      <\description-compact>
        <item*|Pferde und Hunde>Liebe Tiere.

        <item*|Mücken und Fliegen>Nicht so nett.
      </description-compact>
    </itemize>
  </with>

  <\remark>
    Die Makros <markup|aligned-item> und <markup|compact-item> müssen
    Zeileninhalt produzieren, damit man sie benutzen kann, um damit
    Blockinhalte zu umgeben. Eine Reihe von anderen internen Makros
    (<markup|aligned-space-item>, <markup|long-compact-strong-dot-item>,
    <abbr|usw.>) basieren auf <markup|aligned-item> und
    <markup|compact-item>, und werden für eine groÿe Zahl verschiedener Arten
    von Listen benutzt (<markup|itemize-arrow>, <markup|description-long>,
    <abbr|<localize|etc.>>). Für die Zukunft planen wir, <markup|item> und
    <markup|item*> mit einem notwendigen Rumpfargument, <src-arg|body>, zu
    versehen. Man sollte das berücksichtigen, wenn man Listen-Kontexte
    entwirft, um den Code aufwärts-kompatibel zu halten.
  </remark>

  Die <tmdtd|std-list> <abbr|D.T.D.> stellt ein Makro <markup|new-list>
  bereit, mit dem neue Listen definiert werden können. Seine Syntax ist
  <explain-macro|new-list|name|item-render|item-transform>. <src-arg|name>
  ist der Name des neuen Listen-Konstrukts, \ <src-arg|item-render> ein
  (Zeilen)-Makro zur Darstellung und <src-arg|item-transform> eine
  zusätzliche Transformation, die auf den zu diesem Punkt gehörigen Text
  angewendet wird. So kann man z.B. einen Kontext <markup|enumerate-roman>,
  wie folgt, definieren:

  <\tm-fragment>
    <\inactive*>
      <new-list|enumerate-roman|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|roman>>>
    </inactive*>
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
    <associate|preamble|false>
  </collection>
</initial>