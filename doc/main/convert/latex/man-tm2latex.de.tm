<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Konvertierung von <TeXmacs> zu <LaTeX>>

  Die vermutlich am häufigsten auftretende Situation ist, dass Sie einen
  Artikel von <TeXmacs> zu <LaTeX> konvertieren möchten, um ihn in einem
  Journal o.ä. zu veröffentlichen. Sie exportieren die <TeXmacs>-Datei
  <kbd|name.tm> über <menu|File|Export|Latex> in die <LaTeX>-Datei
  <kbd|name.tex>. Zunächst sollten Sie dann einen <LaTeX>-Durchlauf auf
  <kbd|name.tex> ausführen und prüfen ob das Ergebnis bereits Ihren
  Anforderungen entspricht. Wenn dem so ist, geben Sie die Datei
  <kbd|name.tex> zusammen mit der Style-Datei <kbd|TeXmacs.sty>, die sie
  unter <kbd|$TEXMACS_PATH/misc/latex> finden, weiter.

  Das Journal an welches Sie Ihr Dokument weitergeben möchten benutzt
  möglicherweise seine eigene Style-Datei, beispielsweise <kbd|journal.sty>.
  In diesem Fall sollten Sie auch die Datei

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/styles/article.ts
  </verbatim>

  nach

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/styles/journal.ts
  </verbatim>

  kopieren und <kbd|journal> als Dokumentstil über
  <menu|Document|Style|Other> benutzen. Optional können Sie <kbd|journal.ts>
  auch weiter bearbeiten, so dass das Layout des Artikels besser zu dem Stil
  des Journals passt. In manchen Fällen müssen Sie auÿerdem eine neue Kopie
  von <kbd|TeXmacs.sty> erstellen und einige der Umgebungen modifizieren
  damit sie zu der Style-Datei <kbd|journal.sty> kompatibel sind.

  Wenn Ihr erster Versuch das Dokument zu <LaTeX> zu konvertieren zu keinem
  akzeptablen Ergebnis geführt hat, werden Sie normalerweise feststellen,
  dass nur kleine Teile des Textes nicht korrekt übersetzt wurden. Dies ist
  hauptsächlich auf drei Ursachen zurückzuführen:

  <\itemize-dot>
    <item>In Ihrem Text benutzen Sie spezielle <TeXmacs>-Features.

    <item>Sie nutzen ein <TeXmacs>-Feature dass noch nicht in dem
    Konvertierungsalgorithmus implementiert wurde.

    <item>Sie haben einen Bug im Konvertierungsalgorithmus gefunden.
  </itemize-dot>

  Diese Punkte werden im nächsten Abschnitt näher besprochen.

  In Problemfällen wäre die naheliegenste Möglichkeit, die produzierte
  <LaTeX>-Datei zu korrigieren und sie dann zum Journal zu schicken. Diese
  Strategie hat aber den Nachteil dass Sie die Korrekturen immer und immer
  wieder durchführen müssen, jedes mal wenn Sie die <TeXmacs>-Datei
  <kbd|name.tm> konvertieren nachdem Sie einige zusätzliche Modifikationen
  vorgenommen haben. Eine bessere Lösung ist es, die Konstruktionen über
  <menu|Insert|Specific|Latex> und <menu|Insert|Specific|Texmacs> zu
  verwenden um Text zu schreiben der nur in der konvertierten bzw. der
  original Datei zu sehen ist.

  Nehmen Sie an dass beispielsweise das Wort blauwbilgorgel`` in der
  <TeXmacs>-Datei korrekt getrennt wird, nicht aber in der
  <LaTeX>-Übersetzung. Sie könnten dann wie folgt vorgehen:

  <\enumerate-numeric>
    <item>Markieren Sie blauwbilgorgel``.

    <item>Wählen Sie <menu|Insert|Specific|Texmacs> um den Text
    blauwbilgorgel`` <TeXmacs>-spezifisch zu machen.

    <item>Wählen Sie <menu|Einfügen|Specific|Latex>.

    <item>Schreiben Sie den <LaTeX>-Code <kbd|blauw\\-bil\\-gor\\-gel> mit
    der korrekten Silbentrennung.

    <item>Drücken Sie <key|return> um den <LaTeX>-spezifischen Text zu
    aktivieren.
  </enumerate-numeric>

  Auf gleichem Wege können Sie <LaTeX>-spezifische Zeilenumbrüche,
  Seitenumbrüche, vertikalen Leerraum, Stil-Parameter-Modifikationen usw.
  einfügen.

  <tmdoc-copyright|1998-2004|Joris van der Hoeven, Christoph Strobel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|page-type|a4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|german>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-5|<tuple|2.|?>>
    <associate|idx-6|<tuple|3.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>