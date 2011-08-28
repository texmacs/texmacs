<TeXmacs|1.0.4.2>

<style|<tuple|tmdoc|>>

<\body>
  <tmdoc-title|<TeXmacs>-Standard-Pakete>

  <TeXmacs> stellt eine Anzahl von Paketen bereit, mit denen sich das
  Verhalten der Standard-Stile ändern lässt:

  <\description>
    <item*|<tmpackage|number-long-article>><strong|=
    <localize|number-long-article>:> Dieses Paket sorgt dafür, dass in allen
    nummerierten Kontexten (<localize|<active*|theorem, remark, equations,
    figure, etc.>>) die Nummern mit der aktuellen Abschnitts-Nummer als
    Präfix versehen werden. Dieses Paket wird meist mit dem Basis-Stil
    <compound|localize|article>, <tmstyle|article>, oder <localize|book>,
    <tmstyle|book>, benutzt.

    <item*|<tmpackage|number-europe>><strong|= <localize|number-europe>:>
    Normalerweise benutzt <TeXmacs> den Amerikanischen Nummerierung-Stil,
    d.h., dass ein und derselbe Zähler für alle ähnlichen Zähler wie z.B.
    <localize|theorem> <inactive|>oder <localize|proposition> verwendet
    werden. In anderen Worten eine <localize|remark>, die auf
    \R<localize|theorem> 3`` folgt, hat die Nummerierung \R<localize|remark>
    4``. Wenn Sie für jeden dieser Fälle einen eigenen Zähler haben wollen,
    müssen Sie das Paket <localize|number-europe>,
    \ <tmpackage|number-europe>, wählen.

    <item*|<tmpackage|number-us>><strong|= <localize|number-us>:> Dieses
    Paket kann benutzt werden, um zum amerikanischen Stil der Nummerierung
    zurückzukehren, wenn ein von Dritten stammendes Stil-Paket europäische
    Nummerierung erzwingt.

    <item*|<tmpackage|structured-list>><strong|= <localize|structured-list>:>
    Das ist ein noch ein Experiment. Normalerweise haben unnummerierte Listen
    keine Argumente und Punkte in Beschreibungen ein Argument. Wenn man das
    Paket <tmpackage|structured-list> verwendet, dann sie können ein weiteres
    optionales Argument erhalten.

    <item*|<tmpackage|structured-section>><strong|=
    <localize|structured-section>:> Das ist ein noch ein Experiment.
    Normalerweise haben Abschnitte nur den Titel als Argument. Wenn man
    <tmpackage|structured-section> verwendet, können sie ein weiteres
    Argument annehmen. Auÿerdem kann das Konstrukt <markup|rsection> rekursiv
    verwendet werden.

    <item*|<tmpackage|varsession>><strong|= <localize|varsession>:> Dieses
    Paket dient dazu, interaktive Sitzungen, bei denen <TeXmacs> als
    Schnittstelle und Oberfläche für andere Programme dient, anders
    darzustellen. Die Darstellung ist für interaktive Sitzungen geeignet,
    aber weniger gut für den Druck.
  </description>

  Zusätzlich zu den genannten Paketen und den vielen Paketen für den internen
  <TeXmacs>-Gebrauch, gibt es in <TeXmacs> ein paar persönliche
  Beispiel-Pakete: <tmpackage|allouche>, <tmpackage|bpr> und <tmpackage|vdh>
  sowie verschiedene Pakete Stil-Pakete zur Benutzung mit externen Plug-Ins
  (<tmpackage|axiom>, <tmpackage|giaca>, <tmpackage|macaulay2>,
  <abbr|<localize|etc.>>).

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