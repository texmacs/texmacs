<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Definition neuer Kontexte>

  Die <tmdtd|env-base> <abbr|D.T.D.> enthält Konstrukte, die man benutzen
  kann, um neue nummerierte Kontexte, wie z.B. <localize|theorem>,
  <localize|remark>, <localize|exercise> und <localize|figure>:

  <\explain|<explain-macro|new-theorem|env-name|display-name>>
    Dieses Meta-Makro wird zur Definition neuer nummerierter
    (Theorem-ähnlicher) Kontexte benutzt. Das erste Argument
    <src-arg|env-name> spezifiziert den Namen des Kontextes z.B.
    \RExperimente'' und <src-arg|display-name> den dazugehörigen Text z.B.
    \RVersuch''. Wenn ein Theorem-ähnlicher Kontext definiert wird, z.B.
    <markup|Experimente>, wird gleichzeitig eine nicht nummerierte Variante
    <markup|Experimente*> automatisch erzeugt.
  </explain>

  <\explain|<explain-macro|new-remark|env-name|display-name>>
    Ähnlich <markup|new-theorem>, aber für <localize|remark>.
  </explain>

  <\explain|<explain-macro|new-exercise|env-name|display-name>>
    Ähnlich <markup|new-theorem>, aber für <localize|exercise>.
  </explain>

  <\explain|<explain-macro|new-figure|env-name|display-name>>
    Ähnlich <markup|new-theorem>, aber für <localize|figure>. Wenn man einen
    neuen Abbildungs-Typ definiert, z.B. \RGemälde'', dann erzeugt das
    <markup|new-figure>-Makro gleichzeitig einen Zeilen-Kontext
    <markup|small-picture> und einen Block-Kontext <markup|big-picture>,
    sowie die nicht nummerierten Varianten <markup|small-picture*> und
    <markup|big-picture*>.
  </explain>

  Die nummerierten (Theorem-ähnlichen) Kontexte gehören alle zu der
  Haupt-Gruppe <verbatim|theorem-env>. Voreinstellung ist die amerikanische
  Art der Nummerierung: ein gemeinsamer Zähler für alle Kontexte. Wenn man
  aber das Paket <tmpackage|number-europe>, <localize|number-europe>, wählen,
  hat jeder Kontext einen eigenen Zähler. Jede <localize|exercise> und jede
  <localize|figure> benutzt eine eigene Zählergruppe.

  Allgemein gilt, dass die <verbatim|std-env> Zählergruppe die Zähler für
  alle <TeXmacs>-Standard-Kontexte umgruppiert. Typischerweise werden alle
  Gruppen auf ähnliche Weise mit einem Präfix, z.B. der Kapitel-Nummer,
  versehen. Abbildung <reference|std-env-fig> zeigt die hierarchische
  Struktur dieser Zählergruppe.

  <\big-figure|<tree|<verbatim|std-env>|<tree|<verbatim|theorem-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|theorem>>>|<row|<cell|<verbatim|proposition>>>|<row|<cell|<verbatim|remark>>>|<row|<cell|<with|mode|math|\<vdots\>>>>>>>>|<tree|<verbatim|exercise-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|exercise>>>|<row|<cell|<verbatim|problem>>>>>>>|<tree|<verbatim|figure-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|figure>>>|<row|<cell|<verbatim|table>>>>>>>|<verbatim|equation>|<verbatim|footnote>>>
    <label|std-env-fig>Organisation der Zähler von
    <TeXmacs>-Standard-Kontexten.
  </big-figure>

  Zusätzlich zu den Standard-Typen, <localize|theorem>-ähnliche,
  <localize|remark>-ähnliche, <localize|exercise>-ähnliche und
  <localize|figure>-ähnliche Kontexte, können weitere nummerierte
  Text-Kontexte mit dem <markup|new-env>-Makro erzeugt werden. Diese können
  auf beliebigen Zählergruppen basieren:

  <\explain|<explain-macro|new-env|group|env|env-name|display-name>>
    Das erste Argument ist der Name der Zählergruppe <src-arg|group>, zu der
    der neue Kontext gehören soll. Das zweite Argument <src-arg|env> ist der
    Name eines binären Makros zur Darstellung des Kontexts. Die Argumente des
    darstellenden Makros sind: ein Name (z.B. \RTheorem 3.14'') und sein
    Rumpf. Die verbleibenden Argumente entsprechen denjenigen von
    <markup|new-theorem>. Beispielsweise wird in den Standard-Basis-Stilen
    <markup|new-theorem> so

    <\tm-fragment>
      <\inactive*>
        <assign|new-theorem|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem>>>
      </inactive*>
    </tm-fragment>
  </explain>

  definiert.

  Es sei daran erinnert, dass man neue Zähler und Zählergruppen zu
  <verbatim|theorem-env> mit Hilfe von <markup|new-counter-group> und
  <markup|add-to-counter-group>, wie das in dem Abschnitt über Zähler erklärt
  wurde.\ 

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