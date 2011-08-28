<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Nummerierte Kontexte nutzen>

  Die <tmdtd|env-theorem> <abbr|D.T.D.> enthält die nummerierten
  (Theorem-ähnlichen) Standard-Kontexte und andere Standard-Text-Kontexte,
  die über das Menü <menu|Insert|Environment> erreicht werden können. Sie
  können in drei Haupt-Kategorien unterteilt werden:

  <\description>
    <item*|Theorem-(<localize|theorem>)-Varianten>Die Rümpfe dieser Kontexte
    werden normalerweise in besonderer Weise hervorgehoben. <TeXmacs> stellt
    folgende Kontexte bereit: <markup|theorem> (<localize|theorem>),
    <markup|proposition> (<localize|proposition>), <markup|lemma>
    (<localize|lemma>), <markup|corollary> (<localize|corollary>),
    <markup|axiom> (<localize|axiom>), <markup|definition>
    (<localize|definition>), <markup|notation> (<localize|notation>),
    <markup|conjecture> (<localize|conjecture>), die über das Menü
    <menu|Insert|Environment> zugänglich sind.

    <item*|<localize|remark>-Varianten>Im Menü<menu|Insert|Environment> lassen
    sich folgende erzeugen: <markup|remark> (<localize|remark>),
    <markup|example> (<localize|example>), <markup|note> (<localize|note>),
    <markup|warning> (<localize|warning>), <markup|convention>
    (<localize|convention>).

    <item*|<localize|exercise>-Varianten>Zwei solche Kontexte besitzt
    <TeXmacs>, die über das Menü <menu|Insert|Environment> erzeugt werden
    können: <markup|exercise> (<localize|exercise>) und <markup|problem>
    (<localize|problem>).
  </description>

  Alle diese Kontexte gibt es auch in einer unnummerierten Variante
  \ <markup|theorem*>, <markup|proposition*>, <abbr|usw.>. Man kann den
  Kurzbefehl \ <shortcut|(numbered-toggle (focus-tree))> \ benutzen, um zwischen der nummerierten und der
  unnummerierten Variante zu wechseln. Auÿerdem gibt es noch:

  <\explain|<explain-macro|proof|body>>
    Für Beweise (von Sätzen).
  </explain>

  <\explain|<explain-macro|dueto|who>>
    Ein Makro, das dazu dient die Quelle des Satzes zu kennzeichnen. Es
    sollte innerhalb eines Satz-Kontexts verwendet werden, z.B.

    <\theorem*>
      <dueto|Pythagoras><with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
    </theorem*>
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