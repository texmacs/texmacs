<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Abschnitt-Kontexte nutzen>

  Die <tmdtd|section-base> <abbr|D.T.D.> stellt die Standard-Befehle für
  Abschnitte zur Verfügung, und zwar die gleichen wie in <LaTeX>. Die meisten
  Abschnitts-Befehle haben ein Argument: den Namen des Abschnitts.\ 

  <underline|Mit den folgenden Befehlen werden nummerierte Abschnitte
  erzeugt:>

  <\explain>
    <explain-macro|chapter|title>

    <explain-macro|section|title>

    <explain-macro|subsection|title>

    <explain-macro|subsubsection|title>

    <explain-macro|paragraph|title>

    <explain-macro|subparagraph|title>

    <explain-macro|appendix|title>
  <|explain>
    Diese Makros erzeugen nummerierte Titel für Kapitel usw.. Die
    Nummerierung ist nicht notwendig, sie ist aber eine wichtige Option.
    Absätze, <markup|paragraph>, und Unter-Absätze, <markup|subparagraph>,
    werden normalerweise nicht nummeriert, können es aber sein. Manche
    Basis-Stile, z.B. <localize|generic> kennen überhaupt keine Nummerierung.
  </explain>

  Befehle <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*>, <markup|subparagraph*> und
  <markup|appendix*> dienen zur Erzeugung der unnummerierten Varianten.

  Als Vorgabe produzieren die Abschnitts-Konstrukte nur die Abschnitts-Titel.
  Wenn man aber das experimentelle Paket <tmpackage|structured-section>
  (<localize|structured-section>) benutzt, dann übernehmen alle
  Abschnitts-Befehle einen Abschnitts-Rumpf als weiteres Argument. Dazu gibt
  es den zusätzlichen Befehl <markup|rsection>, um rekursiv eingebettete
  Abschnitte zu erzeugen. So verhält sich beispielsweise eine
  <markup|rsection> innerhalb eines Abschnitts <markup|section> wie ein
  Unter-Abschnitt <markup|subsection>. Wir planen alle Abschnitte
  <em|strukturiert> zu gestalten.

  <underline|Die <tmdtd|section-base> <abbr|D.T.D.> liefert auÿerdem die
  folgenden Abschnitt-artigen Kontexte für automatisch erzeugte
  Verzeichnisse:>

  <\explain|<explain-macro|bibliography|aux|style|file-name|body>>
    Dieses Makro dient zur Erstellung von Literaturverzeichnissen. Das erste
    Argument <src-arg|aux> spezifiziert einen <em|auxiliary channel> mit den
    Daten zur Erzeugung der Bibliographie (<verbatim|bib> als Vorgabe). Die
    Argumente <src-arg|style> und <src-arg|file-name> sind der
    Verzeichnis-Stil und die Datei mit der bibliographischen Datenbasis. Der
    Rumpf <src-arg|body> entspricht dem automatisch erzeugten Verzeichnis.
  </explain>

  <\explain|<explain-macro|table-of-contents|aux|body>>
    Dieses Makro erzeugt Inhaltsverzeichnisse. Das erste Argument
    <src-arg|aux> spezifiziert einen <em|auxiliary channel> mit den Daten zur
    Erzeugung der Bibliographie (<verbatim|toc> als Vorgabe). Der Rumpf
    <src-arg|body> entspricht dem automatisch erzeugten Verzeichnis.
  </explain>

  <\explain|<explain-macro|the-index|aux|body>>
    Ähnlich <markup|table-of-contents> aber für Stichwortverzeichnisse mit
    dem Vorgabe-<em|auxiliary channel> \ <verbatim|idx>.
  </explain>

  <\explain|<explain-macro|the-glossary|aux|body>>
    Ähnlich <markup|table-of-contents> aber für Glossare mit dem
    Vorgabe-<em|auxiliary channel> <verbatim|gly>.
  </explain>

  Die vorstehenden Befehle haben die Varianten <markup|bibliography*>,
  <markup|table-of-contents*>, <markup|the-index*> und <markup|the-glossary*>
  mit dem zusätzlichen Argument <src-arg|name> vor dem Argument
  <src-arg|body>. <src-arg|name> spezifiziert den Namen des Abschnitts. Der
  <markup|the-glossary*>-Befehl wird z.B. zur Erzeugung der Liste der
  Abbildungen und der Liste der Tabellen benutzt.

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