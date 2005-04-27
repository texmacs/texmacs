<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Den tmdoc Stil benutzen>

  Neben den Makros über die <apply|hyper-link|Urheberrechtsinformationen|copy\
  right.de.tm> und den <apply|hyper-link|Leit-Makros|traversal.de.tm>, die
  bereits dokumentiert wurden, kommt der <tmstyle|tmdoc> Stil noch mit
  einigen anderen Makros, die sie benutzen sollten, wann immer es angebracht
  erscheint:

  <\description>
    <expand|item*|<markup|key>>Dieses Makro wird benutzt, um
    Keyboard-Eingaben zu kennzeichnen, wie <key|C-x C-s>.

    <expand|item*|<markup|menu>>Dieses Makro kennzeichnet ein Hauptmenü, wie
    <apply|menu|Datei>.

    <expand|item*|<markup|submenu>>Dieses Makro kennzeichnet ein Untermenü
    oder einen Menü-Eintrag, wie <apply|menu|Dokument|Sprache>.

    <expand|item*|<markup|subsubmenu>>Dieses Makro kennzeichnet ein
    Unteruntermenü oder ein Untermenü-Eintrag, wie
    <apply|menu|Text|Farbe|Weiÿ>.

    <expand|item*|<markup|tmstyle>>Dieses Makro kennzeichnet den Namen einer
    <TeXmacs> Stil-Datei oder eines Paketes, wie <tmstyle|article>.

    <expand|item*|<markup|markup>>Dieses Makro wird benutzt um ein Makro oder
    eine Funktion zu kennzeichnen, wie <markup|section>.
  </description>

  Die folgenden Makros und Funktionen werden für Links und Indizierungen
  benutzt, auch wenn sie in der Zukunft noch verbessert werden sollten:

  <\description>
    <expand|item*|<markup|simple-link>>Dieses Makro nimmt eine URL
    <with|mode|math|x> als Argument entgegen und ist dann ein Hyperlink mit
    Namen und Ziel <with|mode|math|x>.

    <expand|item*|<markup|hyper-link>>Dieses Makro ist ein normaler
    Hyperlink.

    <expand|item*|<markup|concept-link>>Dieses Makro nimmt ein Konzept als
    Argument. Später könnte dann ein entsprechender Hyperlink automatisch
    erstellt werden von dieser und anderer Dokumentation.

    <expand|item*|<markup|only-index>>Eine einfache Zeichenkette indizieren.

    <expand|item*|<markup|def-index>>Definition eines neuen Konzeptes; der
    Text wird kursiv angezeigt und indiziert.

    <expand|item*|<markup|re-index>>Wiederauftauchen eines schon definierten
    Konzepts; der Text wird in Romanisch angezeigt und indiziert.
  </description>

  Der <tmstyle|tmdoc> Stil leitet sich aus dem <tmstyle|Brief> Stil ab und
  sollte Makros wie <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|usw.> dieses Stils benutzen, wann immer es passend erscheint.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
