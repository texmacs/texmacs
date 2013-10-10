<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Den tmdoc Stil benutzen>

  Neben den Makros über die <hlink|Urheberrechtsinformationen|copyright.de.tm>
  und den <hlink|Leit-Makros|traversal.de.tm>, die bereits dokumentiert
  wurden, kommt der <tmstyle|tmdoc> Stil noch mit einigen anderen Makros, die
  sie benutzen sollten, wann immer es angebracht erscheint:

  <\explain|<markup|key>>
    Dieses Makro wird benutzt, um Keyboard-Eingaben zu kennzeichnen, wie
    <shortcut|(save-buffer)>.
  </explain>

  <\explain|<markup|menu>>
    Dieses Makro kennzeichnet ein Hauptmenü, wie <menu|Datei>.
  </explain>

  <\explain|<markup|submenu>>
    Dieses Makro kennzeichnet ein Untermenü oder einen Menü-Eintrag, wie
    <menu|Dokument|Sprache>.
  </explain>

  <\explain|<markup|subsubmenu>>
    Dieses Makro kennzeichnet ein Unteruntermenü oder ein Untermenü-Eintrag,
    wie <menu|Insert|Farbe|Weiÿ>.
  </explain>

  <\explain|<markup|tmstyle>>
    Dieses Makro kennzeichnet den Namen einer <TeXmacs> Stil-Datei oder eines
    Paketes, wie <tmstyle|article>.
  </explain>

  <\explain|<markup|markup>>
    Dieses Makro wird benutzt um ein Makro oder eine Funktion zu
    kennzeichnen, wie <markup|section>.
  </explain>

  Die folgenden Makros und Funktionen werden für Links und Indizierungen
  benutzt, auch wenn sie in der Zukunft noch verbessert werden sollten:

  <\explain|<markup|simple-link>>
    Dieses Makro nimmt eine URL <math|x> als Argument entgegen und ist dann
    ein Hyperlink mit Namen und Ziel <math|x>.
  </explain>

  <\explain|<markup|hyper-link>>
    Dieses Makro ist ein normaler Hyperlink.
  </explain>

  <\explain|<markup|concept-link>>
    Dieses Makro nimmt ein Konzept als Argument. Später könnte dann ein
    entsprechender Hyperlink automatisch erstellt werden von dieser und
    anderer Dokumentation.
  </explain>

  <\explain|<markup|only-index>>
    Eine einfache Zeichenkette indizieren.
  </explain>

  <\explain|<markup|def-index>>
    Definition eines neuen Konzeptes; der Text wird kursiv angezeigt und
    indiziert.
  </explain>

  <\explain|<markup|re-index>>
    Wiederauftauchen eines schon definierten Konzepts; der Text wird in
    Romanisch angezeigt und indiziert.
  </explain>

  Der <tmstyle|tmdoc> Stil leitet sich aus dem <tmstyle|Brief> Stil ab und
  sollte Makros wie <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|usw.> dieses Stils benutzen, wann immer es passend erscheint.

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
    <associate|language|english>
  </collection>
</initial>