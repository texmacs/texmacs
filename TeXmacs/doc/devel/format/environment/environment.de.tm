<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Vordefinierte Kontextvariable>

  Die Art und Weise wie <TeXmacs> den <hyper-link|Schriftsatz|../basics/typesetting.de.tm>
  von Dokumenten durchführt, wird durch sogenannte <em|Kontextvariablen>
  beeinflusst. Die <hyper-link|Stildefinitions-Sprache|../stylesheet/stylesheet.de.tm>
  benutzt einen sogenannten <em|Kontext>, um dort Kontextvariable und
  <hyper-link|Makros|../stylesheet/prim-macro.de.tm> zu speichern.
  Kontextvariablen lassen sich in zwei Klassen einteilen: vordefinierte
  Variablen und zusätzliche Variablen, die durch Stildefinitionen
  bereitgestellt werden. Vordefinierte Variablen beeinflussen in der Regel
  das eigentliche Layout, während die zusätzlichen Variablen mehr zu
  Berechnungen dienen. In den nächsten Abschnitten werden wir alle
  vordefinierten Kontextvariablen beschreiben.

  Eine typische vordefinierte Kontextvariable ist <src-var|color>. Der Wert
  dieser Variablen kann dauerhaft <hyper-link|geändert|../stylesheet/prim-env.de.tm>
  werden, mit dem Befehl <markup|assign> und vorübergehend (lokal) mit dem
  Konstrukt <markup|with>:

  <\tm-fragment>
    <with|color|dark red|Roter> Text.
  </tm-fragment>

  <\tm-fragment>
    <inactive*|<with|color|dark red|Roter> Text.>
  </tm-fragment>

  Zähler sind typische Kontextvariablen von Stildefinitionen:

  <\tm-fragment>
    <\enumerate>
      <item>eine verrückt

      <assign|item-nr|3><item>nummerierte Liste ...
    </enumerate>
  </tm-fragment>

  <\tm-fragment>
    <inactive*|<\enumerate>
      <item>Eine verrückt

      <assign|item-nr|3><item>nummerierte Liste ...
    </enumerate>>
  </tm-fragment>

  Die Schriftsatz-sprache benutzt <def-index|dynamische Kontextbereiche> für
  ihre Variablen. Das bedeutet, dass Makros auf Kontextvariablen, die den
  Kontext betreffen, indem sie aufgerufen wurden, zugreifen können und diese
  modifizieren dürfen. Im obigen Beispiel hat das <markup|enumerate>-Makro
  lokal die Variable <src-var|item-nr> auf <with|mode|math|0> gesetzt (dabei
  hat es <markup|with> benutzt) dann inkrementiert das <markup|item>-Makro um
  1 und zeigt den Wert. Im folgenden wird durch <markup|assign> auf 3 gesetzt
  und durch <markup|item>-Makro um 1 inkrementiert und angezeigt. Der
  Original-Wert von <src-var|item-nr> wird beim Verlassen von
  <markup|enumerate> wiederhergestellt.

  Jede Dokument kommt mit einem <hyper-link|Startkontext|../basics/tm-docs.de.tm>
  mit vorgegebenen Werten für die Kontextvariablen, das sind also Werte, die
  gesetzt werden, bevor mit dem Schriftsetzen begonnen wird. Wenn eine
  Kontextvariable in diesem Start-kontext noch nicht vorhanden ist, dann wird
  sie auf ihren Vorgabewert gesetzt, nachdem der Dokument-Stil gesetzt wurde
  und ggfs. weitere Stilpakete geladen wurden. Der Start-kontext selbst ist
  Teil des Editors.

  Einige Variablen wie Kopf- und Fuÿzeilen, müssen innerhalb des Dokuments
  gesetzt werden. Ihre Startwerte werden ignoriert. Sie sollten generell
  immer mit den Fuÿ- und Kopfzeilen-Befehlen gesetzt werden.

  <\traverse>
    <branch|Allgemeine Kontextvariable|env-general.de.tm><with|language|german|>

    <branch|Festlegung der aktuellen Schriftart|env-font.de.tm>

    <branch|Mathematischer Schriftsatz|env-math.de.tm>

    <branch|Absatz-Layout|env-par.de.tm>

    <branch|Seitenlayout|env-page.de.tm>

    <branch|Tabellen-Layout|env-table.de.tm>

    <branch|Quellcode editieren|env-src.de.tm>

    <branch|Weitere Kontextvariable|env-misc.de.tm>
  </traverse>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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
    <associate|sfactor|4>
  </collection>
</initial>