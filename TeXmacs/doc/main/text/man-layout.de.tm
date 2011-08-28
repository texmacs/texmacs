<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|<menu|Layout issues>>

  Es gilt allgemein, \R<TeXmacs> sorgt für das Layout Ihres Textes''. Deshalb
  sollten Sie vermeiden, Ihr Dokument manuell zu gestalten, indem Sie
  \ Leerzeichen oder Leerzeilen über die Tastatur eingeben, um horizontale
  und vertikale Abstände zu erzeugen, obwohl dies nicht verboten ist und oft
  der Gewohnheit entspricht. Sie sollten dafür die Befehle aus dem Menü
  <menu|Format|Spaces> verwenden. Das hat den Vorteil, dass bei kleinen
  Änderungen, die den Zeilen- oder Seitenumbruch verändern, und auch bei
  groÿen wie einem Wechsel des Dokumentstiles, die Darstellung des Textes
  unverändert bleibt. Wenn Sie sich daran halten, müssen Sie nach solchen
  Änderungen nicht den ganzen Text kontrollieren und anpassen.

  Mehrere Typen von expliziten Abstandsbefehlen wurden implementiert. Einmal
  gibt es <em|fixe> Abstände, die eine vorgegebene Gröÿe haben und behalten.
  Sie müssen ihre Länge in <menu|Standard length units> eingeben. Mehr dazu
  finden Sie unter <hlink|<menu|Standard length
  units>|../../devel/format/basics/lengths.de.tm>. Zum anderen gibt es
  variable Abstände, deren Länge vom Zeilenumbruch abhängt. Weiterhin wird
  unterschieden zwischen horizontalen und vertikalen Abständen. Horizontale
  Abstände haben Breite aber keine Höhe. Sie können variabel oder fix sein.
  Vertikale Abstände können vor oder nach einem Absatz eingefügt werden. Der
  Abstand zwischen zwei Absätzen ist das Maximum von zwei Abständen und zwar
  dem nach dem vorderen Absatz spezifizierten Abstand einerseits und dem vor
  dem folgenden Absatz spezifizierten Abstand andererseits. Er ist also
  <strong|nicht> die Summe beider im Gegensatz zu <TeX>.

  Auÿerdem können Sie mit der Option <menu|Tab> Tabulatoren einfügen. Die
  Funktion der Tabulatoren ist anders als gewöhnlich. Fügt man nur einen
  Tabulator ein, so wird der nachfolgende Text rechtsbündig an dem rechten
  Rand eingeführt. Wird ein zweiter Tabulator hinzugefügt, so entsteht eine
  zweigeteilte Zeile usw., z.B.:

  kein Tab<htab|5mm>zwei

  kein Tab<htab|5mm>ein<htab|5mm>zwei

  kein Tab<htab|5mm>ein<htab|5mm>zwei<htab|5mm>drei

  Absätze können linksbündig, rechtsbündig, zentriert oder als Blocksatz
  formatiert werden. Auÿerdem können für jeden Absatz der linke und der
  rechte Seitenrand und der Erstzeileneinzug sowie die Zeilenabstände
  innerhalb des Absatzes spezifiziert werden. Wenn für einen gröÿeren
  Textbereich ein Erstzeileneinzug formatiert wurde, dann kann durch
  Einzugsmarken der Einzug ab- bzw. angeschaltet werden. Dabei gibt es zwei
  verschiedene Marken, die auf den vorgehenden Absatz bzw. den nachfolgenden
  Absatz wirken. Genaueres finden Sie <hlink|<menu|Hier>|../../devel/format/regular/prim-indent.de.tm>.

  Im Menü <menu|Document|Page> können Sie das Seitenlayout festlegen. Die Art
  und Weise der Bildschirmdarstellung können Sie mit den Optionen unter
  <menu|Document|Page|Type> beeinflussen. Wenn Sie z.B. <menu|Paper> wählen,
  werden die Seitenumbrüche explizit dargestellt. Die Voreinstellung ist
  <menu|Papyrus>. Damit werden Seitenumbrüche nicht auf dem Bildschirm
  gezeigt und die Darstellung ist schneller. Die Einstellung <menu|Automatic>
  unterstellt, dass das Bildschirmfenster exakt der Papiergröÿe entspricht.
  Mehr dazu finden Sie <hlink|<menu|Hier>|../../devel/format/environment/env-page.de.tm>.
  Die Ränder (oben, unten, links, rechts) von Druckseiten werden im Menü
  <menu|Document|Page|Margins> festgelegt. Oft ist es jedoch erwünscht, den
  Text auf dem Bildschirm anders darzustellen, z. B. die Ränder auf dem
  Bildschirm zu verkleinern. Das kann man mit den Optionen von
  <menu|Document|View|Page layout>.\ 

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
    <associate|preamble|false>
  </collection>
</initial>