<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|<menu|Layout issues>>

  Es gilt allgemein, \R<TeXmacs> sorgt f�r das Layout Ihres Textes''. Deshalb
  sollten Sie vermeiden, Ihr Dokument manuell zu gestalten, indem Sie
  \ Leerzeichen oder Leerzeilen �ber die Tastatur eingeben, um horizontale
  und vertikale Abst�nde zu erzeugen, obwohl dies nicht verboten ist und oft
  der Gewohnheit entspricht. Sie sollten daf�r die Befehle aus dem Men�
  <menu|Format|Spaces> verwenden. Das hat den Vorteil, dass bei kleinen
  �nderungen, die den Zeilen- oder Seitenumbruch ver�ndern, und auch bei
  gro�en wie einem Wechsel des Dokumentstiles, die Darstellung des Textes
  unver�ndert bleibt. Wenn Sie sich daran halten, m�ssen Sie nach solchen
  �nderungen nicht den ganzen Text kontrollieren und anpassen.

  Mehrere Typen von expliziten Abstandsbefehlen wurden implementiert. Einmal
  gibt es <em|fixe> Abst�nde, die eine vorgegebene Gr��e haben und behalten.
  Sie m�ssen ihre L�nge in <menu|Standard length units> eingeben. Mehr dazu
  finden Sie unter <hlink|<menu|Standard length
  units>|../../devel/format/basics/lengths.de.tm>. Zum anderen gibt es
  variable Abst�nde, deren L�nge vom Zeilenumbruch abh�ngt. Weiterhin wird
  unterschieden zwischen horizontalen und vertikalen Abst�nden. Horizontale
  Abst�nde haben Breite aber keine H�he. Sie k�nnen variabel oder fix sein.
  Vertikale Abst�nde k�nnen vor oder nach einem Absatz eingef�gt werden. Der
  Abstand zwischen zwei Abs�tzen ist das Maximum von zwei Abst�nden und zwar
  dem nach dem vorderen Absatz spezifizierten Abstand einerseits und dem vor
  dem folgenden Absatz spezifizierten Abstand andererseits. Er ist also
  <strong|nicht> die Summe beider im Gegensatz zu <TeX>.

  Au�erdem k�nnen Sie mit der Option <menu|Tab> Tabulatoren einf�gen. Die
  Funktion der Tabulatoren ist anders als gew�hnlich. F�gt man nur einen
  Tabulator ein, so wird der nachfolgende Text rechtsb�ndig an dem rechten
  Rand eingef�hrt. Wird ein zweiter Tabulator hinzugef�gt, so entsteht eine
  zweigeteilte Zeile usw., z.B.:

  kein Tab<htab|5mm>zwei

  kein Tab<htab|5mm>ein<htab|5mm>zwei

  kein Tab<htab|5mm>ein<htab|5mm>zwei<htab|5mm>drei

  Abs�tze k�nnen linksb�ndig, rechtsb�ndig, zentriert oder als Blocksatz
  formatiert werden. Au�erdem k�nnen f�r jeden Absatz der linke und der
  rechte Seitenrand und der Erstzeileneinzug sowie die Zeilenabst�nde
  innerhalb des Absatzes spezifiziert werden. Wenn f�r einen gr��eren
  Textbereich ein Erstzeileneinzug formatiert wurde, dann kann durch
  Einzugsmarken der Einzug ab- bzw. angeschaltet werden. Dabei gibt es zwei
  verschiedene Marken, die auf den vorgehenden Absatz bzw. den nachfolgenden
  Absatz wirken. Genaueres finden Sie <hlink|<menu|Hier>|../../devel/format/regular/prim-indent.de.tm>.

  Im Men� <menu|Document|Page> k�nnen Sie das Seitenlayout festlegen. Die Art
  und Weise der Bildschirmdarstellung k�nnen Sie mit den Optionen unter
  <menu|Document|Page|Type> beeinflussen. Wenn Sie z.B. <menu|Paper> w�hlen,
  werden die Seitenumbr�che explizit dargestellt. Die Voreinstellung ist
  <menu|Papyrus>. Damit werden Seitenumbr�che nicht auf dem Bildschirm
  gezeigt und die Darstellung ist schneller. Die Einstellung <menu|Automatic>
  unterstellt, dass das Bildschirmfenster exakt der Papiergr��e entspricht.
  Mehr dazu finden Sie <hlink|<menu|Hier>|../../devel/format/environment/env-page.de.tm>.
  Die R�nder (oben, unten, links, rechts) von Druckseiten werden im Men�
  <menu|Document|Page|Margins> festgelegt. Oft ist es jedoch erw�nscht, den
  Text auf dem Bildschirm anders darzustellen, z. B. die R�nder auf dem
  Bildschirm zu verkleinern. Das kann man mit den Optionen von
  <menu|Document|Page|Screen margins>.\ 

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