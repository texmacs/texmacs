<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Steuerung des logischen Ablaufs>

  Neben Folgen von Ausdrücken, die mit dem <markup|concat>-Konstrukt erzeugt
  werden könne, und dem Mechanismus für die Makro-Expansion besitzt die
  <TeXmacs>-Stil-Definitions-Sprache noch einige andere Konstrukte, um den
  logischen Ablauf zu steuern: <markup|if>, <markup|case>, <markup|while> und
  <markup|for-each>. Diese Konstrukte können mit dem Menü <menu|Source|Flow
  control> erreicht werden. Wir müssen aber den Anwender warnen, diese
  Konstrukte sind nicht sehr sicher: sie können nur auf Zeileninhalte
  angewendet werden und ihr Ergebnis hängt von der Erreichbarkeit der
  Makroargumente ab. Die Erreichbarkeit kann aber nicht vorab geprüft werden!

  Das wichtigste Konstrukt ist <markup|if>, das mit dem Kurzbefehl
  \ <shortcut|(make 'if)> erzeugt werden kann. Damit kann man bedingten Schriftsatz
  erreichen:

  <\tm-fragment>
    <inactive*|<assign|appendix|<\macro|title|body>
      <style-with|src-compact|none|<compound|<if|<long-document>|<value|chapter-appendix>|<value|section-appendix>>|<arg|title>|<arg|body>>>
    </macro>>>
  </tm-fragment>

  In diesem Beispiel ist <markup|appendix> ein Block, der aus einem Titel und
  einem Rumpf besteht. Dieser wird als Kapitel in langen Dokumenten und als
  Abschnitt in anderen Dokumenten gesetzt. Im folgenden ist eine unzulässige
  Inplementierung gezeigt. Sie funktioniert nicht, weil <markup|if> zur Zeit
  nur mit Zeileninhalten arbeitet:

  <\tm-fragment>
    <active*|<\inactive*>
      <active*|<\with|color|red>
        So gehts nicht:
      </with>>

      \;

      <assign|appendix|<\macro|title|body>
        <style-with|src-compact|none|<if|<long-document>|<chapter-appendix|<arg|title>|<arg|body>>|<section-appendix|<arg|title>|<arg|body>>>>
      </macro>>
    </inactive*>>
  </tm-fragment>

  Das <markup|if>-Konstrukt kann auch benutzt werden um optionale Argumente
  einzuführen:

  <\tm-fragment>
    <inactive*|<assign|hey|<macro|first|second|<style-with|src-compact|none|<if|<equal|<arg|second>|<uninit>>|Hey
    <arg|first>, you look lonely today...|Hey <arg|first> and <arg|second>,
    you form a nice couple!>>>>>
  </tm-fragment>

  <TeXmacs> kann aber nicht entscheiden, welche Argumente optional sind und
  welche Argumente erreichbar (vom Anwender editierbar) sind. Deshalb muÿ
  diese Information von Hand mit dem <markup|drd-props>-Konstrukt festgelegt
  werden. Die Konstrukte <markup|case>, <markup|while> und <markup|for-each>
  werden in <hyper-link|Steuerung des logischen
  Ablaufs|../../format/stylesheet/prim-control.de.tm> im Abschnitt
  <hyper-link|Konstrukte für Stil-Definitionen|../../format/stylesheet/stylesheet.de.tm>
  eingehender behandelt.

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