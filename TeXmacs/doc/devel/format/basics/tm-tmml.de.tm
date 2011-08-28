<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|XML-Linearisierung>

  Um Kompatibilität mit XML zu erreichen, unterstützt <TeXmacs> auch die
  Linearisierung ins \ XML-Format. Allerdings ist das XML-Format wortreicher
  und schwerer lesbar als das normale <TeXmacs>-Format. Um eine Datei im
  XML-Format (mit Datei-Erweiterung:<verbatim|.tmml>) zu speichern oder zu
  laden, können Sie das Menü <menu|File|Export|XML> bzw.
  <menu|File|Import|XML>.\ 

  <TeXmacs>-Dokumente entsprechen keinem vordefiniertem DTD, denn das für ein
  Dokument zutreffende DTD hängt von seinem Stil ab. Das XML-Format stellt
  daher nur eine XML-Darstellung von <TeXmacs>-Bäumen bereit. Die Syntax
  wurde mit dem Ziel, eine möglichst Baum-ähnliche Struktur zu erreichen,
  unter Verwendung konventioneller XML-Syntax entwickelt, die von den
  üblichen Standard-Werkzeugen unterstützt wird.

  <paragraph*|die Codierung von Zeichenketten>

  Die Blätter der <TeXmacs>-Bäume werden von der universellen
  <TeXmacs>-Kodierung in Unicode übertragen. Zeichen ohne
  Unicode-Entsprechung werden als Dateneinheit dargestellt (für die Zukunft
  planen wir eine <verbatim|tmsym>-Operation zur Darstellung solcher
  Zeichen).

  <paragraph*|XML-Darstellung von normalen Operationen>

  Bäume mit einem einzigen Kind werden einfach durch die entsprechende
  XML-Operation ersetzt. Hat der Baum mehrere Kinder, wird jedes Kind in eine
  \ <verbatim|tm-arg> -Markierung (tag) eingeschlossen.
  <with|mode|math|<sqrt|x+y>> wird so zu

  <\quote-env>
    <framed-fragment|<verbatim|\<less\>sqrt\<gtr\>y+z\<less\>/sqrt\<gtr\>>>
  </quote-env>

  linearisiert wird, während der Bruch <with|mode|math|<frac|1|2>> durch\ 

  <\quote-env>
    <\framed-fragment>
      <\with|par-par-sep|0fn>
        <\verbatim>
          \<less\>frac\<gtr\>

          \ \ \<less\>tm-arg\<gtr\>1\<less\>/tm-arg\<gtr\>

          \ \ \<less\>tm-arg\<gtr\>2\<less\>/tm-arg\<gtr\>

          \<less\>/frac\<gtr\>
        </verbatim>
      </with>
    </framed-fragment>
  </quote-env>

  \;

  dargestellt wird.

  In dem Beispiel oben wurde Leerraum ignoriert. Indem man die
  Standard-Variable <verbatim|xml:space> auf <verbatim|preserve> setzt, kann
  man Leerraum erhalten.\ 

  <paragraph*|Spezielle tags (Markierungen)>

  Einige <TeXmacs>-Operationen werden auf eine spezielle Weise nach XML
  übertragen. Die Grundoperation <markup|concat> wird einfach durch die
  Verkettung von Zeichenketten ersetzt. So wird aus
  <line-break><with|mode|math|<frac|1|2>+<sqrt|x+y>> einfach:

  <\quote-env>
    <framed-fragment|<\verbatim>
      \<less\>frac\<gtr\>\<less\>tm-arg\<gtr\>1\<less\>/tm-arg\<gtr\>\<less\>tm-arg\<gtr\>2\<less\>/tm-arg\<gtr\>\<less\>/frac\<gtr\>+\<less\>sqrt\<gtr\>y+z\<less\>/sqrt\<gtr\>
    </verbatim>>
  </quote-env>

  Auch die <markup|document> Grundoperation wird nicht explizit exportiert.
  Dafür wird jedes Argument, das ein Absatz ist, in <verbatim|tm-par>
  Markierungen eingeschlossen. Z.B. wird das Zitat

  <\quote-env>
    Dies ist der erste Absatz.

    Das ist der zweite Absatz.
  </quote-env>

  linearisiert zu

  <\tm-fragment>
    <\with|par-par-sep|0fn>
      <\verbatim>
        \<less\>quote-env\<gtr\>

        \ \ \<less\>tm-par\<gtr\>

        \ \ \ \ Dies ist der erste Absatz.

        \ \ \<less\>tm-par\<gtr\>

        \ \ \ \ Das ist der zweite Absatz.

        \<less\>/quote-env\<gtr\>
      </verbatim>
    </with>
  </tm-fragment>

  Eine <markup|with> Grundoperation, die nur Zeichenketten-Attribute und
  Werte enthält, wird durch die normalen \ XML-Attribut-Anweisungen ersetzt.
  \Rein <with|color|blue|blauer> Text'' würde zu\ 

  <\quote-env>
    <framed-fragment|<\verbatim>
      ein \<less\>with color="blue"\<gtr\>blauer\<less\>/with\<gtr\> Text
    </verbatim>>
  </quote-env>

  Umgekehrt stellt <TeXmacs> die Grundoperation <markup|attr> bereit, um XML
  Markierungen in <TeXmacs> verwenden zu können. So würde das XML Fragment

  <\quote-env>
    <framed-fragment|<\verbatim>
      ein \<less\>mytag Tier="haarig"\<gtr\>special\<less\>/mytag\<gtr\> Text
    </verbatim>>
  </quote-env>

  importiert zu \R<inactive*|ein <my-tag|<attr|Tier|haarig>|special> Text>''.
  Deshalb ist es prinzipiell möglich, <TeXmacs> als Editor für normale
  XML-Dateien zu verwenden.

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
  </collection>
</initial>