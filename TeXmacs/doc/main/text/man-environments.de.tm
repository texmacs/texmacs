<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|<menu|Environment>>

  <em|<menu|Environment>> und <em|<menu|Content tag>> sind nahe verwandt.
  Beide stellen Text in einer Weise dar, die dem Inhalt in besonderer Weise
  angemessen ist. Während die Befehle im Menü <menu|Content tag> jedoch dazu
  benutzt werden, kurze Textstücke speziell hervorzuheben (mehr
  <hyper-link|hier|man-content-tags.en.tm>) dienen die Optionen in
  \ <menu|Environment> gewöhnlich zur systematischen speziellen Gestaltung
  länger Textteile, die oft mehrere Absätze umfassen können. Wichtig ist
  dabei, dass jede dieser <em|Umgebungen> seine eigene automatische Zählung
  vornimmt, sofern eine Nummerierung vorgesehen ist. Beispielsweise werden in
  der Mathematik die Befehle <menu|Theorem> und <menu|Proof> aus dem Menü
  <menu|Environment> häufig verwendet:\ 

  <\theorem>
    Es existieren keine positiven ganzen Zahlen <with|mode|math|a>,
    <with|mode|math|b>, <with|mode|math|c> und <with|mode|math|n> mit
    <with|mode|math|n\<geqslant\>3>, so dass
    <with|mode|math|a<rsup|n>+b<rsup|n>=c<rsup|n>> gilt.
  </theorem>

  <\proof>
    Ich habe hier nicht den Platz, um dies zu zeigen.
  </proof>

  Sie finden alle Befehle für <menu|Environment> unter
  <menu|Insert|Environment>: \ <menu|Theorem>, <menu|Proof>,
  <menu|Proposition>, <menu|Lemma>, <menu|Corollary>, <menu|Axiom> und
  <menu|Definition>. Innerhalb \ <em|<menu|Environment>> können Sie das
  <markup|dueto>-Makro benutzen und \ <key|\\ d u e t o return>
  eintippen, dann die Person(en), von der die Aussage stammt, und schlieÿlich
  <key|Entf> , um das Makro zu verlassen. So erhalten Sie z.B.:

  <\theorem>
    <dueto|Pythagoras>In einem rechtwinkligen Dreieck gilt
    <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>.
  </theorem>

  Andere häufig gebrauchte Optionen, die aber den Text nicht hervorheben,
  sind <menu|Remark>, \ <menu|Note> , \ <menu|Example>, \ <menu|Warning> und
  <menu|Exercise>. Die verbleibenden <menu|Verbatim>, \ <menu|Code> ,
  \ <menu|Quote>, \ <menu|Quotation> und <menu|Verse> dienen zur Eingabe von
  wörtlichem <menu|Verbatim>, <menu|Code>, <menu|Quote>, <menu|Quotation> und
  <menu|Verse> in Textteilen mit mehreren Paragraphen.\ 

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