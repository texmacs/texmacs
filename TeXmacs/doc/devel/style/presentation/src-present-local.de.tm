<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Lokale Anpassung>

  Auch wenn <TeXmacs> versucht, den Quellcode mit den globalen
  Darstellungsoptionen übersichtlich zu präsentieren, muÿ die Lesbarkeit des
  Codes lokal verbessert werden. Im Quellmodus kann man das mit den Optionen
  in den Menüs <menu|Source|Activation> und <menu|Source|Presentation>
  erreichen. Alle lokalen Anweisungen, die die Darstellung des Quellcodes
  betreffen, werden automatisch entfernt, wenn die Datei als Basis-Stil oder
  als Stil-Paket verwendet wird.\ 

  Insbesondere wenn es um ganz bestimmte Inhalte wie mathematische Symbole
  oder um eingebettete Bilder geht, wird man lieber das Ergebnis des
  Konstrukts, also die \Raktivierte`` Form, als das inaktive
  Quellcode-Konstrukt sehen wollen. Beispielsweise wollen Sie möglicherweise
  lieber in Ihrem Quellcode

  <\tm-fragment>
    <with|mode|math|<inactive*|<assign|R|<macro|<active*|\<bbb-R\>>>>>>
  </tm-fragment>

  als den inaktiven Code

  <\tm-fragment>
    <inactive*|<with|mode|math|<assign|R|<macro|\<bbb-R\>>>>>
  </tm-fragment|>

  sehen.

  \;

  <\samp>
    Beispiel
  </samp>

  Besonders in komplizierteren Makros wie

  <\tm-fragment>
    <inactive*|<with|mode|math|<assign|diag|<macro|var|dim|<active*|<matrix|<tformat|<table|<row|<cell|<inactive*|<arg|var>><rsub|1>>|<cell|>|<cell|\<b-0\>>>|<row|<cell|>|<cell|\<ddots\>>|<cell|>>|<row|<cell|\<b-0\>>|<cell|>|<cell|<inactive*|<arg|var>><rsub|<inactive*|<arg|dim>>>>>>>>>>>>>
  </tm-fragment>

  \;

  kann eine teilweise Aktivierung des Codes beispielsweise zu folgender
  Darstellung\ 

  <\tm-fragment>
    <with|mode|math|<inactive*|<assign|diag|<macro|var|dim|<active*|<matrix|<tformat|<table|<row|<cell|<inactive*|<arg|var>><rsub|1>>|<cell|>|<cell|\<b-0\>>>|<row|<cell|>|<cell|\<ddots\>>|<cell|>>|<row|<cell|\<b-0\>>|<cell|>|<cell|<inactive*|<arg|var>><rsub|<inactive*|<arg|dim>>>>>>>>>>>>>
  </tm-fragment>

  \;

  die Lesbarkeit des Codes sehr verbessern.

  Teile des Codes können aktiviert werden, indem man es mit der Maus auswählt
  und dann das Menü <menu|Source|Activation|Activate> oder die
  Tastenkombination <shortcut|(make-mod-active 'active*)> verwendet. Entsprechend kann Code nach
  Markierung mit \ <key|<group|M->-> deaktiviert werden. Im obigen Beispiel
  haben wir das für die Darstellung der Argumente benutzt und zwar zur
  Darstellung der Variablen <src-arg|var> and <src-arg|dim> in beiden
  gezeigten Codefragmenten. Aktivierung und Deaktivierung können sich auf den
  ganzen (markierten) Baum <menu|Source|Activation|Activate> beziehen oder
  auch nur auf die Wurzel \ <menu|Source|Activation|Activate once>.

  Ein anderer Weg , um die Darstellung an spezielle Bedürfnisse anzupassen,
  liegt darin, globale Darstellungsoptionen lokal zu ersetzen. Das ist vor
  allem dort von Interesse, wo es darum geht, die Gliederung durch
  Zeilenumbrüche zu verändern. Beispielsweise kann das
  <markup|concat>-Konstrukt dazu benutzt werden, Textinhalte
  aneinanderzufügen oder auch dazu einen Block bestehend aus einer Folge von
  Anweisungen/Befehlen zu erzeugen - oder auch einer Kombination aus beiden.
  So haben wir in folgendem Beispiel

  <\tm-fragment>
    <inactive*|<assign|my-section|<macro|title|<style-with|src-compact|none|<style-with|src-compact|none|<header-hook|<arg|title>><toc-hook|<arg|title>><my-section-title|<arg|title>>>>>>>
  </tm-fragment>

  das Konstrukt <markup|concat> mit seinen Argumenten derartig dargestellt,
  daÿ er sich über mehrere Zeilen erstreckt. dazu haben wir den Befehl
  <menu|Source|Presentation|Stretched> benutzt. Das setzt übrigens voraus,
  daÿ das Konstrukt \ <markup|concat> explizit auftaucht, da sonst eine
  Verwechslung mit dem Konstrukt <markup|document> möglich wäre. Wenn dagegen
  ein Teil dieses Konstrukts wie üblich dargestellt werden sollen, kann man
  <menu|Source|Presentation|Compact> benutzen:

  <\tm-fragment>
    <inactive*|<assign|my-section|<macro|title|<style-with|src-compact|all|<style-with|src-compact|none|<header-hook|<arg|title>><toc-hook|<arg|title>><style-with|src-compact|all|<with|font-series|bold|Section:>
    <arg|title>>>>>>>
  </tm-fragment>

  Zur Zeit ist es noch nicht vorgesehen, Argumente als \ <em|inline> or
  <em|block> zu markieren. Möglicherweise tun wir das aber noch.

  Schlieÿlich kann die Darstellung von Code aus dem Menü
  <menu|Source|Presentation|Apply macro> oder <menu|Source|Presentation|Apply
  macro once> mit einem beliebigen Makro angepasst werden. Solche Makros
  werden automatisch entfernt, wenn das Dokument als Basis-Stil oder als
  Stil-Paket genutzt wird.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>\ 
</body>

<\initial>
  <\collection>
    <associate|language|german>
    <associate|preamble|false>
  </collection>
</initial>