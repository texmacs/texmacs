<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Steuerung der Evaluierung>

  Das im Quellcode-Modus erscheinende Menü <menu|Source|Evaluation> enhält
  eine Anzahl von Konstrukten, mit denen sich die Evaluierung von Ausdrücken
  der Stil-Definitions-Sprache steuern lässt. Die wichtigste Anwendung
  solcher Konstrukte ist der Gebrauch in \RMeta-Makros'', die Makros als
  Argumente nehmen, um diese zu ändern wie z.B. das unten definierte
  <markup|new-theorem>:\ 

  <\tm-fragment>
    <inactive*|<assign|new-theorem|<macro|name|text|<quasi|<assign|<unquote|name>|<macro|body|<surround|<no-indent><strong|<unquote|<arg|text>>.
    >|<right-flush>|<arg|body>>>>>>>>
  </tm-fragment>

  Mit dem Aufruf <inactive*|<new-theorem|theorem|Theorem>>, werden zuerst
  alle <markup|unquote>-Befehle innerhalb des <markup|quasi>-Konstrukts
  ausgeführt, was zuu folgendem Ausdruck führt:\ 

  <\tm-fragment>
    <inactive*|<assign|theorem|<macro|body|<surround|<no-indent><strong|Theorem.
    >|<right-flush>|<arg|body>>>>>
  </tm-fragment>

  Dann wird dieser Ausdruck evaluiert. Das definiert das Makro
  <markup|theorem>.

  Man beachte, dass <TeXmacs>-Evaluierungsregeln sich etwas von den
  Scheme-Regeln unterscheiden. Diese geringfügigen Unterschiede sollen es dem
  Anwender besonders leicht machen, Makros für den Schriftsatz zu schreiben.\ 

  Beipielsweise, wenn <TeXmacs> ein Makro
  <inactive*|<macro|<active*|x<rsub|1>>|<active*|<with|mode|math|\<cdots\>>>|<active*|x<rsub|n>>|body>>
  mit den Argumenten <verbatim|y<rsub|1>> bis <verbatim|y<rsub|n>> aufruft,
  bleiben die Argumentvariablen <src-arg|x<rsub|1>> bis <src-arg|x<rsub|n>>
  an die unevaluierten Ausdrücke <verbatim|y<rsub|1>> bis
  <verbatim|y<rsub|n>> gebunden bis der Rumpf, body, mit diesen Bindungen
  evaluiert worden ist. Die Evaluierung von <verbatim|y<rsub|i>> wird
  jedesmal durchgeführt, wenn das Argument <src-arg|x<rsub|i>> aufgerufen
  wird. Wenn also ein Makro <inactive*|<macro|x|<arg|x> und nochmal <arg|x>>>
  auf den Ausdruck <verbatim|y> angewandt wird, wird der Ausdruck
  <verbatim|y> genau zweimal ausgewertet.

  In <name|Scheme> werden die Rümpfe von <name|Scheme>-Makros zweimal
  evaluiert, während bei Funktionen die Argumente evaluiert werden. Dagegen
  wird, wenn auf eine Variable zugegriffen wird, der Wert nicht evaluiert,
  gleichgültig, ob es sich um ein Argument oder eine Kontext-Variable
  handelt. Daher entspricht das <TeXmacs>-Makro

  <\tm-fragment>
    <inactive*|<assign|foo|<macro|x|<blah|<arg|x>|<arg|x>>>>>
  </tm-fragment>

  dem folgenden <name|Scheme>-Makro:

  <\scheme-fragment>
    (define-macro (foo x)

    \ \ `(let ((x (lambda () ,x)))

    \ \ \ \ \ (blah (x) (x)))
  </scheme-fragment>

  Umgekehrt gehören zu dem folgenden <name|Scheme>-Makro bzw. -Funktion

  <\scheme-fragment>
    (define-macro (foo x) (blah x x))

    (define (fun x) (blah x x))
  </scheme-fragment>

  die folgenden <TeXmacs>-Analoga:

  <\tm-fragment>
    <\inactive*>
      <assign|foo|<macro|x|<eval|<blah|<quote-arg|x>|<quote-arg|x>>>>>
    </inactive*>

    <\inactive*>
      <assign|fun|<macro|x|<with|x*|<arg|x>|<blah|<quote-value|x*>|<quote-value|x*>>>>>
    </inactive*>
  </tm-fragment>

  Hier wurden die Konstrukte <markup|quote-arg> und <markup|quote-value> dazu
  benutzt Argumente bzw. Kontext-Variable aufzurufen. Die
  <TeXmacs>-Konstrukte <markup|eval>, <markup|quote>, <markup|quasiquote> und
  <markup|unquote> verhalten sich wie ihre <name|Scheme>-Analoga. Das
  <markup|quasi>-Konstrukt ist eine Abkürzung für <markup|quasiquote> gefolgt
  von Evaluierung.

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
  </collection>
</initial>