<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Balises mathématiques standards>

  Les balises mathématiques standards sont définies dans <tmdtd|std-math>.

  <\explain|<markup|binom>>
    Pour les coefficients binomiaux <math|<binom|n|m>>.
  </explain>

  <\explain|<markup|choose>>
    Autre balise équivalente à <markup|binom> (obsolète)
  </explain>

  <\explain|<markup|shrink-inline>>
    Principalement utilisée par les développeurs, cette macro réduit le texte
    à la taille d'un indice quand on est en mode affichage. La macro
    <markup|binom> l'utilise.
  </explain>

  Les macros suivantes sont des environnements mathématiques tabulaires
  standards :

  <\explain|<markup|matrix>>
    Pour les matrices <math|M=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|det>>
    Pour les déterminants <math|\<Delta\>=<det|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>>.
  </explain>

  <\explain|<markup|choice>>
    Pour les listes de choix <math|<around|\||x|\|>=<choice|<tformat|<table|<row|<cell|\<um\>x,>|<cell|<text|if
    >x\<leqslant\>0>>|<row|<cell|x,>|<cell|<text|if >x\<geqslant\>0>>>>>>
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Michèle Garoche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>