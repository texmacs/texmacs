<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Utilisation du style tmdoc>

  En plus des macros <hlink|droits d'auteur|copyright.fr.tm> et
  <hlink|navigation|traversal.fr.tm>, qui ont déjà été expliquées, le style
  <tmstyle|tmdoc> contient un certain nombre d'autres macros et fonctions que
  vous pouvez utiliser si nécessaire :

  <\explain|<markup|key>>
    Cette macro est utilisée pour signaler des saisies clavier, telle
    <shortcut|(save-buffer)>. Les macros spécifiques <markup|kbd-gen>,
    <markup|kbd-text>, <markup|kbd-math>, <markup|kbd-symb>,
    <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>, <markup|kbd-exec>
    \ et <markup|kbd-table> sont utilisées pour les saisies clavier
    correspondant à un type spécifique d'action ou de mode. Par exemple, la
    macro <markup|kbd-math> correspond aux raccourcis clavier pour les
    opérations mathématiques, tel <key|math f>, qui débute une fraction.
  </explain>

  <\explain|<markup|menu>>
    Cette fonction, composée d'un nombre arbitraire d'arguments, fait
    référence à un menu, tel <menu|File> ou <menu|Document|Language>. Les
    articles de menu sont automatiquement traduits par cette fonction.
  </explain>

  <\explain|<markup|markup>>
    Cette macro est utilisée pour signaler une macro ou une fonction, telle
    <markup|section>.
  </explain>

  <\explain|<markup|tmstyle>>
    Cette macro indique le nom d'un fichier de style <TeXmacs> ou un module,
    tel <tmstyle|article>.
  </explain>

  <\explain|<markup|tmpackage>>
    Cette macro indique le nom d'un package, tel <tmstyle|std-markup>.
  </explain>

  <\explain|<markup|tmdtd>>
    Cette macro indique le nom d'un d.t.d. <TeXmacs>, tel <tmdtd|number-env>.
  </explain>

  Attention, aucune des marques ci-dessus ne doit être traduite. En effet,
  les marques de menus sont automatiquement traduites, de façon à assurer la
  synchronisation de leur traduction avec la traduction actuelle des menus de
  <TeXmacs>. En ce qui concerne les marques, styles, packages et
  <abbr|d.t.d.>s, il faut absolument garder le nom original, car il
  correspond souvent au nom d'un fichier.\ 

  Les macros et fonctions suivantes sont utilisées pour les liens et les
  index ; elles seront améliorées plus tard :

  <\explain|<markup|simple-link>>
    Cette macro a pour argument <math|x> une URL et génère un hyperlien de
    nom et destination <math|x>.
  </explain>

  <\explain|<markup|hyper-link>>
    Cette macro correspond à un hyperlien.
  </explain>

  <\explain|<markup|concept-link>>
    Cette macro a pour argument un concept. Plus tard, un hyperlien pourra
    être créé automatiquement à partir du concept et du reste de la
    documentation.
  </explain>

  <\explain|<markup|only-index>>
    Indexe une chaîne de caractères.
  </explain>

  <\explain|<markup|def-index>>
    Définit un nouveau concept ; le texte est imprimé en italique et indexé.
  </explain>

  <\explain|<markup|re-index>>
    Réutilise un concept déjà défini ; le texte est imprimé en roman et mis
    dans l'index.
  </explain>

  Les marques suivantes sont aussi assez fréquemment utilisées :

  <\explain|<markup|icon>>
    Lien vers une icône située dans un répertoire central, tel
    \ <verbatim|$TEXMACS_PATH/doc/images/pixmaps>.
  </explain>

  <\explain|<markup|screenshot>>
    Lien vers une capture d'écran. Les captures d'écran sont stockées dans
    une répertoire central, tel <verbatim|$TEXMACS_PATH/doc/images/screenshots>.
  </explain>

  <\explain|<markup|scheme>>
    Le language <scheme>.
  </explain>

  <\explain|<markup|framed-fragment>>
    Pour afficher un fragment de code dans un cadre approprié.
  </explain>

  <\explain|<markup|scheme-fragment>>
    Pour du code <scheme> sur plusieurs paragraphes.
  </explain>

  <\explain|<markup|tm-fragment>>
    Pour marquer du code <TeXmacs> en format <scheme>.
  </explain>

  <\explain|<markup|descriptive-table>>
    Pour les tables de description ; on peut utiliser ces tables pour
    documenter des listes de raccourcis clavier, différents types de
    marquage, etc...
  </explain>

  The style <tmstyle|tmdoc> hérite du style <tmstyle|generic>. Vous devez
  utiliser les macros <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|etc.> contenues dans ce style quand le cas se présente.

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