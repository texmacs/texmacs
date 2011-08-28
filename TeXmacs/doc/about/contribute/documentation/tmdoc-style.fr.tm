<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Utilisation du style tmdoc>

  En plus des macros <apply|hyper-link|droits d'auteur|copyright.fr.tm> et
  <apply|hyper-link|navigation|traversal.fr.tm>, qui ont déjà été expliquées,
  le style <tmstyle|tmdoc> contient un certain nombre d'autres macros et
  fonctions que vous pouvez utiliser si nécessaire :

  <\description>
    <expand|item*|<markup|key>>Cette macro est utilisée pour signaler des
    saisies clavier, telle <shortcut|(save-buffer)>. Les macros spécifiques
    <markup|kbd-gen>, <markup|kbd-text>, <markup|kbd-math>,
    <markup|kbd-symb>, <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>,
    <markup|kbd-exec> \ et <markup|kbd-table> sont utilisées pour les saisies
    clavier correspondant à un type spécifique d'action ou de mode. Par
    exemple, la macro <markup|kbd-math> correspond aux raccourcis clavier
    pour les opérations mathématiques, tel <key|math f>, qui débute
    une fraction.

    <expand|item*|<markup|menu>>Cette fonction, composée d'un nombre
    arbitraire d'arguments, fait référence à un menu, tel <apply|menu|File>
    ou <apply|menu|Document|Language>. Les articles de menu sont
    automatiquement traduits par cette fonction.

    <expand|item*|<markup|markup>>Cette macro est utilisée pour signaler une
    macro ou une fonction, telle <markup|section>.

    <expand|item*|<markup|tmstyle>>Cette macro indique le nom d'un fichier de
    style <TeXmacs> ou un module, tel <tmstyle|article>.

    <expand|item*|<markup|tmpackage>>Cette macro indique le nom d'un package,
    tel <tmstyle|std-markup>.

    <expand|item*|<markup|tmdtd>>Cette macro indique le nom d'un d.t.d.
    <TeXmacs>, tel <tmdtd|number-env>.
  </description>

  Attention, aucune des marques ci-dessus ne doit être traduite. En effet,
  les marques de menus sont automatiquement traduites, de façon à assurer la
  synchronisation de leur traduction avec la traduction actuelle des menus de
  <TeXmacs>. En ce qui concerne les marques, styles, packages et
  <abbr|d.t.d.>s, il faut absolument garder le nom original, car il
  correspond souvent au nom d'un fichier.\ 

  Les macros et fonctions suivantes sont utilisées pour les liens et les
  index ; elles seront améliorées plus tard :

  <\description>
    <expand|item*|<markup|simple-link>>Cette macro a pour argument
    <with|mode|math|x> une URL et génère un hyperlien de nom et destination
    <with|mode|math|x>.

    <expand|item*|<markup|hyper-link>>Cette macro correspond à un hyperlien.

    <expand|item*|<markup|concept-link>>Cette macro a pour argument un
    concept. Plus tard, un hyperlien pourra être créé automatiquement à
    partir du concept et du reste de la documentation.

    <expand|item*|<markup|only-index>>Indexe une chaîne de caractères.

    <expand|item*|<markup|def-index>>Définit un nouveau concept ; le texte
    est imprimé en italique et indexé.

    <expand|item*|<markup|re-index>>Réutilise un concept déjà défini ; le
    texte est imprimé en roman et mis dans l'index.
  </description>

  Les marques suivantes sont aussi assez fréquemment utilisées :

  <\description>
    <expand|item*|<markup|icon>>Lien vers une icône située dans un répertoire
    central, tel \ <verbatim|$TEXMACS_PATH/doc/images/pixmaps>.

    <expand|item*|<markup|screenshot>>Lien vers une capture d'écran. Les
    captures d'écran sont stockées dans une répertoire central, tel
    <verbatim|$TEXMACS_PATH/doc/images/screenshots>.

    <expand|item*|<markup|scheme>>Le language <value|scheme>.

    <expand|item*|<markup|framed-fragment>>Pour afficher un fragment de code
    dans un cadre approprié.

    <expand|item*|<markup|scheme-fragment>>Pour du code <value|scheme> sur
    plusieurs paragraphes.

    <expand|item*|<markup|tm-fragment>>Pour marquer du code <TeXmacs> en
    format <value|scheme>.

    <expand|item*|<markup|descriptive-table>>Pour les tables de description ;
    on peut utiliser ces tables pour documenter des listes de raccourcis
    clavier, différents types de marquage, etc...
  </description>

  The style <tmstyle|tmdoc> hérite du style <tmstyle|generic>. Vous devez
  utiliser les macros <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|etc.> contenues dans ce style quand le cas se présente.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Michèle Garoche>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-46|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|key>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-gen>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-text>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-symb>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-big>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-large>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-ia>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-exec>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-table>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|menu>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|Fichier>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Langue>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|markup>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmstyle>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|article>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmpackage>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|std-markup>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdtd>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|number-env>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|simple-link>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hyper-link>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|concept-link>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|only-index>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|def-index>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|re-index>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|icon>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|screenshot>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|framed-fragment>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme-code>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tm-fragment>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|descriptive-table>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|generic>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize>>|<pageref|idx-41>>
    </associate>
  </collection>
</auxiliary>
