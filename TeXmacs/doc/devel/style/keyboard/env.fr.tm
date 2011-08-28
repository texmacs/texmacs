<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Macros, fonctions et variables d'environnement>

  Les combinaisons de touches que vous devez connaître pour créer des
  fichiers de style sont les suivantes :

  <\description>
    <expand|item*|<key|M-=>>crée une nouvelle commande. Le premier
    argument est le nom de la nouvelle commande, le second une expression.

    <expand|item*|<key|M-w>>permet de changer localement une ou
    plusieurs variables d'environnement. Les ordres with
    <em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|<em|>>>>>>>>>>>>>>>>>>ont
    la forme suivante : <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<rsub|n>\|b\<rangle\>>,
    où \ <with|mode|math|x<rsub|i>> est le nom d'un variable,
    <with|mode|math|a<rsub|i>> sa valeur locale et <with|mode|math|b> le
    texte sur lequel s'applique l'environnement local.

    <expand|item*|<key|M-m>>crée une macro. Les arguments de la
    macro sont insérés avec la touche <key|tab>.

    <expand|item*|<key|M-f>>crée une fonction. Les arguments de la
    fonction sont insérés avec la touche <key|tab>.

    <expand|item*|<key|inactive #>>renvoie la valeur d'un argument de la
    macro.

    <expand|item*|<key|inactive v>>renvoie la valeur d'une variable
    d'environnement.

    <expand|item*|<key|inactive e>>développe une macro à n arguments (n
    positif ou nul).

    <expand|item*|<key|inactive a>>exécute une fonction à n arguments (n
    positif ou nul).
  </description>

  Plus précisément, lors de l'évaluation d'une macro
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> créée avec
  <key|inactive e>, voici ce qu'il se passe :

  <\itemize>
    <item>Si <with|mode|math|a> n'est ni une chaîne de caractères, ni une
    macro, alors <with|mode|math|a> est évaluée en premier. Le résultat est
    soit un nom de macro, soit une expression <with|mode|math|f>.

    <item>Dans le cas d'un nom de macro, on remplace <with|mode|math|f> par
    la valeur de la variable d'environnement <with|mode|math|f>. Si, après
    cela, <with|mode|math|f> n'est toujours pas une macro, on retourne
    <with|mode|math|f>.

    <item>Soit <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> les arguments
    de <with|mode|math|f> et <with|mode|math|b> son corps (les arguments en
    trop sont supprimés ; les arguments manquants prennent une chaîne de
    caractère nulle par défaut). On substitue ensuite
    <with|mode|math|x<rsub|i>> à <with|mode|math|y<rsub|i>> dans
    <with|mode|math|b> et l'on retourne le résultat du calcul.
  </itemize>

  Les fonctions sont similaires aux macros, sauf que les arguments des
  fonctions sont évalués et qu'ils ne peuvent être modifiées directement
  (vous devez d'abord désactiver la fonction, modifier les arguments et la
  réactiver). De plus, <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> sont
  maintenant considérées comme des variables d'environnement locales, dont
  les valeurs sont <with|mode|math|x<rsub|1>,\<ldots\>,x<rsub|n>>. Ces
  variables locales sont perdues au retour de la fonction qui les utilise.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Michèle Garoche>

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
