<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Architecture générale de <TeXmacs>>

  <section|Introduction>

  Le programme <apply|TeXmacs> a été écrit en C++. Vous devez utiliser
  <verbatim|g++> et l'utilitaire <verbatim|makefile> pour compiler
  <apply|TeXmacs>. À l'heure actuelle, le source (situé dans le répertoire
  <verbatim|src>) de <apply|TeXmacs> est ventilé en plusieurs parties dans
  différents répertoires :

  <\itemize>
    <item>Un jeu de structures de données de base et génériques dans le
    répertoire <verbatim|Basic>.

    <item>Des ressources standards pour <apply|TeXmacs>, tels les polices
    <apply|TeX>, les langages, encodages et dictionnaires dans le répertoire
    <verbatim|Resource>.

    <item>Une boîte à outils graphiques (avec sa documentation pas très à
    jour) dans le répertoire <verbatim|Window>.

    <item>Le langage d'extension pour <apply|TeXmacs> dans le répertoire
    <verbatim|Prg>.

    <item>La partie typographique de l'éditeur dans le répertoire
    <verbatim|src/Typeset>.

    <item>L'éditeur dans le répertoire <verbatim|src/Edit>.

    <item>Le serveur <apply|TeXmacs> dans le répertoire
    <verbatim|src/Server>.
  </itemize>

  Toutes ces parties utilisent les structures de données définies dans
  <verbatim|Basic>. La boîte à outils graphiques dépend de
  <verbatim|Resource> en ce qui concerne les polices <apply|TeX>. Le langage
  d'extension est indépendant de <verbatim|Resource> et de <verbatim|Window>.
  La partie typographique s'appuie sur toutes les autres parties, sauf
  <verbatim|Prg>. L'éditeur et le serveur <apply|TeXmacs> utilisent toutes
  les autres parties.

  Les données de <apply|TeXmacs> sont contenues dans le répertoire
  <verbatim|edit> qui correspond à la distribution <apply|TeXmacs> sans code
  source. En gros, on a les types de données suivantes :

  <\itemize>
    <item>Données concernant les polices dans <verbatim|fonts> (encodage,
    fichiers<verbatim|.pk>, etc...).

    <item>Données concernant les langages dans <verbatim|languages> (modèles
    de césure, dictionnaires, etc...).

    <item>Styles de document dans <verbatim|style>.

    <item>Initialisation et autres programmes <apply|scheme> dans
    <verbatim|progs>.
  </itemize>

  Le répertoire <verbatim|misc> contient diverses données, telle l'icône
  d'édition (<verbatim|misc/pixmaps/traditional/--x17/edit.xpm>).

  <section|Représentation interne des textes>

  <apply|TeXmacs> représente tous les textes par des arbres (pour un texte
  donnée, l'arbre correspondant est appelé <em|arbre d'édition>). Les noeuds
  d'un tel arbre sont étiquetés par des <em|opérateurs> standards dont la
  liste figure dans les fichiers <verbatim|Basic/Data/tree.hpp> et
  <verbatim|Basic/Data/tree.cpp>. Les étiquettes des feuilles des arbres sont
  des chaînes, qui sont visibles (le texte réel) ou invisibles (telles les
  longueurs ou les définitions de macros).

  Le sens du texte et la manière dont il est typographié dépendent
  essentiellement de l'environnement actif. L'environnement consiste
  principalement en une table de hachage de type
  <verbatim|rel_hashmap\<less\>string,tree\<gtr\>>, c'est-à-dire une table de
  correspondance entre les variables d'environnement et leur valeur dans
  l'arbre. Le langage actif et la police courante sont des exemples de
  variables d'environnement ; de nouvelles variables peuvent être définies
  par l'utilisateur.

  <subsection|Texte>

  Dans <apply|TeXmacs>, toutes les chaînes de texte sont constituées de
  suites de symboles universels ou spéciaux. Un symbole spécial est un
  caractère, différent de <verbatim|'\\0'>, <verbatim|'\<less\>'> ou
  <verbatim|'\<gtr\>'>. Son sens peut varier en fonction de la police avec
  laquelle il est utilisé. Un symbole universel est une chaîne commençant par
  <verbatim|'\<less\>'>, suivi d'une suite arbitraire de caractères différent
  de <verbatim|'\\0'>, <verbatim|'\<less\>'> ou <verbatim|'\<gtr\>'>, et se
  terminant par <verbatim|'\<gtr\>'>. Le sens des caractères universels ne
  dépend pas de la police avec laquelle ils sont utilisés, mais des polices
  différentes peuvent les rendre de façon différente.

  <subsection|langage>

  Le langage d'un texte est capable de fournir une analyse sémantique
  détaillée d'une phrase du texte. Il peut, au minimum, découper la phrase en
  <em|mots> (qui sont de plus petites phrases) et donner à l'outil
  typographique des informations sur les espaces désirées entre les mots et
  sur les césures à appliquer. Plus tard, il se peut qu'on ajoute d'autres
  sémantiques aux langages. Par exemple, on pourrait implémenter des
  correcteurs orthographiques pour les langages naturels et des analyseurs
  syntaxiques pour les formules mathématiques et les langages de
  programmation.

  <section|Composition des textes>

  En gros, l'outil typographique de <apply|TeXmacs> prend un arbre en entrée
  et renvoie un cadre en accédant et en modifiant l'environnement
  typographique. La classe <verbatim|box> est multifonctionnelle. Sa méthode
  principale est utilisée pour afficher le cadre sur un périphérique
  PostScript (soit l'écran, soit une imprimante). Mais elle contient aussi de
  nombreuses données de composition, tels les cadres logiques et les cadres
  d'encre, la position des scripts, etc...

  Les cadres servent aussi à passer d'un curseur physique (position sur
  l'écran) à un curseur logique (chemin dans l'arbre d'édition). En fait, les
  cadres sont, eux aussi, organisés sous forme d'arbre, ce qui facilite la
  conversion. Néanmoins, les routines de conversion peuvent être très
  complexes du fait de l'expansion des macros et des sauts de ligne et de
  page. Notez également qu'en plus d'une position horizontale et verticale le
  curseur physique possède aussi une position horizontale infinitésimale. En
  gros, cette coordonnée infinitésimale est utilisée dans certains cadres
  (par exemple pour les changements de couleur) pour ajouter une largeur
  infinitésimale.

  <section|Modification du texte>

  Vous trouverez dans le répertoire <verbatim|Edit/Modify> différentes
  routines de modification de l'arbre d'édition. Les modifications se
  déroulent en plusieurs étapes :

  <\enumerate>
    <item>Un événement en entrée déclenche une action, comme par exemple
    <verbatim|make_fraction>, qui tente de modifier l'arbre d'édition.

    <item>Toutes les modifications que <verbatim|make_fraction> ou ses
    sous-routines appliquent à l'arbre d'édition sont, en fait, décomposées
    en sept routines de modification élémentaires : <verbatim|assign>,
    <verbatim|insert>, <verbatim|remove>, <verbatim|split>, <verbatim|join>,
    <verbatim|ins_unary> et <verbatim|rem_unary>.

    <item>Avant d'exécuter la modification demandée, la routine de
    modification élémentaire envoie à toutes les vues du texte une
    notification concernant la modification.

    <item>Lors de la notification, chaque vue met à jour plusieurs éléments,
    comme la position du curseur. Une notification est aussi envoyée à
    l'outil de composition du texte, car il maintient une liste des
    paragraphes déjà composés.

    <item>Lorsque toutes les vues ont reçu notification de la modification,
    elle est alors réellement exécutée.

    <item>Chaque action utilisateur, tels la frappe sur une touche ou un clic
    de souris, est responsable de l'insertion de <em|points d'annulation>
    entre les suites de modifications élémentaires. Lorsqu'une modification
    est annulée, l'édition revient au point d'annulation précédent.
  </enumerate>

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
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|2.1|?>>
    <associate|toc-4|<tuple|2.2|?>>
    <associate|toc-5|<tuple|3|?>>
    <associate|toc-6|<tuple|4|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Introduction><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Intern representation of
      texts><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      2.1<space|2spc>Text<value|toc-dots><pageref|toc-3>

      2.2<space|2spc>The language<value|toc-dots><pageref|toc-4>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Typesetting
      texts><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Making modifications in
      texts><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
