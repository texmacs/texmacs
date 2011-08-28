<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cadres produits par l'outil typographique>

  <section|Introduction>

  L'outil de composition de <apply|TeXmacs> traduit un document représenté
  par un arbre en un cadre graphique, qui peut être affiché sur l'écran ou
  imprimé. Contrairement à <apply|LaTeX>, le cadre graphique contient
  beaucoup plus d'informations qu'il n'est nécessaire pour un rendu
  graphique. En gros, ces informations peuvent être rangées dans les
  catégories suivantes :

  <\itemize>
    <item>Cadres physique et logique.

    <item>Méthode de rendu graphique.

    <item>Diverses données typographiques.

    <item>Trace du sous-arbre source qui a conduit au cadre.

    <item>Calcul des positions des curseurs et des sélections.

    <item>Gestionnaires d'événements pour le contenu dynamique.
  </itemize>

  Le cadre logique est utilisée par le composeur pour positionner le cadre
  par rapport aux autres. D'autres informations, telle l'inclinaison du
  cadre, sont aussi sauvegardées pour que le composeur puisse en faire usage.
  Le cadre physique contient la représentation graphique du cadre. Cette
  information sert à redessiner partiellement un cadre de manière efficiente.

  Pour positionner le curseur ou pour faire une sélection, il faut avoir une
  correspondance entre les positions logiques dans l'arbre source et les
  positions physiques dans les cadres composés. En fait, les cadres et leurs
  sous-cadres sont organisés logiquement comme des arbres. Les cadres
  fournissent des routines de traduction entre les chemins dans l'arbre du
  cadre et l'arbre source, et pour trouver le chemin associé à un point
  graphique.

  <section|Correspondance entre un cadre et son source>

  <subsection|Problèmes à résoudre>

  Pour implémenter la correspondance entre les chemins dans l'arbre source et
  l'arbre du cadre, il faut surmonter plusieurs types de difficultés en même
  temps :

  <\enumerate>
    <item>La correspondance peut ne pas être directe à cause des sauts de
    lignes, des notes en base de page et de l'expansion des macros.

    <item>La correspondance doit être relativement efficiente du point de vue
    de l'espace et du temps.

    <item>Certains cadres, tels les en-têtes et les pieds de page ou le
    résultat de certains expansions de macros, peuvent ne pas être
    <space|0.2spc>accessibles<space|0.2spc>. Bien qu'on puisse trouver une
    position du curseur pour cliquer dessus, le contenu d'un tel cadre n'est
    pas directement éditable.

    <item>La correspondance doit être relativement complète (voir section
    suivante).
  </enumerate>

  La première difficulté nous oblige à stocker dans l'arbre d'édition un
  chemin pour tout cadre. Pour gagner de la place, ce chemin est stocké en
  ordre inverse de façon à ce que les sommets communs puissent être partagés.
  Ce partage des sommets communs est aussi nécessaire pour changer rapidement
  les emplacements du source quand on modifie l'arbre source, par exemple
  lorsqu'on insère un nouveau paragraphe.

  Pour surmonter la troisième difficulté, le chemin inverse peut débuter par
  un nombre négatif qui indique que le cadre ne peut être édité directement
  (on dit alors que le cadre est une décoration). Dans ce cas, la fin du
  chemin inverse correspond à l'emplacement, dans l'arbre source, où le
  curseur est positionné lorsque l'on clique sur le cadre. Le nombre négatif
  a une influence sur la façon dont ceci est réalisé.

  <subsection|Trois sortes de chemins>

  Il faut gérer trois sortes de chemins :

  <\description>
    <expand|item*|Chemins d'arbres.>Ces chemins correspondent aux chemins
    dans l'arbre source. En fait, le chemin amputé de son dernier élément
    pointer sur un sous-arbre de l'arbre source. Le dernier élément donne la
    position dans ce sous-arbre : si le sous-arbre est une feuille,
    c'est-à-dire une chaîne, c'est la position dans cette chaîne. Autrement,
    un 0 indique une position avant le sous-arbre, un 1 une position après le
    sous-arbre.

    <expand|item*|Chemins inverses.>Ce sont les chemins construits en
    inversant les chemins d'arbres (avec une fin de chemin commune) ; ils
    peuvent avec une en-tête négative. Une en-tête négative indique que le
    chemin d'arbre n'est pas accessible, c'est-à-dire que le sous-arbre
    correspondant ne représente pas un contenu éditable. Si les valeurs
    négatives sont <with|mode|math|-2>, <with|mode|math|-3> ou
    <with|mode|math|-4>, alors il faut mettre un 0 ou un 1 derrière le chemin
    d'arbre suivant la valeur et la position du curseur.

    <expand|item*|Chemins de cadres.>Ces chemins correspondent aux chemins
    logiques dans l'arbre de cadre. Le chemin amputé de son dernier élément
    pointe vers un sous-cadre du cadre principal. Le dernier élément donne la
    position dans le sous-arbre : si le sous-cadre correspond à un cadre de
    texte, c'est la position dans ce texte. Sinon, un 0 indique une position
    avant le sous-cadre et un 1 une position après le sous-cadre. Dans le cas
    de cadres auxiliaires, un 2 ou un 3 peuvent aussi indiquer la position
    après un indice ou exposant à gauche, ou bien avant un indice ou exposant
    à droite.
  </description>

  <subsection|Routines de conversion>

  Pour implémenter la conversion entre les trois sortes de chemins, chaque
  cadre possède un chemin inverse de référence <verbatim|ip> dans l'arbre
  source. Les cadres composites possèdent en plus un chemin de référence
  gauche <verbatim|lip> et droit <verbatim|rip>, qui correspondent aux
  chemins accessibles juste à leur gauche ou juste à leur droite dans leurs
  sous-cadres (s'il existe de tels sous-cadres).

  La routine :

  <\verbatim>
    \ \ \ \ virtual path box_rep::find_tree_path (path bp)
  </verbatim>

  transforme un chemin de cadre en un chemin d'arbre. Cette routine (qui
  n'utilise que <verbatim|ip>) est rapide et possède une complexité en temps
  linéaire fonction de la longueur des chemins. La routine :

  <\verbatim>
    \ \ \ \ virtual path box_rep::find_box_path (path p)
  </verbatim>

  effectue la conversion inverse. Malheureusement, dans le pire des cas, il
  peut être nécessaire de rechercher le chemin d'arbre dans tous les
  sous-cadres. Néanmoins, dans le meilleur des cas, un algorithme
  dichotomique (qui utilise <verbatim|lip> et <verbatim|rip>) \ trouve la
  branche à parcourir dans un temps logarithmique. Cet algorithme possède
  aussi une complexité quadratique du temps fonction de la longueur des
  chemins, car il faut souvent inverser les chemins.

  <section|Curseur et sélection>

  Pour remplir son rôle d'éditeur structuré<space|0.2spc>, <apply|TeXmacs>
  doit fournir une correspondance (relativement) complète entre les chemins
  d'arbres logiques et les positions physiques du curseur. Ceci conduit à des
  difficultés supplémentaires dans le cas de <space|0.2spc>changement
  d'environnement<space|0.2spc>, tel un changement de police ou de couleur.
  En effet, quand on se trouve à la frontière d'un tel changement, on ne sait
  pas, <with|font shape|italic|a priori>, de façon précise dans quel
  environnement on se trouve.

  C'est pourquoi, dans <apply|TeXmacs>, la position du curseur contient des
  coordonnées <with|mode|math|x> et <with|mode|math|y>, ainsi qu'une abscisse
  infinitésimale supplémentaire, appelée <with|mode|math|\<delta\>>. Par
  suite, un changement d'environnement est représenté par un cadre de largeur
  infinitésimale. Bien que la position <with|mode|math|\<delta\>> du curseur
  soit toujours 0 quand on utilise la souris pour effectuer une sélection,
  elle peut ne pas être nulle quand on se déplace à l'aide des flèches
  directionnelles. La routine en temps linéaire :

  <\verbatim>
    \ \ \ \ virtual path box_rep::find_box_path (SI x, SI y, SI delta)
  </verbatim>

  fonction de la longueur du chemin recherche le chemin du cadre qui
  correspond à la position du curseur. Inversement, la routine :\ 

  <\verbatim>
    \ \ \ \ virtual cursor box_rep::find_cursor (box bp)
  </verbatim>

  mène à une représentation graphique du curseur pour un chemin de cadre
  donné. Le curseur est défini par ses coordonnées <with|mode|math|x>,
  <with|mode|math|y> et <with|mode|math|\<delta\>> et un segment de ligne
  relatif à cette origine, défini par ses extrémités
  <with|mode|math|(x<rsub|1>,y<rsub|1>)> et
  <with|mode|math|(x<rsub|2>,y<rsub|2>)>.

  De même, la routine :

  <\verbatim>
    \ \ \ \ virtual selection box_rep::find_selection (box lbp, box rbp)
  </verbatim>

  calcule la sélection entre deux chemins de cadres donnés. Cette sélection
  comprend deux chemins d'arbre délimitant la sélection et une représentation
  graphique sous la forme d'une liste de rectangles.

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|2.1|?>>
    <associate|toc-4|<tuple|2.2|?>>
    <associate|toc-5|<tuple|2.3|?>>
    <associate|toc-6|<tuple|3|?>>
    <associate|toc-7|<tuple|4.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Introduction><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>The correspondence between a box and
      its source><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      2.1<space|2spc>Discussion of the problems being
      encountered<value|toc-dots><pageref|toc-3>

      2.2<space|2spc>The three kinds of paths<value|toc-dots><pageref|toc-4>

      2.3<space|2spc>The conversion routines<value|toc-dots><pageref|toc-5>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>The cursor and
      selections><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
