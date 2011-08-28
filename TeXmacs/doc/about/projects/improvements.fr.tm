<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Amélioration de l'implémentation actuelle>

  <with|color|red|[Doit être mis à jour]>

  <section|Gestion de la mémoire>

  Si j'en ai le courage, j'écrirais peut-être un jour un collecteur d'espace
  mémoire pour <apply|TeXmacs>.

  <section|Interface graphique>

  Nous souhaitons passer à <with|font shape|small-caps|Guile-Gtk> ou à toute
  autre interface graphique portable.

  <section|Encodage et polices>

  <\itemize>
    <item>Il reste encore quelques changements à faire dans la façon dont
    l'encodage des polices est gérée. Ceci devrait faciliter la maintenance
    de polices comportant des caractères issus de différentes polices
    physiques, virtuelles, des caractères spéciaux, etc...

    <item>À l'heure actuelle, la police logique active n'est déterminée qu'à
    partir d'un jeu donnée de variables d'environnement. Il faudrait que la
    police logique soit représentée par un arbre (au lieu d'une chaîne de
    caractères), qui serait évalué (de façon à permettre le remplacement des
    variables d'environnement), puis l'ensemble serait passé à
    <verbatim|find_font(display,tree)>. La police active serait alors une
    police fusionnée et les polices pourraient alors dépendre des variables
    d'environnement (polices à plusieurs couleurs)..
  </itemize>

  <section|Vitesse>

  Pour accélérer la vitesse d'exécution du programme, un document n'est pas
  mis à jour dans son entier chaque fois qu'il subit une modification.
  Néanmoins, il reste à mettre en place certaines changements :

  <\itemize>
    <item>Améliorer la vitesse de chargement (et de sauvegarde) des fichiers.
    Cela accélérera le chargement des polices.

    <item>Encoder les variables d'environnement système ; cela accélérera le
    programme dans son entier.

    <item>Repenser complètement la façon dont les variables système
    d'environnement de style sont gérées lors de la mise en page des
    concaténations et de paragraphes ; elle est loin d'être optimale.

    <item>Améliorer l'évaluation du contexte actif sous le curseur, en
    particulier l'évaluation de l'environnement. Ce processus prend beaucoup
    de temps actuellement et ralentit la vitesse de déplacement du curseur
    dans les documents complexes utilisant les polices <TeX> (le défilement
    du texte est bien plus rapide quand on utilise des polices X).
  </itemize>

  <section|Changements divers>

  Il faudrait changer ou stabiliser les implémentations suivantes :

  <\itemize>
    <item>Mouvement du curseur entre les lignes d'un même paragraphe (il
    faudrait que le curseur passe au début de la ligne suivante quand on le
    déplace après la fin de la ligne précédente).

    <item>Largeurs des barres de fraction, des lignes supérieures des
    symboles de racine carrée et du signe barré.

    <item>Les cadres ne devraient pas avoir d'origines, mais leurs héritiers
    devraient avoir une position.

    <item>Séparation claire entre les fichiers qui dépendent du système (par
    exemple : <verbatim|fast_alloc.cpp>, <verbatim|file.hpp>,
    <verbatim|dir.hpp>) dans certains répertoires et les autres.
  </itemize>

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
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Memory
      management><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Graphical
      interface><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Encodings and
      fonts><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Speed><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Miscellaneous
      changes><value|toc-dots><pageref|toc-5><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
