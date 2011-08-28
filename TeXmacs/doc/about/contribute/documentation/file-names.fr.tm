<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conventions de noms de fichiers>

  La majeure partie des documents doivent être insérés en fonction d'un sujet
  dans une arborescence de répertoires. Les sous-répertoires du répertoire
  principal sont les suivants :

  <\description>
    <expand|item*|devel>Documentation pour développeurs.

    <expand|item*|examples>Exemples de documents <TeXmacs>.

    <expand|item*|incoming>Documents en devenir, sujets à variation.

    <expand|item*|main>Documentation principale.

    <expand|item*|meta>Documentation sur la création et la compilation de
    documents.
  </description>

  Essayez de ne pas créer trop de fichiers par répertoire.

  Les noms des fichiers dans le répertoire main sont du type
  <verbatim|type-nom.langue.tm>. Dans les autres répertoires, ils sont de la
  forme <verbatim|nom.langue.tm>. Ici <verbatim|type> représente un certain
  type de documentation ; ce soit être l'un des suivants :

  <\description>
    <expand|item*|adv>Documentation pour utilisateurs expérimentés.

    <expand|item*|man>Documentation à inclure dans le guide <TeXmacs>.

    <expand|item*|tut>Documentation à inclure dans le tutoriel <TeXmacs>.
  </description>

  Vous devez essayer de regrouper la documentation sur un sujet donné, quel
  qu'en soit le type. En effet, ceci permet de retrouver plus facilement tous
  les documents existants sur un sujet particulier. Il peut arriver aussi que
  l'on veuille incorporer dans le guide des documents prévus au départ pour
  le tutoriel. La <verbatim|langue> dans laquelle le document a été écrit est
  un code de deux lettres, comme par exemple : <verbatim|en>, <verbatim|fr>,
  etc... Le <verbatim|nom> du fichier doit être le même quelle que soit la
  langue. Par exemple, <verbatim|man-keyboard.en.tm> ne doit pas être traduit
  par <verbatim|man-clavier.fr.tm>, mais devenir man-keyboard.fr.tm.

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
