<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Génération automatique de contenu>

  Le d.t.d. <tmdtd|std-automatic> définit la génération automatique de
  contenu, telles les tables de matières et les bibliographies, et leur
  affichage. Les balises suivantes sont utilisées pour les bibliographies\ 

  <\description>
    <expand|item*|<markup|cite>>Fonction avec un nombre arbitraire
    d'arguments. Chaque argument est une citation correspondant à un article
    dans un fichier BiB-<TeX> file. Les citations sont affichées telles que
    référencées dans la bibliographie et servent d'hyperliens aux références.
    Un point d'interrogation remplace les citations quand la bibliographie
    n'est pas générée.

    <expand|item*|<markup|nocite*>>Identique à <markup|cite>, mais les
    citations ne sont pas affichées dans le texte principal.

    <expand|item*|<markup|bibitem*>>Fonction qui indique comment afficher un
    article de bibliographie.
  </description>

  Les balises suivantes sont utilisées pour compiler des tables de matières :

  <\description>
    <expand|item*|<markup|toc-main-1>>Fonction à un argument pour créer une
    entrée principale dans la table des matières. Cette fonction peut être
    utilisée, par exemple, pour les différentes parties d'un livre.

    <expand|item*|<markup|toc-main-2>>Fonction à un argument pour créer une
    entrée principale dans la table des matières. Cette fonction est utilisée
    pour les chapitres.

    <expand|item*|<markup|toc-normal-1>>Fonction à un argument pour créer une
    entrée ordinaire dans la table des matières. Cette fonction est souvent
    utilisée pour les sections.

    <expand|item*|<markup|toc-normal-2>>Identique à <markup|toc-normal-2>
    pour des entrées moins importantes, telles les sous-sections.

    <expand|item*|<markup|toc-normal-3>>Identique à <markup|toc-normal-3>
    pour des entrées encore moins importantes, telles les sous-sous-sections.

    <expand|item*|<markup|toc-small-1>>Utilisée pour des entrées de peu
    d'importance, tels les paragraphes (peut être ignorée).

    <expand|item*|<markup|toc-small-2>>Utilisée pour des entrées d'encore
    moins d'importance, tels les sous-paragraphes.

    <expand|item*|<markup|toc-dots>>Séparation entre une entrée dans la table
    des matières et le numéro de page correspondant. Par défaut, on utilise
    une suite de points horizontaux.
  </description>

  Les balises suivantes sont utilisées pour les indices :

  <\description>
    <expand|item*|<markup|index>>Fonction à un argument <var|x>, qui l'insère
    dans l'index en tant qu'entrée principale.

    <expand|item*|<markup|subindex>>Fonction à deux arguments <var|x> et
    <var|y>, qui insère <var|y> dans l'index en tant que sous-entrée de
    <var|x>.

    <expand|item*|<markup|subsubindex>>Fonction à trois arguments <var|x>,
    <var|y> et <var|z>, qui insère <var|z> dans l'index en tant que
    sous-entrée de <var|y>, lui-même sous-entrée de <var|x>.

    <expand|item*|<markup|index-complex>>Fonction à quatre arguments
    <var|key>, <var|how>, <var|range>, <var|entry>, expliquée dans le section
    <apply|hyper-link|génération des index|../../links/man-index.fr.tm>.

    <expand|item*|<markup|index-line>>Cette fonction a deux arguments. Le
    premier <var|key>, clé de tri, indique comment trier le second
    <var|entry>, l'entrée. Aucun numéro de page n'est généré.

    <expand|item*|<markup|index-1>>Macro avec une entrée d'index et un numéro
    de page. Utilisée pour l'affichage d'une entrée principale d'index.

    <expand|item*|<markup|index-1*>>Identique à <markup|index-1>, mais sans
    numéro de page.

    <expand|item*|<markup|index-<with|mode|math|n>>>(avec <with|mode|math|n>
    compris entre 1 et 5) : macro avec une entrée d'index et un numéro de
    page, utilisée pour l'affichage d'une entrée de niveau
    <with|mode|math|n>.

    <expand|item*|<markup|index-<with|mode|math|n>*>>Identique à
    <markup|index-<with|mode|math|n>>, mais sans numéro de page.

    <expand|item*|<markup|index-dots>>Macro qui génère les points entre une
    entrée d'index et le(s) numéro(s) de page(s) correspondant(s).
  </description>

  Les balises suivantes sont utilisées pour les glossaires :

  <\description>
    <expand|item*|<markup|glossary>>Fonction qui insère son unique argument
    dans un glossaire.

    <expand|item*|<markup|glossary-dup>>Crée un numéro de page supplémentaire
    pour une entrée déjà insérée.

    <expand|item*|<markup|glossary-explain>>Fonction pour insérer une entrée
    de glossaire accompagnée de son explication.

    <expand|item*|<markup|glossary-line>>Insère une entrée de glossaire sans
    numéro de page.

    <expand|item*|<markup|glossary-1>>Macro pour afficher une entrée de
    glossaire et le numéro de page correspondant.

    <expand|item*|<markup|glossary-2>>Macro pour afficher une entrée de
    glossaire, son explication et le numéro de page correspondant.

    <expand|item*|<markup|glossary-dots>>Macro qui génère les points entre
    une entrée de glossaire et le(s) numéro(s) de page(s) correspondant(s).
  </description>

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-automatic>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nocite*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|bibitem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-1>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-2>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-1>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-2>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-dots>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subindex>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubindex>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-complex>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-line>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1*>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>*>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-dots>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dup>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-explain>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-line>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-1>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-2>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dots>>|<pageref|idx-34>>
    </associate>
  </collection>
</auxiliary>
