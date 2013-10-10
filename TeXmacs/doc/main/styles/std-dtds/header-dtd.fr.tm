<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|En-têtes standards>

  Le d.t.d. <tmdtd|header> définit les balises de personnalisation des
  en-têtes et pieds de page. Cette personnalisation est basée sur l'idée que
  l'on peut définir un <em|texte de page> pour chaque page. Ce texte de page
  peut être, par exemple, un titre courant ou le nom de la section courante.
  Le texte de page peut dépendre de la parité du numéro de page et apparaître
  différemment pour certaines pages, comme celles qui débutent un chapitre.
  Les balises suivantes gèrent la disposition physique des différents types
  de pages :

  <\explain|<markup|start-page>>
    Cette balise, qui a pour seul argument le texte de page, gère la
    disposition de la première page d'un chapitre ou d'une section.
  </explain>

  <\explain|<markup|odd-page-text>>
    Identique à <markup|start-page> pour la disposition des pages impaires
    ordinaires.
  </explain>

  <\explain|<markup|even-page-text>>
    Identique à <markup|start-page> pour la disposition des pages paires
    ordinaires.
  </explain>

  Les balises suivantes gèrent les actions logiques relatives aux en-têtes
  qui sont exécutées lors de la définition d'un titre, d'un auteur ou au
  début d'une nouvelle section.

  <\explain|<markup|header-title>>
    Une balise avec un \S<space|0.2spc>argument titre<space|0.2spc>\T qui est
    utilisé lors de la spécification du titre du document.
  </explain>

  <\explain|<markup|header-author>>
    Une balise avec un \S<space|0.2spc>argument auteur<space|0.2spc>\T qui
    est utilisé lors de la spécification de l'auteur du document.
  </explain>

  <\explain|<markup|header-primary>>
    Une balise avec un \S<space|0.2spc>argument de nom de
    section<space|0.2spc>\T qui est utilisé au début de chaque section de
    base (c'est-à-dire <markup|chapter> pour le style livre ou
    <markup|section> pour le style article).
  </explain>

  <\explain|<markup|header-secondary>>
    Une balise avec un \S<space|0.2spc>argument de nom de
    section<space|0.2spc>\T qui est utilisé au début de chaque section
    secondaire (c'est-à-dire <markup|section> pour le style livre et
    <markup|subsection> pour le style article).
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