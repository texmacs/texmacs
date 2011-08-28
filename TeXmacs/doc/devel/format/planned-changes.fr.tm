<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <\expand|tmdoc-title>
    Changements prévus dans le format\ 

    \ \ \ \ \ \ \ \ \ \ de document <TeXmacs>
  </expand>

  <\enumerate>
    <item>Actuellement, le nombre d'arguments des opérateurs <apply|TeXmacs>
    ne peut être nul (dans ce cas, ils deviennent des feuilles). Pour les
    opérateurs tels que<verbatim| concat>, ceci n'est pas très pratique ni
    sain. Nous avons donc prévu d'autoriser un nombre nul d'arguments. Les
    opérateurs de formatage de la forme <verbatim|(format how)> deviendront
    des opérateurs avec un nombre d'arguments nul.

    <item>Nous ne sommes pas très satisfaits de la façon dont les tableaux
    sont formatés. Nous fournissons actuellement des opérateurs
    <verbatim|mosaic> et <verbatim|split>, qu'il faudrait remplacer par un
    unique opérateur plus puissant. Pour l'instant, nous n'avons pas encore
    réussi à trouver l'opérateur idéal.

    <item>Il faudrait prévoir plusieurs extensions de format. Dans
    <verbatim|Typeset/data/data.gen.cpp>, vous noterez que nous avons déjà
    introduit les opérateurs <verbatim|graphics>, <verbatim|point>,
    <verbatim|line>, <verbatim|arc> et <verbatim|bezier>, qui ne sont pas
    encore implémentés. D'autres opérateurs viendront facilitant la
    manipulation des notes en base de page, du multi-colonnage, des textes
    dynamiques, des graphiques, des résultats de calcul formel, des bases de
    données, des tableurs, etc...
  </enumerate>

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
