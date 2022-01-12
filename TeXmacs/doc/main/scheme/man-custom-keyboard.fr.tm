<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Création de raccourcis clavier>

  Les correspondances clavier sont définies par la commande :

  <\verbatim>
    \ \ \ \ (kbd-map predicate . keymaps)
  </verbatim>

  La liste des prédicats définit dans quelles circonstances les
  correspondances clavier doivent être appliquées. Exemples de prédicats :
  <verbatim|always?>, <verbatim|in-math?> et <verbatim|in-french?>.
  L'utilisateur peut définir ses propres prédicats. Chaque correspondance
  doit avoir l'une des formes suivantes :\ 

  <\verbatim>
    \ \ \ \ (key-combination action_1 ... action_n)<format|next line>
    \ \ \ (key-combination result)<format|next line> \ \ \ (key-combination
    result help-message)
  </verbatim>

  Dans le premier cas, <verbatim|action_i> est une commande <apply|scheme>
  associée à la chaîne de caractères <verbatim|key-combination>. Dans les
  autres cas, <verbatim|result> est une chaîne de caractères à insérer dans
  le texte après que <verbatim|key-combination> a été exécutée. Un message
  d'aide <verbatim|help-message> peut être affiché à la fin de l'exécution de
  <verbatim|key-combination>.

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
