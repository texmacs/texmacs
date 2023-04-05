<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cr�ation de raccourcis clavier>

  Les correspondances clavier sont d�finies par la commande :

  <\verbatim>
    \ \ \ \ (kbd-map predicate . keymaps)
  </verbatim>

  La liste des pr�dicats d�finit dans quelles circonstances les
  correspondances clavier doivent �tre appliqu�es. Exemples de pr�dicats :
  <verbatim|always?>, <verbatim|in-math?> et <verbatim|in-french?>.
  L'utilisateur peut d�finir ses propres pr�dicats. Chaque correspondance
  doit avoir l'une des formes suivantes :\ 

  <\verbatim>
    \ \ \ \ (key-combination action_1 ... action_n)<format|next line>
    \ \ \ (key-combination result)<format|next line> \ \ \ (key-combination
    result help-message)
  </verbatim>

  Dans le premier cas, <verbatim|action_i> est une commande <apply|scheme>
  associ�e � la cha�ne de caract�res <verbatim|key-combination>. Dans les
  autres cas, <verbatim|result> est une cha�ne de caract�res � ins�rer dans
  le texte apr�s que <verbatim|key-combination> a �t� ex�cut�e. Un message
  d'aide <verbatim|help-message> peut �tre affich� � la fin de l'ex�cution de
  <verbatim|key-combination>.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

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
