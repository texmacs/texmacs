<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Commandes hybrides et simulation <LaTeX>>

  <apply|TeXmacs> vous permet de saisir des commandes <apply|LaTeX> à partir
  du clavier, comme expliqué ci-dessous. Pressez tout d'abord la touche
  <key|\\> pour entrer en mode commande hybride
  <apply|LaTeX>/<apply|TeXmacs>. Saisissez ensuite la commande que vous
  souhaitez exécuter. À la fin de la frappe, vous verrez dans le pied de page
  à gauche quelque chose de ce genre :

  <\verbatim>
    \ \ \ \ \<less\>entrée\<gtr\>: commande à exécuter
  </verbatim>

  À ce moment, si vous pressez la touche <key|entrée>, votre commande sera
  exécutée. Par exemple, en mode maths, vous pouvez créer une fraction avec
  <key|\\ f r a c entrée>.

  Si la commande que vous avez saisie n'est pas une commande <apply|LaTeX>
  reconnue, le programme cherchera tout d'abord s'il existe une macro, une
  fonction ou un environnement <apply|TeXmacs> correspondant fourni par le
  fichier de style. Si c'est le cas, il y aura exécution de la macro, de la
  fonction ou de l'environnement compte tenu des arguments fournis. Dans le
  cas contraire, le programme considère que vous voulez définir une variable
  d'environnement et vous demande sa valeur. La touche <key|\\> est toujours
  équivalente à l'une des commandes suivantes : <key|inactive l>,
  <key|inactive e>, <key|inactive a>, <key|inactive #> ou
  <key|inactive v>.

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
