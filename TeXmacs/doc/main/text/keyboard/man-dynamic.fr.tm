<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Objets dynamiques>

  Certains objets complexes passent par plusieurs <em|états> durant
  l'édition. Les étiquettes et références, par exemple, sont des <em|objets
  dynamiques>, car leur apparence dépend d'un nombre déterminé dynamiquement.
  Vous trouverez de nombreux autres exemples de marquage dynamique dans la
  section <apply|hyper-link|création de fichiers de
  style|../../../devel/style/keyboard/style-kbd.fr.tm>.

  À la création d'un objet dynamique, telle une étiquette avec
  <shortcut|(make-label)>, son état est <em|inactif> par défaut. Cet état inactif
  vous permet de saisir les informations nécessaires à la création de l'objet
  dynamique, tel le nom de l'étiquette dans notre cas. Certains objets
  dynamiques peuvent prendre un nombre arbitraire de paramètres ; on insère
  les paramètres supplémentaires avec <key|tab>.

  À la fin de la saisie des données concernant l'objet dynamique, appuyez sur
  <key|entrée> pour <em|activer> l'objet. Un objet dynamique peut être
  désactivé en plaçant le curseur juste derrière l'objet et en appuyant sur
  <key|retour arrière>.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
  </collection>
</references>
