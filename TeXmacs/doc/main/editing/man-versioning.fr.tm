<TeXmacs|1.99.10>

<style|<tuple|tmdoc|french|old-spacing>>

<\body>
  <tmdoc-title|Outil gestionnaire de version>

  Lors de l'écriture coopérative de documents, il arrive fréquemment qu'un
  auteur veuille explorer les changements faits par les autres, pour les
  accepter, les refuser ou même les corriger. Après avoir activer la gestion
  des versions à l'aide de : <menu|Tools|Versioning tool>, un menu spécial
  <menu|Version> apparaît dans la barre de menu principal, ce qui rend cette
  action automatique . Regardons plus en détail comment cet outil fonctionne.

  Pour le moment, des controles automatiques de version tels que
  <name|Subversion> ne sont pas encore supportés. Dans le futur, nous avons
  l'intention d'inclure le support de tels systèmes et le moyen d'assembler
  deux nouvelles versions.

  <paragraph*|Comparer deux versions>

  Supposons que nous ayons deux versions <verbatim|old.tm> et
  <verbatim|new.tm> du même document. Afin de visualiser les changements,
  premièrement chargeons la nouvelle version <verbatim|new.tm>, puis cliquons
  sur <menu|Version|File|Compare> et sélectionnons la vieille version
  <verbatim|old.tm>. Le tampon sera toujours nommé <verbatim|new.tm>, et les
  changements entre les deux versions seront indiqués par un marqueur
  spécial. Si il n'y a pas de changement, alors le curseur sera positionné
  sur la première différence.

  Il est possible de parcourir toutes les différences à la fois à partir des
  choix du sous-menu <menu|Version|Move>, ou en utilisant les raccourcis
  clavier <shortcut|(version-previous-difference)> and
  <shortcut|(version-next-difference)>. On peut aussi utiliser les raccourcis
  plus généraux de navigation structurée <shortcut|(traverse-first)>,
  <shortcut|(traverse-last)>, <shortcut|(traverse-previous)> and
  <shortcut|(traverse-next)>.

  <paragraph*|Visualisation des differences>

  Les différences entre deux versions peuvent être visualisées de trois
  manières : en visualisant seulement la vieille version, seulement la
  nouvelle, ou les deux à la fois. Dans tous les cas, la vieille version est
  affichée en rouge foncé et la nouvelle en vert foncé.

  Le style de visualisation peut être défini spécifiquement pour chaque
  différence, via <menu|Version|Show> ou les raccourcis claviers
  <shortcut|(version-show 'version-old)> (vieille version),
  <shortcut|(version-show 'version-new)> (nouvelle version) and
  <shortcut|(version-show 'version-both)> (toutes versions). On peut aussi
  parcourir les différents styles en utilisant les touches de variantes
  structurées <shortcut|(variant-circulate (focus-tree) #t)>. Si vous
  sélectionnez du texte, alors les actions décrites ci-dessus s'appliquent à
  toute la sélection. Le style de visualisation peut être défini globalement
  en utilisant <menu|Version|File|Show old version>, <menu|Version|File|Show
  new version> and <menu|Version|File|Show both versions>.

  <paragraph*|Accepter une version spécifique>

  Il arrive souvent que l'on veuille parcourir les changement entre deux
  versions et retenir l'une ou l'autre des différences rencontrées. Supposons
  que le curseur soit à l'intérieur d'une différence donnée, alors cela
  peut-être fait à l'aide du sous-menu <menu|Version|Retain>. D'une autre
  manière, on peut utiliser les raccourcis <shortcut|(version-retain 0)>,
  <shortcut|(version-retain 1)> et <shortcut|(version-retain 'current)> pour
  retenir respectivement l'ancienne, la nouvelle ou la version affichée. Si
  les deux versions sont affichées, alors <shortcut|(version-retain
  'current)> retient la nouvelle version. Après avoir retenu l'une des
  versions, nous sautons automatiquement à la prochaine différence, qui peut
  alors être traitée. Si vous sélectionnez une portion de texte, alors
  n'importe laquelle des actions citées au dessus, retiendra la version
  appropriée pour chaque différence de la sélection.

  Il est aussi possible de sélectionner globalement la vieille, la nouvelle
  ou la version courante en utilisant <menu|Version|File|Retain old version>,
  <menu|Version|File|Retain new version>, <abbr|resp.>
  <menu|Version|File|Retain current version>. Une alternative pratique pour
  traiter toutes les différences est d'utiliser
  <shortcut|(version-previous-difference)> et
  <shortcut|(version-next-difference)> pour les parcourir, d'utiliser
  <shortcut|(version-show 'version-old)> et <shortcut|(version-show
  'version-new)> pour sélectionner la version préférée, et ensuite de cliquer
  sur <menu|Version|File|Retain current version> aussitôt que toutes les
  différences aurons été traitées.

  <paragraph*|Controle fin et mise à jour des differences>

  Les entrées dans le sous-menu <menu|Version|Grain> permettent de contrôler
  la finesse avec laquelle les differences entre les versions sont calculées.
  Par défaut, nous utilisons le niveau <menu|Detailed>. Il est aussi possible
  de calculer les différences au niveau du paragraphe, en utilisant
  <menu|Block>. Dans se cas, c'est le paragraphe entier dans lequel un
  changement intervient,qui va être surligné. Le niveau de finesse le plus
  grossier <menu|Rough> va surligné le texte entier, si un changement est
  paru.

  La finesse est utilisée lors de la comparaison à l'aide de
  <menu|Version|File|Compare>, mais il est aussi possible de comparer à
  nouveau une portion de texte sélectionnée à l'aide de
  <menu|Version|Reactualize>. Cette dernière possibilité est fortement utile
  lors d'un changement du niveau de finesse.

  De façon similaire, le curseur étant à l'intérieur d'une différence, vous
  pouvez comparer à nouveau les deux versions en utilisant
  <menu|Version|Reactualize>. Cela peut-être utile si vous avez effectué des
  modifications dans l'une des versions. Par exemple, supposons que la
  vieille version contenait un théorème et que nous l'ayons changé en lemme
  dans la nouvelle version et que nous ayons aussi modifié des passages du
  texte à l'intérieur. Lorsque nous visualisons les changements, le théorème
  entier sera surligné, parce que il n'y a pas de marqueur approprié pour
  indiquer que nous avons juste changé un théorème en lemme. Néanmoins, si
  nous voulons comparer le texte à l'intérieur, nous pouvons changer le vieux
  théorème en lemme et ensuite utiliser <menu|Version|Reactualize>.

  <tmdoc-copyright|2010|Joris van der Hoeven|Denis Raux>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>