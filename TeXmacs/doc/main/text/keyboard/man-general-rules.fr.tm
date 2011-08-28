<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Règles générales applicables aux préfixes>

  Comme il existe de nombreux raccourcis clavier, il est important de définir
  des règles de classification de façon à les mémoriser facilement. En
  général, les raccourcis clavier d'une même catégorie ont le même préfixe.
  Les préfixes actives dépendent fortement du mode de clavier choisi dans
  <menu|Éditer|Préférences|Aspect de l'interface>. Dans l'interface actuelle
  de <TeXmacs>, les préfixes les plus courants sont :\ 

  <\description>
    <item*|<prefix|C->>Les raccourcis clavier basés sur la touche contrôle
    sont utilisés pour les commandes d'édition courantes. Ils dépendent de
    l'\Sapparence\T définie avec <menu|Édition|Préférences>. Si vous utilisez
    une apparence <name|Emacs>, les raccourcis clavier de forme <prefix|C->
    correspondent aux commandes <name|Emacs>. Exemple : <key|C-y> pour coller
    du texte.

    <item*|<prefix|A->>La touche alt est utilisée pour les commandes qui
    dépendent du mode dans lequel vous êtes. Par exemple, <key|text s> génère
    du texte <strong|important> en mode texte et une racine carrée
    <math|<sqrt|>> en mode maths. Notez que <key|escape escape> est
    équivalent à <prefix|A->.

    <item*|<prefix|M->>La touche meta est utilisée pour les commandes
    <TeXmacs> générales, celles qui peuvent être utilisées dans tous les
    modes. Par exemple, <key|executable> est utilisé pour insérer une balise
    executable, ce qui est utile pour l'<hlink|écriture de fichiers de
    styles|../../../devel/style/style.fr.tm>. Le raccourci <key|executable +>
    constitue un exemple pour l'insertion d'une addition.

    <item*|<prefix|M-A->>La touche hyper est utilisée pour générer des
    symboles spéciaux, tels les caractères grecs en mode maths. Vous pouvez
    configurer votre clavier de telle sorte que la touche majuscule fixe joue
    le rôle de la touche hyper. La touche de fonction <prefix|math:greek> est
    équivalente à <prefix|M->.
  </description>

  Rappelons que les touches spéciales de modification utilisées pour générer
  les préfixes <prefix|M-> et <prefix|M-A-> peuvent être
  <hlink|configurées|../../config/man-config-kbd-modkeys.fr.tm> avec
  <menu|Édition|Préférences>.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Michèle Garoche, Daouda
  Niang Diatta>

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