<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Règles générales applicables aux préfixes>

  Comme il existe de nombreux raccourcis clavier, il est important de définir
  des règles de classification de façon à les mémoriser facilement. En
  général, les raccourcis clavier d'une même catégorie ont le même préfixe.
  Les préfixes les plus courants sont :\ 

  <\description>
    <expand|item*|<key|C-<with|mode|math|x>>>Les raccourcis clavier basés sur
    la touche contrôle sont utilisés pour les commandes d'édition courantes.
    Ils dépendent de l'apparence définie avec
    <apply|menu|Édition|Préférences>. Si vous utilisez une apparence
    <name|Emacs>, les raccourcis clavier de forme <key|C-<with|mode|math|x>>
    correspondent aux commandes <name|Emacs>. Exemple : <key|C-y> pour coller
    du texte.

    <expand|item*|<key|A-<with|mode|math|x>>>La touche alt est utilisée pour
    les commandes qui dépendent du mode dans lequel vous êtes. Par exemple,
    <expand|kbd-text|s> génère du texte <strong|important> en mode texte et
    une racine carrée <with|mode|math|<sqrt|>> en mode maths. Notez que
    <key|escape escape> est équivalent à <key|A->.

    <expand|item*|<key|M-<with|mode|math|x>>>La touche meta est utilisée pour
    les commandes <apply|TeXmacs> générales, celles qui peuvent être
    utilisées dans tous les modes. Par exemple, <expand|kbd-gen|!> génère une
    étiquette. Elle est aussi utilisée pour certaines commandes d'édition
    avec l'apparence Emacs, comme <key|A-w> pour copier du texte. Notez que
    <key|escape> est équivalent à <key|M->.

    <expand|item*|<key|H-<with|mode|math|x>>>La touche hyper est utilisée
    pour générer des symboles spéciaux, tels les caractères grecs en mode
    maths. Vous pouvez configurer votre clavier de telle sorte que la touche
    majuscule fixe joue le rôle de la touche hyper. La touche de fonction
    <key|F5> est équivalente à <key|M->.
  </description>

  Rappelons que les touches spéciales de modification utilisées pour générer
  les préfixes <key|M-> et <key|H-> peuvent être
  <apply|hyper-link|configurées|../../config/man-config-kbd-modkeys.fr.tm>
  avec <apply|menu|Édition|Préférences>.

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
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Édition>|<with|font
      family|<quote|ss>|Préférences>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Édition>|<with|font
      family|<quote|ss>|Préférences>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
