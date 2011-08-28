<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Configuration des touches spéciales>

  <apply|TeXmacs> utilise 5 touches spéciales : <key|majuscule temporaire>,
  <key|ctrl>, <key|alt>, <key|meta> et <key|hyper>, dont les abréviations
  seront les suivantes dans ce guide : <prefix|S->, <prefix|C->, <prefix|A->, <prefix|M->
  et <prefix|M-A->. Les touches <key|majuscule temporaire> et <key|ctrl> sont
  présentes sur tous les claviers et la touche <key|alt> sur la plupart
  d'entre eux. La majorité des claviers des PC récents ont aussi une touche
  <key|windows>, qui est l'équivalent de la touche <key|meta> de <TeXmacs>.

  Avant de vous lancer dans la reconfiguration de votre clavier, vérifiez que
  cela est vraiment nécessaire. Si votre clavier possède des touches
  correspondantes aux touches <key|majuscule>, <key|ctrl>, <key|alt> et
  <key|meta> et qui fonctionnent de façon satisfaisante, il est plus que
  probable que vous n'ayez rien à faire. Cependant, si vous désirez utiliser
  la touche <key|majuscule fixe> pour saisir des symboles mathématiques, vous
  devrez alors faire correspondre la touche <key|majuscule fixe> à la touche
  <key|hyper>.

  Pour reconfigurer votre clavier, sélectionner la touche logique à
  reconfigurer et faites-la correspondre à la touche physique désirée avec
  <apply|menu|Edit|Preferences|Keyboard>. Par exemple, en sélectionnant
  <apply|menu|Windows key|Envoyer vers modificateur M>, la touche
  <key|windows> sera mise en correspondance avec la touche <key|meta>. De
  même, en sélectionnant <apply|menu|Touche Majuscule fixe|Envoyer vers
  modificateur H>, la touche <key|majuscule fixe> sera mise en correspondance
  avec la touche <key|hyper>.

  Malheureusement, X Window ne permet qu'une reconfiguration globale. Si,
  donc, vous reconfigurez la touche <key|majuscule fixe> dans
  <apply|TeXmacs>, alors cette reconfiguration s'appliquera aussi à toutes
  les autres applications. Par conséquent, il est primordial que vous ne
  reconfiguriez que les touches que vous n'utilisez jamais dans d'autres
  applications. Par exemple, la touche <key|windows> n'est pas tellement
  utilisée, il est donc en général possible de la reconfigurer sans que cela
  tire à conséquences. Vous pouvez aussi changer la configuration globale de
  façon appropriée. Utilisez pour ce faire la commande <verbatim|xmodmap> ;
  voir la page man correspondante pour de plus amples informations.

  Dans certains cas, votre clavier possède les touches <key|alt>, <key|meta>
  et <key|hyper>, mais leur fonctionnement ne vous convient pas. Vous pouvez
  alors changer leur comportement en faisant correspondre les touches
  <prefix|A->, <prefix|M-> et <prefix|M-A-> à d'autres touches logiques dans le premier
  groupe de sous-menus du menu <apply|menu|Edit|Preferences|Keyboard>.

  Par exemple et pour des raisons de compatibilité avec Emacs, vous pouvez
  échanger les touches <key|meta> ou <key|windows> avec la touche <key|alt>
  sans pour autant que ce changement soit global. Pour ce faire, recherchez
  quelles sont les modificateurs qui correspondent à ces touches ; en
  général, <key|Mod1> correspond à <key|alt> et <key|Mod4> à <key|meta> ou
  <key|windows>. Vous effectuerez ensuite le changement dans
  <apply|menu|Edit|Preferences|Keyboard>, en sélectionnant
  <apply|menu|Modificateur A|Équivalent de Mod4> et <apply|menu|Modificateur
  M|Équivalent de Mod1>.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Préférences>|<with|font
      family|<quote|ss>|Clavier>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Touche windows>|<with|font
      family|<quote|ss>|Envoyer vers modificateur M>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Touche Majuscule
      fixe>|<with|font family|<quote|ss>|Envoyer vers modificateur
      H>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Préférences>|<with|font
      family|<quote|ss>|Clavier>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Éditer>|<with|font
      family|<quote|ss>|Préférences>|<with|font
      family|<quote|ss>|Clavier>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modificateur A>|<with|font
      family|<quote|ss>|Équivalent de Mod4>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modificateur M>|<with|font
      family|<quote|ss>|Équivalent de Mod1>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
