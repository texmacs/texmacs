<TeXmacs|1.0.1.10>

<style|<tuple|tmdoc|pari|maxima>>

<\body>
  <expand|tmdoc-title|Utilisation normale>

  On démarre une session avec <apply|menu|Insert|Session>. Une session consiste
  en une série d'environnements d'entrée et de sortie entrecoupée
  éventuellement de texte. Quand on appuie sur la touche <key|retour chariot>
  à l'intérieur d'un environnement d'entrée de session, le texte situé dans
  l'environnement d'entrée est évalué et son résultat est affiché dans un
  environnement de sortie.

  Quand on entre une commande dans une session, l'application tente de
  l'exécuter. Plusieurs commandes peuvent être lancées concurremment dans le
  même document pendant la même session, mais la sortie ne sera effective
  qu'à l'endroit du curseur. C'est pourquoi il vaut mieux utiliser des
  tampons différents pour des exécutions en parallèle. On peut interrompre
  l'exécution à partir de la barre d'icônes. On peut aussi clore
  l'application ; dans ce cas, on ne pourra plus exécuter de commande dans la
  session correspondante.

  Dans la barre d'icônes de session, il y a plusieurs boutons pour choisir le
  mode d'entrée des données (si applicable), interrompre l'exécution ou clore
  la session (déconnecter le système).

  Vous pouvez entrer des données mathématiques sous forme graphique à deux
  dimensions, si le système le permet. L'interruption de l'exécution d'une
  commande ne fonctionne pas toujours très bien avec certains systèmes. Si
  vous appuyez sur <key|retour chariot> dans l'entrée d'un système
  déconnecté, il sera immédiatement reconnecté.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Michèle Garoche>

  <expand|tmdoc-license|Permission is<em|> granted to copy, distribute and/or
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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
