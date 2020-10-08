<TeXmacs|1.99.13>

<style|<tuple|tmdoc|automate|french>>

<\body>
  <block-assign|<implied-scm|styles>|<implied-scm|(get-style-list)>>

  <block-assign|<implied-scm|style>|<implied-scm|(if (null? styles) "none"
  (car styles))>>

  <block-assign|<implied-scm|style-doc>|<implied-scm|(tmdoc-search-style
  style)>>

  <tmdoc-title|Aide contextuelle à la racine>

  <\unfolded-documentation|Édition à la racine du document>
    Le <help-link|focus actuel|main/text/man-structure> est sur le document
    en entier. Dans ce cas précis, la barre de focus (<abbr|c.à.d.> la
    troisième barre d'outils) affiche les caractéristiques principales de
    votre document, comme son style (<tmstyle|<output-string|<implied-scm|style>>>),
    la taille du papier (<output-string|<implied-scm|(get-init
    "page-type")>>), et la taille de la police
    (<output-string|<implied-scm|(get-init "font-base-size")>>pt). La barre
    de focus indique aussi la section dans laquelle se trouve votre curseur.

    Les propriétés et la section courante de ci-dessus peuvent également être
    modifiées utilisant les icônes et entrées sur la barre de focus, ou
    encore depuis le menu <menu|Focus>. Par exemple, en cliquant sur le style
    du document, un menu déroulant s'ouvre depuis lequel vous pouvez changer
    de style. En cliquant sur l'icône <icon|tm_add.xpm> après le style du
    document, vous pouvez également sélectuionner des paquetages de style
    supplémentaires.

    De même, en cliquant sur la section courante, un menu déroulant s'ouvre
    avec toutes les sections du document, ce qui vous permet de rapidement
    sauter vers la section de votre choix. Si le curseur se trouve au début
    du document et vous ne lui avez pas encore donné de titre, alors un
    bouton <menu|Title> à cet effet apparaîtra sur la barre de focus. De
    manière similaire, si votre curseur est juste après le titre, un bouton
    <menu|Abstract> peut apparaître pour saisir un résumé.
  </unfolded-documentation>

  <\unfolded-documentation|Style du document>
    <\block-if-else|<implied-scm|style-doc>>
      <block-output|<implied-scm|style-doc>>
    <|block-if-else>
      <\explain|<tmstyle|<output-string|<implied-scm|style>>>>
        Documentation indisponible.
      </explain>
    </block-if-else>
  </unfolded-documentation>

  <\block-if|<implied-scm|(and (nnull? styles) (nnull? (cdr styles)))>>
    <\unfolded-documentation|Autres personnalisations de style actives>
      <\block-for|<implied-scm|pack>|<implied-scm|(cdr styles)>>
        <block-assign|<implied-scm|pack-doc>|<\implied-scm>
          (tmdoc-search-style pack)
        </implied-scm>>

        <\block-if-else|<implied-scm|pack-doc>>
          <block-output|<implied-scm|pack-doc>>
        <|block-if-else>
          <\explain|<tmpackage|<output-string|<implied-scm|pack>>>>
            Documentation indisponible.
          </explain>
        </block-if-else>
      </block-for>
    </unfolded-documentation>
  </block-if>
</body>

<initial|<\collection>
</collection>>