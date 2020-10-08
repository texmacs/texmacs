<TeXmacs|1.99.13>

<style|<tuple|tmdoc|automate|french>>

<\body>
  <block-assign|<implied-scm|t>|<implied-scm|(focus-tree)>>

  <block-assign|<implied-scm|tag>|<implied-scm|(tree-label t)>>

  <block-assign|<implied-scm|n>|<implied-scm|(tree-arity t)>>

  <block-assign|<implied-scm|nl>|<implied-scm|(focus-doc-arg-names t 0 '())>>

  <block-assign|<implied-scm|tag-doc>|<implied-scm|(tmdoc-search-tag tag)>>

  <tmdoc-title|Aide contextuelle sur la balise \S
  <output-string|<implied-scm|tag>> \T>

  <\block-if-else|<implied-scm|tag-doc>>
    <\unfolded-documentation|Description>
      <block-output|<implied-scm|tag-doc>>
    </unfolded-documentation>
  <|block-if-else>
    <\unfolded-documentation|Usage>
      <\explain|<inline-output|<implied-scm|`(explain-macro
      ,(symbol-\<gtr\>string tag) ,@nl)>>>
        <\block-texmacs-tag|description-aligned>
          <\block-for|<implied-scm|i>|<implied-scm|(.. 0 n)>>
            <inline-texmacs-tag|item*|<src-arg|<output-string|<implied-scm|(list-ref
            nl i)>>>><inline-if-else|<implied-scm|(== (list-ref nl i)
            "body")>|Le corps principal de la macro.|Un argument de type
            \P<output-string|<implied-scm|(tree-child-type t i)>>\Q.>
          </block-for>
        </block-texmacs-tag>
      </explain>
    </unfolded-documentation>
  </block-if-else>

  <\block-if|<implied-scm|(tree-label-extension? tag)>>
    <\unfolded-documentation|Définition actuelle>
      <\surround||<vspace|2fn>>
        <\tm-fragment>
          <inline-texmacs-tag|inactive*|<inline-texmacs-tag|assign|<output-string|<implied-scm|tag>>|<inline-output|<implied-scm|(get-env-tree
          (symbol-\<gtr\>string tag))>>>>
        </tm-fragment>
      </surround>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(or (focus-has-variants? t) (focus-has-toggles?
  t))>>
    <\unfolded-documentation|Variantes structurées>
      <\block-if|<implied-scm|(focus-has-variants? t)>>
        La balise <markup|<output-string|<implied-scm|tag>>> admet plusieurs
        <help-link|variantes structurées|main/editing/man-structured-variants>
        : <inline-intersperse|, |<inline-for|<implied-scm|v>|<implied-scm|(variants-of
        tag)>|<markup|<output-string|<implied-scm|v>>>>>. Vous pouvez faire
        tourner ces variantes via les raccourcis clavier
        <shortcut|(variant-circulate (focus-tree) #t)> et
        <shortcut|(variant-circulate (focus-tree) #f)>. Vous pouvez aussi
        sélectionner une variante spécifique dans le menu
        <menu|Focus|<output-string|<implied-scm|(focus-tag-name tag)>>> ou
        depuis le menu <menu|<output-string|<implied-scm|(focus-tag-name
        tag)>>> sur la barre de focus.
      </block-if>

      <\block-if|<implied-scm|(numbered-context? t)>>
        Pour la plupart de styles, l'environnement
        <markup|<output-string|<implied-scm|tag>>>
        <inline-if-else|<implied-scm|(symbol-numbered? tag)>|est
        numéroté|n'est pas numéroté>. L'environment admet une variante
        <inline-if-else|<implied-scm|(symbol-numbered? tag)>|non
        numérotée|numérotée> <markup|<output-string|<implied-scm|(symbol-toggle-number
        tag)>>>. Vous pouvez basculer la numérotation avec le raccourci
        clavier <shortcut|(numbered-toggle (focus-tree))>, l'entrée de menu
        <menu|Focus|Numbered>, où en cliquant sur l'icône
        <icon|tm_numbered.xpm> sur la barre de focus.
      </block-if>

      <\block-if|<implied-scm|(alternate-context? t)>>
        <\block-if|<implied-scm|(alternate-first? t)>>
          L'environnement <markup|<output-string|<implied-scm|tag>>> est \S
          plié \T et <markup|<output-string|<implied-scm|tag>>> admet la
          variante dépliée <markup|<output-string|<implied-scm|(symbol-toggle-alternate
          tag)>>>. Vous pouvez déplier l'environnement utilisant le raccourci
          <shortcut|(alternate-toggle (focus-tree))>, l'entrée de menu
          <menu|Focus|Folded>, ou en cliquant sur l'icône
          <icon|tm_alternate_first.xpm> sur la barre de focus.
        </block-if>

        <\block-if|<implied-scm|(alternate-second? t)>>
          L'environnement <markup|<output-string|<implied-scm|tag>>> est \S
          déplié \T et <markup|<output-string|<implied-scm|tag>>> admet la
          variante repliée <markup|<output-string|<implied-scm|(symbol-toggle-alternate
          tag)>>>. Vous pouvez plier l'environnement utilisant le raccourci
          <shortcut|(alternate-toggle (focus-tree))>, l'entrée de menu
          <menu|Focus|<output-string|<implied-scm|(alternate-second-name
          t)>>>, ou en cliquant sur l'icône
          <inline-texmacs-tag|icon|<output-string|<implied-scm|(alternate-second-icon
          t)>>> sur la barre de focus.
        </block-if>

        En traversant un document en <help-link|mode de
        présentation|main/beamer/man-beamer>, des environnements pliables
        sont pliés et dépliés automatiquement.
      </block-if>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-has-preferences? t)>>
    <\unfolded-documentation|Préférences de style>
      <block-assign|<implied-scm|opts>|<implied-scm|(search-tag-options t)>>

      <block-assign|<implied-scm|pars>|<\implied-scm>
        (list-filter (search-tag-parameters t) parameter-show-in-menu?)
      </implied-scm>>

      <block-assign|<implied-scm|thms>|<\implied-scm>
        (search-tag-themes t)
      </implied-scm>>

      La rendue de la balise <markup|<output-string|<implied-scm|tag>>> peut
      être personnalisée en éditant la macro qui la définit. Pour ceci, il
      suffit de cliquer sur l'entrée <menu|Edit macro> du menu
      <menu|Focus|Preferences> menu (ou du menu sous l'icône
      <icon|tm_focus_prefs.xpm> de la barre de focus). Vous pouvez aussi
      éditer directement la macro dans le fichier de style où elle est
      définie, utilisant <menu|Edit source>.

      <\block-if|<implied-scm|(nnull? (append opts pars thms))>>
        Toujours dans le menu <menu|Focus|Preferences> menu, il est possible
        de spécifier <inline-if|<implied-scm|(and (nnull? opts) (null?
        pars))>|des options de style><inline-if|<implied-scm|(and (null?
        opts) (nnull? pars))>|des paramètres de
        style><inline-if|<implied-scm|(and (nnull? opts) (nnull? pars))>|des
        options et des paramètres de style> qui s'appliquent à la balise
        <markup|<output-string|<implied-scm|tag>>>. Ces réglages sont
        globaux, donc il s'appliquent à toutes les balises
        <markup|<output-string|<implied-scm|tag>>> dans votre document, et
        même à toutes les balises similaires.
      </block-if>

      <\block-if|<implied-scm|(nnull? thms)>>
        La balise <markup|<output-string|<implied-scm|tag>>> utilise des
        thèmes pour sa rendue. Ces thèmes viennent avec leurs propres
        paramètres de style qui peuvent être personnalisés via
        <menu|Focus|Preferences|Theme parameters>.
      </block-if>

      <\block-if|<implied-scm|(nnull? opts)>>
        <\folded|<strong|Options de style>>
          <\block-for|<implied-scm|opt>|<implied-scm|opts>>
            <block-assign|<implied-scm|opt-doc>|<\implied-scm>
              (tmdoc-search-style opt)
            </implied-scm>>

            <block-assign|<implied-scm|sty-doc>|<\implied-scm>
              (style-get-documentation opt)
            </implied-scm>>

            <\block-if-else|<implied-scm|opt-doc>>
              <block-output|<implied-scm|opt-doc>>
            <|block-if-else>
              <\explain|<tmpackage|<output-string|<implied-scm|opt>>><explain-synopsis|<output-string|<implied-scm|(style-get-menu-name
              opt)>>>>
                <\block-if-else|<implied-scm|sty-doc>>
                  <inline-output|<implied-scm|sty-doc>>.
                <|block-if-else>
                  Paquetage de style non documenté.
                </block-if-else>
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-if>

      <\block-if|<implied-scm|(nnull? pars)>>
        <\folded|<strong|Paramètres de style>>
          <\block-for|<implied-scm|par>|<implied-scm|pars>>
            <block-assign|<implied-scm|par-doc>|<\implied-scm>
              (tmdoc-search-parameter par)
            </implied-scm>>

            <\block-if-else|<implied-scm|par-doc>>
              <block-output|<implied-scm|par-doc>>
            <|block-if-else>
              <\explain|<src-var|<output-string|<implied-scm|par>>>>
                Un paramètre de type <output-string|<implied-scm|(tree-label-type
                (string-\<gtr\>symbol par))>>.
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-if>

      <\block-for|<implied-scm|thm>|<implied-scm|thms>>
        <\folded|<strong|Paramètres de style pour le thème
        <output-string|<implied-scm|thm>>>>
          <\block-for|<implied-scm|par>|<implied-scm|(theme-\<gtr\>members
          thm)>>
            <block-assign|<implied-scm|par-doc>|<\implied-scm>
              (tmdoc-search-parameter par)
            </implied-scm>>

            <\block-if-else|<implied-scm|par-doc>>
              <block-output|<implied-scm|par-doc>>
            <|block-if-else>
              <\explain|<src-var|<output-string|<implied-scm|par>>>>
                Un paramètre de type <output-string|<implied-scm|(tree-label-type
                (string-\<gtr\>symbol par))>>.
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-for>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-can-move? t)>>
    <\unfolded-documentation|Navigation structurée>
      Il est parfois utile de traverser rapidement toutes les balises
      <markup|<output-string|<implied-scm|tag>>> et ses variantes qui se
      trouvent dans un document. Ceci peut se faire efficacement utilisant
      des raccourcis claviers, des entrées de menu, ou des icônes sur la
      barre de focus :

      <\description-long>
        <item*|<shortcut|(kbd-select-if-active traverse-first)>,
        <menu|Focus|First similar>, <icon|tm_similar_first.xpm>>Aller vers la
        première balise similaire à <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-previous)>,
        <menu|Focus|Previous similar>, <icon|tm_similar_previous.xpm>>Aller
        vers la balise précédente et similaire à
        <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-next)>,
        <menu|Focus|Next similar>, <icon|tm_similar_next.xpm>>Aller vers la
        balise suivante et similaire à <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-last)>,
        <menu|Focus|Last similar>, <icon|tm_similar_last.xpm>>Aller vers la
        dernière balise similaire à <markup|<output-string|<implied-scm|tag>>>.
      </description-long>

      Pour plus de renseignements et d'autres raccourcis clavier utils, nous
      vous renvoyons vers la section sur les <help-link|mouvements de curseur
      structurés|main/editing/man-structured-move>.
    </unfolded-documentation>
  </block-if>

  <\block-if-else|<implied-scm|(and (defined? 'any-table-tag?)
  (any-table-tag? tag))>>
    <\unfolded-documentation|Insertions et suppressions structurées>
      La balise <markup|<output-string|<implied-scm|tag>>> est un
      environnement tabulaire. De nouvelles lignes et colonnes peuvent être
      crées de manière suivante :

      <\description-long>
        <item*|<shortcut|(structured-insert-left)>, <menu|Focus|Insert left>,
        <icon|tm_insert_left.xpm>>Insérer une nouvelle colonne à gauche du
        curseur.

        <item*|<shortcut|(structured-insert-right)>, <menu|Focus|Insert
        right>, <icon|tm_insert_right.xpm>>Insérer une nouvelle colonne à
        droite du curseur.

        <item*|<shortcut|(structured-insert-up)>, <menu|Focus|Insert above>,
        <icon|tm_insert_up.xpm>>Insérer une nouvelle ligne au dessus du
        curseur.

        <item*|<shortcut|(structured-insert-down)>, <menu|Focus|Insert
        below>, <icon|tm_insert_down.xpm>>Insérer une nouvelle ligne en
        dessous du curseur.
      </description-long>

      Des lignes et des colonnes existantes peuvent être supprimées comme
      suit :

      <\description-long>
        <item*|<shortcut|(structured-remove-left)>, <menu|Focus|Remove
        leftwards>, <icon|tm_delete_left.xpm>>Supprimer la colonne à gauche
        du curseur.

        <item*|<shortcut|(structured-remove-right)>, <menu|Focus|Remove
        rightwards>, <icon|tm_delete_right.xpm>>Supprimer la colonne avec le
        curseur et aller vers la colonne d'après.

        <item*|<menu|Focus|Remove upwards>, <icon|tm_delete_up.xpm>>Supprimer
        la ligne au dessus du curseur.

        <item*|<menu|Focus|Remove downwards>,
        <icon|tm_delete_down.xpm>>Supprimer la ligne avec le curseur et aller
        vers la ligne en dessous.
      </description-long>
    </unfolded-documentation>
  <|block-if-else>
    <\block-if-else|<implied-scm|(and (defined? 'field-tags) (tm-in? t
    field-tags))>>
      <\unfolded-documentation|Insertions et suppressions structurées>
        La balise <markup|<output-string|<implied-scm|tag>>> est utilisée à
        l'intérieur de sessions interactives. De nouveaux champs d'entrée
        peuvent être insérés de manière suivante :

        <\description-long>
          <item*|<shortcut|(structured-insert-up)>, <menu|Focus|Insert field
          above>, <icon|tm_insert_up.xpm>>Insérer un nouveau champ d'entrée
          au dessus du curseur.

          <item*|<shortcut|(structured-insert-down)>, <menu|Focus|Insert
          field below>, <icon|tm_insert_down.xpm>>Insérer un nouveau champ
          d'entrée en dessous du curseur.
        </description-long>

        Des champs d'entrée et de sortie existants peuvent être supprimés
        comme suit :

        <\description-long>
          <item*|<menu|Focus|Remove field above>,
          <icon|tm_delete_up.xpm>>Supprimer le champ au dessus du curseur.

          <item*|<menu|Focus|Remove field below>,
          <icon|tm_delete_down.xpm>>Supprimer le champ courant et aller au
          champ suivant.

          <item*|<menu|Focus|Remove banner>>Supprimer la bannière de
          démarrage de la session.

          <item*|<menu|Focus|Remove last field>>Supprimer le dernier champ de
          la session.
        </description-long>
      </unfolded-documentation>
    <|block-if-else>
      <\block-if|<implied-scm|(or (structured-horizontal? t)
      (structured-vertical? t))>>
        <\unfolded-documentation|Insertions et suppressions structurées>
          La balise <markup|<output-string|<implied-scm|tag>>> admet un
          nombre variable d'arguments. De nouveaux arguments peuvent être
          insérés de manière suivante :

          <\block-texmacs-tag|description-long>
            <\block-if|<implied-scm|(structured-horizontal? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-insert-left)>,
              <menu|Focus|Insert left>, <icon|tm_insert_left.xpm>>Insérer un
              nouvel argument à gauche du curseur.

              <inline-texmacs-tag|item*|<shortcut|(structured-insert-right)>,
              <menu|Focus|Insert right>, <icon|tm_insert_right.xpm>>Insérer
              un nouvel argument à droite du curseur.
            </block-if>

            <\block-if|<implied-scm|(structured-vertical? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-insert-up)>,
              <menu|Focus|Insert above>, <icon|tm_insert_up.xpm>>Insérer un
              nouvel argument au dessus du curseur.

              <inline-texmacs-tag|item*|<shortcut|(structured-insert-down)>,
              <menu|Focus|Insert below>, <icon|tm_insert_down.xpm>>Insérer un
              nouvel argument en dessous du curseur.
            </block-if>
          </block-texmacs-tag>

          Des arguments existants peuvent être supprimés comme suit :

          <\block-texmacs-tag|description-long>
            <\block-if|<implied-scm|(structured-horizontal? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-remove-left)>,
              <menu|Focus|Remove leftwards>,
              <icon|tm_delete_left.xpm>>Supprimer l'argument à gauche du
              curseur.

              <inline-texmacs-tag|item*|<shortcut|(structured-remove-right)>,
              <menu|Focus|Remove rightwards>,
              <icon|tm_delete_right.xpm>>Supprimer l'argument courant et
              aller vers l'argument suivant.
            </block-if>

            <\block-if|<implied-scm|(structured-vertical? t)>>
              <inline-texmacs-tag|item*|<menu|Focus|Remove upwards>,
              <icon|tm_delete_up.xpm>>Supprimer l'argument au dessus du
              curseur.

              <inline-texmacs-tag|item*|<menu|Focus|Remove downwards>,
              <icon|tm_delete_down.xpm>>Supprimer l'argument courant et aller
              vers la l'argument en dessous.
            </block-if>
          </block-texmacs-tag>
        </unfolded-documentation>
      </block-if>
    </block-if-else>
  </block-if-else>

  <\block-if|<implied-scm|(\<less\> (length (tree-accessible-children t))
  n)>>
    <\unfolded-documentation|Les arguments cachés>
      <block-assign|<implied-scm|il>|<implied-scm|(list-filter (.. 0 (length
      nl)) (lambda (i) (not (tree-accessible-child? t i))))>>

      <block-assign|<implied-scm|sl>|<implied-scm|(map (lambda (i) (list-ref
      nl i)) il)>>

      Quand la balise <markup|<output-string|<implied-scm|tag>>> est
      <help-link|activée|main/text/keyboard/man-dynamic>, les arguments
      suivants sont cachés : <inline-intersperse|,
      |<inline-for|<implied-scm|x>|<implied-scm|sl>|<src-arg|<output-string|<implied-scm|x>>>>>.
      Pour éditer ces arguments, vous pouvez utiliser <menu|Focus|Show
      hidden> ou cliquer sur l'icône<nbsp><icon|tm_show_hidden.xpm> de la
      barre de focus. Des balises désactivées peuvent être réactivées en
      appuyant sur<nbsp><shortcut|(kbd-return)>.

      Certains arguments cachés de type chaîne de caractère peuvent aussi
      être édités directement via les champs texte sur la barre de focus ;
      vous n'avez pas besoin de désactiver la balise dans ce cas.
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-has-geometry? t)>>
    <\unfolded-documentation|Géométrie structurée>
      Non documenté pour l'instant.
    </unfolded-documentation>
  </block-if>
</body>

<initial|<\collection>
</collection>>