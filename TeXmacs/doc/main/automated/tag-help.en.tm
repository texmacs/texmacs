<TeXmacs|1.99.13>

<style|<tuple|tmdoc|automate>>

<\body>
  <block-assign|<implied-scm|t>|<implied-scm|(focus-tree)>>

  <block-assign|<implied-scm|tag>|<implied-scm|(tree-label t)>>

  <block-assign|<implied-scm|n>|<implied-scm|(tree-arity t)>>

  <block-assign|<implied-scm|nl>|<implied-scm|(focus-doc-arg-names t 0 '())>>

  <block-assign|<implied-scm|tag-doc>|<implied-scm|(tmdoc-search-tag tag)>>

  <tmdoc-title|Contextual help on the \P<output-string|<implied-scm|tag>>\Q
  tag>

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
            "body")>|The main body of the macro.|An argument of type
            \P<output-string|<implied-scm|(tree-child-type t i)>>\Q.>
          </block-for>
        </block-texmacs-tag>
      </explain>
    </unfolded-documentation>
  </block-if-else>

  <\block-if|<implied-scm|(tree-label-extension? tag)>>
    <\unfolded-documentation|Current definition>
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
    <\unfolded-documentation|Structured variants>
      <\block-if|<implied-scm|(focus-has-variants? t)>>
        The <markup|<output-string|<implied-scm|tag>>> tag has several
        <help-link|structured variants|main/editing/man-structured-variants>:
        <inline-intersperse|, |<inline-for|<implied-scm|v>|<implied-scm|(variants-of
        tag)>|<markup|<output-string|<implied-scm|v>>>>>. You may circle
        among these variants using the keyboard shortcuts
        <shortcut|(variant-circulate (focus-tree) #t)> and
        <shortcut|(variant-circulate (focus-tree) #f)>. You may also pick a
        specific variant from the <menu|Focus|<output-string|<implied-scm|(focus-tag-name
        tag)>>> menu or the <menu|<output-string|<implied-scm|(focus-tag-name
        tag)>>> menu on the focus toolbar.
      </block-if>

      <\block-if|<implied-scm|(numbered-context? t)>>
        For most style files, the <markup|<output-string|<implied-scm|tag>>>
        environment is <inline-if-else|<implied-scm|(symbol-numbered?
        tag)>|numbered|unnumbered>. The environment has
        <inline-if-else|<implied-scm|(symbol-numbered? tag)>|an unnumbered|a
        numbered> variant <markup|<output-string|<implied-scm|(symbol-toggle-number
        tag)>>>. You may toggle the numbering using the keyboard shortcut
        <shortcut|(numbered-toggle (focus-tree))>, the menu entry
        <menu|Focus|Numbered>, or by pressing the <icon|tm_numbered.xpm> icon
        on the focus toolbar.
      </block-if>

      <\block-if|<implied-scm|(alternate-context? t)>>
        <\block-if|<implied-scm|(alternate-first? t)>>
          The <markup|<output-string|<implied-scm|tag>>> environment is
          \Pfolded\Q and has an unfolded variant
          <markup|<output-string|<implied-scm|(symbol-toggle-alternate
          tag)>>>. You may unfold the environment using the keyboard shortcut
          <shortcut|(alternate-toggle (focus-tree))>, the menu entry
          <menu|Focus|Folded>, or by pressing the
          <icon|tm_alternate_first.xpm> icon on the focus toolbar.
        </block-if>

        <\block-if|<implied-scm|(alternate-second? t)>>
          The <markup|<output-string|<implied-scm|tag>>> environment is
          \Punfolded\Q and has a folded variant
          <markup|<output-string|<implied-scm|(symbol-toggle-alternate
          tag)>>>. You may fold the environment using the keyboard shortcut
          <shortcut|(alternate-toggle (focus-tree))>, the menu entry
          <menu|Focus|<output-string|<implied-scm|(alternate-second-name
          t)>>>, or by pressing the <inline-texmacs-tag|icon|<output-string|<implied-scm|(alternate-second-icon
          t)>>> icon on the focus toolbar.
        </block-if>

        Folding and unfolding is performed automatically during the traversal
        of a document in <help-link|presentation
        mode|main/beamer/man-beamer>.
      </block-if>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-has-preferences? t)>>
    <\unfolded-documentation|Style preferences>
      <block-assign|<implied-scm|opts>|<implied-scm|(search-tag-options t)>>

      <block-assign|<implied-scm|pars>|<\implied-scm>
        (list-filter (search-tag-parameters t) parameter-show-in-menu?)
      </implied-scm>>

      <block-assign|<implied-scm|thms>|<\implied-scm>
        (search-tag-themes t)
      </implied-scm>>

      The rendering of the <markup|<output-string|<implied-scm|tag>>> tag can
      be customized by editing the macro which defines it. This can be done
      by clicking on <menu|Edit macro> button in the <menu|Focus|Preferences>
      menu (or in the equivalent <icon|tm_focus_prefs.xpm> icon menu on the
      focus toolbar). You may also directly edit the macro in the style file
      or package where it was defined, using <menu|Edit source>.

      <\block-if|<implied-scm|(nnull? (append opts pars thms))>>
        Still using the <menu|Focus|Preferences> menu, you may also specify
        <inline-if|<implied-scm|(and (nnull? opts) (null? pars))>|style
        options><inline-if|<implied-scm|(and (null? opts) (nnull?
        pars))>|style parameters><inline-if|<implied-scm|(and (nnull? opts)
        (nnull? pars))>|style options and parameters> that apply to the
        <markup|<output-string|<implied-scm|tag>>> tag. These settings are
        global, so they will apply to all other
        <markup|<output-string|<implied-scm|tag>>> tags in your document, and
        generally also to other similar tags.
      </block-if>

      <\block-if|<implied-scm|(nnull? thms)>>
        The <markup|<output-string|<implied-scm|tag>>> tag uses themes for
        its rendering. These themes come with their own style parameters that
        can be customized via <menu|Focus|Preferences|Theme parameters>.
      </block-if>

      <\block-if|<implied-scm|(nnull? opts)>>
        <\folded|<strong|Style options>>
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
                  Undocumented style package.
                </block-if-else>
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-if>

      <\block-if|<implied-scm|(nnull? pars)>>
        <\folded|<strong|Style parameters>>
          <\block-for|<implied-scm|par>|<implied-scm|pars>>
            <block-assign|<implied-scm|par-doc>|<\implied-scm>
              (tmdoc-search-parameter par)
            </implied-scm>>

            <\block-if-else|<implied-scm|par-doc>>
              <block-output|<implied-scm|par-doc>>
            <|block-if-else>
              <\explain|<src-var|<output-string|<implied-scm|par>>>>
                A parameter of type <output-string|<implied-scm|(tree-label-type
                (string-\<gtr\>symbol par))>>.
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-if>

      <\block-for|<implied-scm|thm>|<implied-scm|thms>>
        <\folded|<strong|Parameters for the <output-string|<implied-scm|thm>>
        theme>>
          <\block-for|<implied-scm|par>|<implied-scm|(theme-\<gtr\>members
          thm)>>
            <block-assign|<implied-scm|par-doc>|<\implied-scm>
              (tmdoc-search-parameter par)
            </implied-scm>>

            <\block-if-else|<implied-scm|par-doc>>
              <block-output|<implied-scm|par-doc>>
            <|block-if-else>
              <\explain|<src-var|<output-string|<implied-scm|par>>>>
                A parameter of type <output-string|<implied-scm|(tree-label-type
                (string-\<gtr\>symbol par))>>.
              </explain>
            </block-if-else>
          </block-for>
        </folded>
      </block-for>
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-can-move? t)>>
    <\unfolded-documentation|Structured navigation>
      It is sometimes useful to quickly go through all
      <markup|<output-string|<implied-scm|tag>>> tags and its variants,
      \ inside a document. This can be done efficiently using the following
      keyboard shortcuts, menu entries, or icons on the focus toolbar:\ 

      <\description-long>
        <item*|<shortcut|(kbd-select-if-active traverse-first)>,
        <menu|Focus|First similar>, <icon|tm_similar_first.xpm>>Jump to the
        first tag similar to <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-previous)>,
        <menu|Focus|Previous similar>, <icon|tm_similar_previous.xpm>>Jump to
        the previous tag similar to <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-next)>,
        <menu|Focus|Next similar>, <icon|tm_similar_next.xpm>>Jump to the
        next tag similar to <markup|<output-string|<implied-scm|tag>>>.

        <item*|<shortcut|(kbd-select-if-active traverse-last)>,
        <menu|Focus|Last similar>, <icon|tm_similar_last.xpm>>Jump to the
        last tag similar to <markup|<output-string|<implied-scm|tag>>>.
      </description-long>

      For more information and further useful shortcuts, we refer to the
      section on <help-link|structured cursor
      movements|main/editing/man-structured-move>.
    </unfolded-documentation>
  </block-if>

  <\block-if-else|<implied-scm|(and (defined? 'any-table-tag?)
  (any-table-tag? tag))>>
    <\unfolded-documentation|Structured insert and delete>
      The <markup|<output-string|<implied-scm|tag>>> tag is a tabular
      environment. New rows and columns can be inserted using the following
      keyboard shortcuts, menu entries, or icons on the focus toolbar:

      <\description-long>
        <item*|<shortcut|(structured-insert-left)>, <menu|Focus|Insert left>,
        <icon|tm_insert_left.xpm>>Insert a new column on the left-hand side
        of the cursor.

        <item*|<shortcut|(structured-insert-right)>, <menu|Focus|Insert
        right>, <icon|tm_insert_right.xpm>>Insert a new column on the
        right-hand side of the cursor.

        <item*|<shortcut|(structured-insert-up)>, <menu|Focus|Insert above>,
        <icon|tm_insert_up.xpm>>Insert a new row above the cursor.

        <item*|<shortcut|(structured-insert-down)>, <menu|Focus|Insert
        below>, <icon|tm_insert_down.xpm>>Insert a new row below the cursor.
      </description-long>

      Existing rows and columns can be removed as follows:

      <\description-long>
        <item*|<shortcut|(structured-remove-left)>, <menu|Focus|Remove
        leftwards>, <icon|tm_delete_left.xpm>>Remove the column at the
        left-hand side of the cursor.

        <item*|<shortcut|(structured-remove-right)>, <menu|Focus|Remove
        rightwards>, <icon|tm_delete_right.xpm>>Remove the current column and
        move to the next one.

        <item*|<menu|Focus|Remove upwards>, <icon|tm_delete_up.xpm>>Remove
        the row above the cursor.

        <item*|<menu|Focus|Remove downwards>,
        <icon|tm_delete_down.xpm>>Remove the current row and move to the one
        below.
      </description-long>
    </unfolded-documentation>
  <|block-if-else>
    <\block-if-else|<implied-scm|(and (defined? 'field-tags) (tm-in? t
    field-tags))>>
      <\unfolded-documentation|Structured insert and delete>
        The <markup|<output-string|<implied-scm|tag>>> tag is used inside
        interactive sessions. New input fields can be inserted using the the
        following keyboard shortcuts, menu entries, or icons on the focus
        toolbar:

        <\description-long>
          <item*|<shortcut|(structured-insert-up)>, <menu|Focus|Insert field
          above>, <icon|tm_insert_up.xpm>>Insert a new input field above the
          cursor.

          <item*|<shortcut|(structured-insert-down)>, <menu|Focus|Insert
          field below>, <icon|tm_insert_down.xpm>>Insert a new input field
          below the cursor.
        </description-long>

        Existing input or input/output fields can be removed as follows:

        <\description-long>
          <item*|<menu|Focus|Remove field above>,
          <icon|tm_delete_up.xpm>>Remove the field above the cursor.

          <item*|<menu|Focus|Remove field below>,
          <icon|tm_delete_down.xpm>>Remove the current field and move to the
          one below.

          <item*|<menu|Focus|Remove banner>>Remove the start-up banner of the
          session.

          <item*|<menu|Focus|Remove last field>>Remove the last field of the
          session.
        </description-long>
      </unfolded-documentation>
    <|block-if-else>
      <\block-if|<implied-scm|(or (structured-horizontal? t)
      (structured-vertical? t))>>
        <\unfolded-documentation|Structured insert and delete>
          The <markup|<output-string|<implied-scm|tag>>> tag has a variable
          number of arguments. New arguments can be inserted using the
          following keyboard shortcuts, menu entries, or icons on the focus
          toolbar:

          <\block-texmacs-tag|description-long>
            <\block-if|<implied-scm|(structured-horizontal? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-insert-left)>,
              <menu|Focus|Insert left>, <icon|tm_insert_left.xpm>>Insert a
              new argument at the left-hand side of the cursor.

              <inline-texmacs-tag|item*|<shortcut|(structured-insert-right)>,
              <menu|Focus|Insert right>, <icon|tm_insert_right.xpm>>Insert a
              new argument at the right-hand side of the cursor.
            </block-if>

            <\block-if|<implied-scm|(structured-vertical? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-insert-up)>,
              <menu|Focus|Insert above>, <icon|tm_insert_up.xpm>>Insert a new
              argument above the cursor.

              <inline-texmacs-tag|item*|<shortcut|(structured-insert-down)>,
              <menu|Focus|Insert below>, <icon|tm_insert_down.xpm>>Insert a
              new argument below the cursor.
            </block-if>
          </block-texmacs-tag>

          Existing arguments can be removed as follows:

          <\block-texmacs-tag|description-long>
            <\block-if|<implied-scm|(structured-horizontal? t)>>
              <inline-texmacs-tag|item*|<shortcut|(structured-remove-left)>,
              <menu|Focus|Remove leftwards>, <icon|tm_delete_left.xpm>>Remove
              the argument at the left-hand side of the cursor.

              <inline-texmacs-tag|item*|<shortcut|(structured-remove-right)>,
              <menu|Focus|Remove rightwards>,
              <icon|tm_delete_right.xpm>>Remove the current argument and move
              to the next one.
            </block-if>

            <\block-if|<implied-scm|(structured-vertical? t)>>
              <inline-texmacs-tag|item*|<menu|Focus|Remove upwards>,
              <icon|tm_delete_up.xpm>>Remove the argument above the cursor.

              <inline-texmacs-tag|item*|<menu|Focus|Remove downwards>,
              <icon|tm_delete_down.xpm>>Remove the current argument and move
              to the one below.
            </block-if>
          </block-texmacs-tag>
        </unfolded-documentation>
      </block-if>
    </block-if-else>
  </block-if-else>

  <\block-if|<implied-scm|(\<less\> (length (tree-accessible-children t))
  n)>>
    <\unfolded-documentation|Hidden arguments>
      <block-assign|<implied-scm|il>|<implied-scm|(list-filter (.. 0 (length
      nl)) (lambda (i) (not (tree-accessible-child? t i))))>>

      <block-assign|<implied-scm|sl>|<implied-scm|(map (lambda (i) (list-ref
      nl i)) il)>>

      When the <markup|<output-string|<implied-scm|tag>>> tag is
      <help-link|activated|main/text/keyboard/man-dynamic>, the following
      arguments are hidden: <inline-intersperse|,
      |<inline-for|<implied-scm|x>|<implied-scm|sl>|<src-arg|<output-string|<implied-scm|x>>>>>.
      In order to edit the hidden arguments, you should use <menu|Focus|Show
      hidden> or push the <icon|tm_show_hidden.xpm> icon on the focus
      toolbar. Deactivated tags can be reactivated by pressing
      <shortcut|(kbd-return)>.

      Non internal hidden arguments which contain string values can also be
      edited directly in the text fields on the focus toolbar; no need to
      deactivate the <markup|<output-string|<implied-scm|tag>>> tag in this
      case.
    </unfolded-documentation>
  </block-if>

  <\block-if|<implied-scm|(focus-has-geometry? t)>>
    <\unfolded-documentation|Structured geometry>
      Not yet documented.
    </unfolded-documentation>
  </block-if>
</body>

<initial|<\collection>
</collection>>