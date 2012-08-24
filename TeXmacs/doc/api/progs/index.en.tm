<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|API documentation>

  The <scheme> API of <TeXmacs> is built from two different sources. On the
  one hand, the module system and on the other the <c++> functions exported
  using the glue. You can read more on this topic in ``<hlink|General
  architecture of the <scheme> API|../devel/scheme/overview/overview-architecture.en.tm>''.

  <subsection|The module system>

  <TeXmacs> modules are currently classified in the following families:

  <traverse-modules-doc>

  <\traverse>
    <branch|somemodule|someplace>

    Some text here

    <branch|someothermodule|someotherplace>

    More text here
  </traverse>

  <\session|scheme|default>
    <\input|Scheme] >
      (define ts (select (buffer-tree) '(traverse :*)))
    </input>

    <\unfolded-io|Scheme] >
      (tree-\<gtr\>stree (car ts))
    <|unfolded-io>
      (traverse (document (branch "somemodule" "someplace") "Some text here"
      (branch "someothermodule" "someotherplace") "More text here"))
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>

    <\input|Scheme] >
      (define (list-modules)

      \ \ (list-uniq\ 

      \ \ \ (apply append (map cdr (ahash-table-\<gtr\>list
      tm-defined-module)))))
    </input>

    <\input|Scheme] >
      (define (list-submodules root)

      \ \ (if (symbol? root)\ 

      \ \ \ \ \ \ (list-filter (list-modules) (lambda (x) (== (car x)
      root)))))

      \ \ \ 

      \ \ \ \ \ \ ; TODO: filter submodules

      ;(list-filter ;WRONG!(list-submodules (cdr root))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ;(lambda (x) (==
      (cr x) root))))))
    </input>

    <\input|Scheme] >
      \;
    </input>

    <\unfolded-io|Scheme] >
      (list-submodules 'graphics)
    <|unfolded-io>
      ((graphics graphics-single) (graphics graphics-kbd) (graphics
      graphics-menu) (graphics graphics-object) (graphics graphics-group)
      (graphics graphics-env) (graphics graphics-main) (graphics
      graphics-edit) (graphics graphics-drd) (graphics graphics-utils))
    </unfolded-io>

    <\input|Scheme] >
      (define (module-\<gtr\>string module)

      \ \ (if (list? module)

      \ \ \ \ \ \ (string-join (map symbol-\<gtr\>string module) ".")

      \ \ \ \ \ \ (symbol-\<gtr\>string module)))
    </input>

    <\input|Scheme] >
      (define (module-doc-path module)

      \ \ (string-concatenate

      \ \ \ \ \ (list "api/"

      \ \ \ \ \ \ \ \ \ \ \ (string-join (map symbol-\<gtr\>string module)
      "/")

      \ \ \ \ \ \ \ \ \ \ \ ".en.tm")))
    </input>

    <\input|Scheme] >
      (define ($module-doc-link module)

      \ \ "Builds a link to the documentation for @module"

      \ \ (let ((pm (module-doc-path module)))

      \ \ \ \ ($link (string-append (url-concretize "$TEXMACS_PATH/doc/") pm)

      \ \ \ \ \ \ (module-\<gtr\>string module))))
    </input>

    <\input|Scheme] >
      (define ($doc-module-branches root)

      \ \ `(document

      \ \ \ \ ,(for (m (list-submodules root))

      \ \ \ \ \ \ `(branch ,(module-\<gtr\>string m) ,($module-doc-link
      m)))))
    </input>

    <\input|Scheme] >
      (define br ($doc-module-branches 'graphics))
    </input>

    <\unfolded-io|Scheme] >
      br
    <|unfolded-io>
      (document #\<less\>unspecified\<gtr\>)
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <subsection|The glue>

  <traverse-glue-doc>

  <tmdoc-copyright||>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>