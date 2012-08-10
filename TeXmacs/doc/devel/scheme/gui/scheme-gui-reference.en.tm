<TeXmacs|1.0.7.16>

<style|tmdoc>

<\body>
  <tmdoc-title|Widgets reference guide>

  This should be a comprehensive list of all the widgets available to the
  user, following this schema:

  <\explain>
    <scm|some-symbol><explain-synopsis|Some synopsis>
  <|explain>
    Some explanation.
  </explain>

  An excerpt from <verbatim|progs/kernel/gui/menu-define.scm>, as of SVN
  revision 5238:

  <\scm>
    (define-table gui-make-table

    \ \ (eval ,gui-make-eval)

    \ \ (dynamic ,gui-make-dynamic)

    \ \ (former ,gui-make-former)

    \ \ (link ,gui-make-link)

    \ \ (let ,gui-make-let)

    \ \ (let* ,gui-make-let)

    \ \ (with ,gui-make-with)

    \ \ (receive ,gui-make-with)

    \ \ (for ,gui-make-for)

    \ \ (cond ,gui-make-cond)

    \ \ (refresh ,gui-make-refresh)

    \ \ (group ,gui-make-group)

    \ \ (text ,gui-make-text)

    \ \ (glue ,gui-make-glue)

    \ \ (color ,gui-make-color)

    \ \ (texmacs-output ,gui-make-texmacs-output)

    \ \ (texmacs-input ,gui-make-texmacs-input)

    \ \ (input ,gui-make-input)

    \ \ (enum ,gui-make-enum)

    \ \ (choice ,gui-make-choice)

    \ \ (choices ,gui-make-choices)

    \ \ (toggle ,gui-make-toggle)

    \ \ (icon ,gui-make-icon)

    \ \ (concat ,gui-make-concat)

    \ \ (verbatim ,gui-make-verbatim)

    \ \ (check ,gui-make-check)

    \ \ (balloon ,gui-make-balloon)

    \ \ (-\<gtr\> ,gui-make-submenu)

    \ \ (=\<gtr\> ,gui-make-top-submenu)

    \ \ (horizontal ,gui-make-horizontal)

    \ \ (vertical ,gui-make-vertical)

    \ \ (hlist ,gui-make-hlist)

    \ \ (vlist ,gui-make-vlist)

    \ \ (aligned ,gui-make-aligned)

    \ \ (item ,gui-make-item)

    \ \ (meti ,gui-make-meti)

    \ \ (tabs ,gui-make-tabs)

    \ \ (tab ,gui-make-tab)

    \ \ (inert ,gui-make-inert)

    \ \ (explicit-buttons ,gui-make-explicit-buttons)

    \ \ (bold ,gui-make-bold)

    \ \ (tile ,gui-make-tile)

    \ \ (scrollable ,gui-make-scrollable)

    \ \ (resize ,gui-make-resize)

    \ \ (hsplit ,gui-make-hsplit)

    \ \ (vsplit ,gui-make-vsplit)

    \ \ (minibar ,gui-make-minibar)

    \ \ (extend ,gui-make-extend)

    \ \ (padded ,gui-make-padded)

    \ \ (centered ,gui-make-centered)

    \ \ (bottom-buttons ,gui-make-bottom-buttons)

    \ \ (assuming ,gui-make-assuming)

    \ \ (if ,gui-make-if)

    \ \ (when ,gui-make-when)

    \ \ (mini ,gui-make-mini)

    \ \ (symbol ,gui-make-symbol)

    \ \ (promise ,gui-make-promise)

    \ \ (ink ,gui-make-ink)

    \ \ (form ,gui-make-form)

    \ \ (form-input ,gui-make-form-input)

    \ \ (form-enum ,gui-make-form-enum)

    \ \ (form-choice ,gui-make-form-choice)

    \ \ (form-choices ,gui-make-form-choices))

    \;

    (tm-define (gui-make x)

    \ \ ;;(display* "x= " x "\\n")

    \ \ (cond ((symbol? x)

    \ \ \ \ \ \ \ \ \ (cond ((== x '---) '$---)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x '===) (gui-make '(glue #f #f 0 5)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x '======) (gui-make '(glue #f #f 0
    15)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x '/) '$/)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x '//) (gui-make '(glue #f #f 5 0)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x '///) (gui-make '(glue #f #f 15 0)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x '\<gtr\>\<gtr\>) (gui-make '(glue #t
    #f 5 0)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x '\<gtr\>\<gtr\>\<gtr\>) (gui-make
    '(glue #t #f 15 0)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((== x (string-\<gtr\>symbol "\|")) '$/)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (else

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (texmacs-error "gui-make" "invalid menu
    item ~S" x))))

    \ \ \ \ \ \ \ \ ((string? x) x)

    \ \ \ \ \ \ \ \ ((and (pair? x) (ahash-ref gui-make-table (car x)))

    \ \ \ \ \ \ \ \ \ (apply (car (ahash-ref gui-make-table (car x))) (list
    x)))

    \ \ \ \ \ \ \ \ ((and (pair? x) (or (string? (car x)) (pair? (car x))))

    \ \ \ \ \ \ \ \ \ \0($\<gtr\> ,(gui-make (car x)) ,@(cdr x)))

    \ \ \ \ \ \ \ \ (else

    \ \ \ \ \ \ \ \ \ \ (texmacs-error "gui-make" "invalid menu item ~S"
    x))))
  </scm>

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>