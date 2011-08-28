<TeXmacs|1.0.7.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Meta information and logical programming>

  Small software projects usually consist of a collection of routines and
  data. In a large software project, where a typical contributor has no
  complete overview of the program, it is a good practive to associate
  additional <em|meta-information> to the individual routines and data. This
  meta-information typically serves documentation purposes, but becomes even
  more interesting if it can be used in an automized fashion to implement
  more general additional functionality.

  The <scm|tm-define> macro supports several options for associating
  meta-information to <scheme> functions and symbols. For instance, the
  <scm|:synopsis>, <scm|:argument> and <scm|:returns> options allow you to
  associate short documentation strings to the function, its arguments and
  its return value:

  <\scm-fragment>
    (tm-define (square x)

    \ \ (:synopsis "Compute the square of @x")

    \ \ (:argument x "A number")

    \ \ (:returns "The square of @x")

    \ \ (* x x))
  </scm-fragment>

  This information is exploited by <TeXmacs> in several ways. For instance,
  the synopsis of the function can be retrieved by executing <scm|(help
  square)>. More interestingly, assuming that we defined <scm|square> as
  above, typing <shortcut|(interactive exec-interactive-command)> followed by <scm|square> and <shortcut|(kbd-return)>
  allows you to execute <scm|square> in an interactive way: you will be
  prompted for ``A number'' on the footer. Moreover, after typing <shortcut|(interactive exec-interactive-command)>,
  you will be able to use ``tab-completion'' in order to enter <scm|square>:
  typing <key|s q u tab> will usually complete into<nbsp><scm|square>.

  In a similar vein, the <scm|:interactive> and <scm|:check-mark> options
  allow you to specify that a given routine requires interactive user input
  or when it should give rise to a check-mark when used in a menu. For
  instance, the statement

  <\scm-fragment>
    (tm-property (choose-file fun text type)

    \ \ (:interactive #t))
  </scm-fragment>

  in the source code of <TeXmacs> states that <scm|choose-file> is an
  interactive command. As a consquence, the <menu|File|Load> entry, which is
  defined by

  <\scm-fragment>
    ("Load" (choose-file load-buffer "Load file" ""))
  </scm-fragment>

  will be followed by dots <scm|...> in the <menu|File> menu. The interesting
  point here is that, although the command <scm|choose-file> may be reused
  several times in different menu entries, we only have to specify once that
  it is an interactive command. Similarly, consider the definition

  <\scm-fragment>
    (tm-define (toggle-session-math-input)

    \ \ (:check-mark "v" session-math-input?)

    \ \ (session-use-math-input (not (session-math-input?))))
  </scm-fragment>

  Given a menu item with <scm|(toggle-session-math-input)> as its associated
  action, this definition specifies in particular that a check-mark should be
  displayed before the menu item whenever the <scm|session-math-input?>
  predicate holds.

  Another frequently used option is <scm|:secure>, which specifies that a
  given routine is secure. For instance, the default implementation of the
  <markup|fold> tag allows the user to click on the
  ``<with|mode|math|<op|\<circ\>>>'' before the folded text so as to unfold
  the tag. When doing this, the scheme script <scm|(mouse-unfold)> is
  launched. However, for this to work, the <scm|mouse-unfold> function needs
  to be secure:

  <\scm-fragment>
    (tm-define (mouse-unfold)

    \ \ (:secure #t)

    \ \ (with-action t

    \ \ \ \ (tree-go-to t :start)

    \ \ \ \ (fold)))
  </scm-fragment>

  The <scm|:secure> option is also needed in combination with <hlink|other
  tags|overview-start.en.tm#markup-scripts> which depend on <scheme> scripts,
  like <markup|extern>.

  In the future, the number of options for entering meta-information is
  likely to increase. <TeXmacs> also supports an additional mechanism for the
  automatic deduction of new meta-properties from existing meta-properties.
  This mechanism is based on a less general, but more efficient form of
  <em|logical programming>. However, since it is not fully stable yet, it
  will be documented only later.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>