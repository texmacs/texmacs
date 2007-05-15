<TeXmacs|1.0.5.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Contextual overloading>

  For large software projects, it is important that different modules can be
  developed as independently as possible one from each other. Furthermore,
  fundamental modules often implement default behaviour which is to be
  overwritten in a more specialized module. In order to facilitate these two
  requirements, <TeXmacs> implements an advanced system of <em|contextual
  overloading>.

  In order to get the main idea behind this system, consider the
  implementation of a given functionality, like hitting the return key.
  Depending on the context, different actions have to be undertaken: by
  default, we start a new paragraph; inside a table, we start a new row; etc.
  A naive implementation would check all possible cases in a routine
  <verbatim|kbd-return> and call the corresponding routine. However, this
  makes it impossible to add a new case in a new module without modifying the
  module which defines <verbatim|kbd-return>. By contrast, the system of
  contextual overloading allows the user to <em|conditionally> redefine the
  routine <verbatim|kbd-return> several times in distinct modules.

  For instance, assume that we want to define a function <verbatim|hello>
  which inserts ``Hello'' by default, but ``<with|mode|math|hello()>'' in
  mode math, while positioning the cursor between the brackets. Using
  contextual overloading, this may be done as follows:

  <\scheme-fragment>
    (tm-define (hello) (insert "Hello"))

    (tm-define (hello) (:mode in-math?) (insert-go-to "hello()" '(6)))
  </scheme-fragment>

  Here we recall that the two definitions may be put inside different
  modules. Notice also that the contextual overloading system considers the
  implementation of <verbatim|hello> inside math mode to be more
  <em|particular> than the default implementation. In case when several
  implementations match their respective conditions, the most particular
  implementation will be chosen.

  Currently, <TeXmacs> supports three major types of conditions:

  <\description>
    <item*|Conditions on the mode>

    A <em|<TeXmacs> mode> corresponds to a simple predicate without
    arguments, together with a finite number of other, less particular, modes
    which are implied by it. New modes can be defined using the instruction

    <\scheme-fragment>
      (texmacs-modes <em|mode>% <em|cond> <em|submode-1> ... <em|submode-n>)
    </scheme-fragment>

    For instance, we might define a new mode <verbatim|inside-theorem?> as
    follows

    <\scheme-fragment>
      (texmacs-modes inside-theorem% (inside? 'theorem) in-text%)
    </scheme-fragment>

    Some standard modes are <verbatim|always?>, <verbatim|in-source?>,
    <verbatim|in-text?>, <verbatim|in-math?>, <verbatim|in-prog?>. There is
    also a special mode <verbatim|prevail?> which is always satisfied, but
    nevertheless considered as more particular than all other modes.

    <\remark>
      Currently, modes necessarily terminate by the <verbatim|?> character.
      However, in the <verbatim|texmacs-modes> instruction, the <verbatim|?>
      has to be replaced by a <verbatim|%>. This may change in a future
      version of <TeXmacs>.
    </remark>

    <item*|Conditions on the cursor context>

    Certain actions only make sense when the cursor is inside a special tag,
    or more generally inside some structure which matches a predicate. For
    instance, the action <verbatim|structured-insert-right> inserts a new
    column at the right-hand side of the cursor when you are inside a
    <markup|table> tag and a new branch when you are inside a <markup|tree>.
    In the case, when you are both inside a <markup|table> and a
    <markup|tree>, the innermost match is considered to be the most
    particular one.

    For instance, consider the implementation of a routine
    <verbatim|structured-swap> for fractions, which permutes the numerator
    and denominator. This may be done as follows:

    <\scheme-fragment>
      (tm-define (structured-swap)

      \ \ (:inside frac)

      \ \ (with-innermost t 'frac

      \ \ \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0)))))
    </scheme-fragment>

    <item*|Conditions on the arguments of the function>

    This type of contextual overloading is closest to the more classical
    concept of overloading used by languages like <name|C++>. Although one
    may overload on the types of the arguments, it is also possible to impose
    more general conditions on the arguments. For instance, one may sometimes
    wish to write the following kind of code:

    <\scheme-fragment>
      (tm-define (my-replace what by)

      \ \ <em|default-implementation>)

      \;

      (tm-define (my-replace what by)

      \ \ (:require (== what by))

      \ \ (noop))
    </scheme-fragment>
  </description>

  \;

  In cases of conflict, we notice that the contextual overloading system
  first dispatches on mode, next on the cursor context and finally on the
  arguments on the function. For instance, consider a routine <verbatim|foo>
  defined<nbsp>by

  <\scheme-fragment>
    (tm-define (foo) (:mode in-math?) <em|implementation-1>)

    (tm-define (foo) (:inside 'frac) <em|implementation-2>)
  </scheme-fragment>

  Then the first implementation will be used when <verbatim|foo> is called
  from within a fraction in math mode.

  Besides <verbatim|tm-define>, several other added language primitives
  support the contextual overloading mechanism. For instance,
  <verbatim|kbd-map>, <verbatim|define-menu> and <verbatim|extend-menu>
  support overloading on mode and cursor context. The
  <verbatim|tm-define-macro> and <verbatim|tm-property> primitives are
  analoguous to<nbsp><verbatim|tm-<no-break>define>.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>