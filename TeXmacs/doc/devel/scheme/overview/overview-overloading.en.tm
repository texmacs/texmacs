<TeXmacs|1.0.7.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Contextual overloading>

  For large software projects, it is important that different modules can be
  developed as independently as possible one from each other. Furthermore,
  fundamental modules often implement default behaviour which is to be
  overwritten in a more specialized module. In order to facilitate these two
  requirements, <TeXmacs> implements a system of <em|contextual overloading>.

  In order to get the main idea behind this system, consider the
  implementation of a given functionality, like hitting the return key.
  Depending on the context, different actions have to be undertaken: by
  default, we start a new paragraph; inside a table, we start a new row; etc.
  A naive implementation would check all possible cases in a routine
  <scm|kbd-enter> and call the corresponding routine. However, this makes it
  impossible to add a new case in a new module without modifying the module
  which defines <scm|kbd-enter>. By contrast, the system of contextual
  overloading allows the user to <em|conditionally> redefine the routine
  <scm|kbd-enter> several times in distinct modules.

  For instance, assume that we want to define a function <scm|hello> which
  inserts ``Hello'' by default, but ``<math|hello<around|(||)>>'' in mode
  math, while positioning the cursor between the brackets. Using contextual
  overloading, this may be done as follows:

  <\scm-code>
    (tm-define (hello) (insert "Hello"))

    (tm-define (hello) (:mode in-math?) (insert-go-to "hello()" '(6)))
  </scm-code>

  Here we recall that the two definitions may be put inside different
  modules. Notice also that the contextual overloading system considers the
  implementation of <scm|hello> inside math mode to be more <em|particular>
  than the default implementation. In case when several implementations match
  their respective conditions, the most particular implementation will be
  chosen.

  Currently, <TeXmacs> supports two major types of conditions:

  <\description>
    <item*|Conditions on the mode>

    A <em|<TeXmacs> mode> corresponds to a simple predicate without
    arguments, together with a finite number of other, less particular, modes
    which are implied by it. New modes can be defined using the instruction

    <\scm-code>
      (texmacs-modes <em|mode>% <em|cond> <em|submode-1> ... <em|submode-n>)
    </scm-code>

    For instance, we might define a new mode <scm|inside-theorem?> as follows

    <\scm-code>
      (texmacs-modes inside-theorem% (inside? 'theorem) in-text%)
    </scm-code>

    Some standard modes are <scm|always?>, <scm|in-source?>, <scm|in-text?>,
    <scm|in-math?>, <scm|in-prog?>. There is also a special mode
    <scm|prevail?> which is always satisfied, but nevertheless considered as
    more particular than all other modes.

    <\remark>
      Currently, modes necessarily terminate by the <verbatim|?> character.
      However, in the <scm|texmacs-modes> instruction, the <verbatim|?> has
      to be replaced by a <verbatim|%>. This may change in a future version
      of <TeXmacs>.
    </remark>

    <item*|Conditions on the arguments of the function>

    This type of contextual overloading is closest to the more classical
    concept of overloading used by languages like <name|C++>. Although one
    may overload on the types of the arguments, it is also possible to impose
    more general conditions on the arguments. For instance, one may sometimes
    wish to write the following kind of code:

    <\scm-code>
      (tm-define (my-replace what by)

      \ \ <em|default-implementation>)

      \;

      (tm-define (my-replace what by)

      \ \ (:require (== what by))

      \ \ (noop))
    </scm-code>
  </description>

  \;

  In cases of conflict, we notice that the contextual overloading system
  first dispatches on mode and then on the arguments on the function. For
  instance, consider a routine <scm|foo> defined<nbsp>by

  <\scm-code>
    (tm-define (foo t) (:mode in-math?) <em|implementation-1>)

    (tm-define (foo t) (:require (tree-is? t 'frac)) <em|implementation-2>)
  </scm-code>

  Then the first implementation will be used when <scm|foo> is called for a
  fraction in math mode. In cases of conflict when no implementation is
  preferrable <em|a priori>, the last implementation prevails. For instance,
  consider a predicate <scm|(gnu? t)> which implies another predicate
  <scm|(hairy? t)>, and assume that we want to overload a function <scm|(foo
  t)> for both predicates. Then we may use something such as

  <\scm-code>
    (tm-define (foo t) (:require (hairy? t) <em|implementation-1>)

    (tm-define (foo t) (:require (gnu? t)) <em|implementation-2>)
  </scm-code>

  Indeed, the most particular implementation should be declared last. In the
  case when both implementations are in different files, the file with the
  definition when <scm|(gnu? t)> should include the other<nbsp>one.

  Besides <scm|tm-define>, several other added language primitives support
  the contextual overloading mechanism. For instance, <scm|kbd-map> and
  <scm|menu-bind> support overloading on mode. The <scm|tm-define-macro> and
  <scm|tm-property> primitives are analoguous to<nbsp><scm|tm-define>.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>