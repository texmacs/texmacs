<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

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
  inserts \PHello\Q by default, but \P<math|hello<around|(||)>>\Q in mode
  math, while positioning the cursor between the brackets. Using contextual
  overloading, this may be done as follows:

  <\scm-code>
    (tm-define (hello) (insert "Hello"))

    (tm-define (hello) (:require (in-math?)) (insert-go-to "hello()" '(6)))
  </scm-code>

  \;

  The order in which routines are overloaded is important. <TeXmacs> first
  tries the latest (re)definition. If this definition does not satisfy the
  requirements (<scm|(in-math?)>, in our case), then it tries the before last
  (re)definition, and so on until an implementation is found which matches
  the requirements. For example, if we invert the two declarations in the
  above example, then the general unconditional definition of <scm|hello>
  will always prevail. If the two declarations are made inside different
  modules, then it is up to the user to ensure that the modules are loaded in
  an appropriate order.

  Inside a redefinition, it is also possible to access the former definition
  using the keyword <scm|former>. In particular, the code

  <\scm-code>
    (tm-define (hello)

    \ \ (if (in-math?) (insert-go-to "hello()" '(6)) (former)))
  </scm-code>

  is equivalent to the second declaration in our example.

  Contextual overloading generalizes more classical overloading on the types
  of the arguments, such as <name|C++> style polymorphism. Although one may
  overload on the types of the arguments, it is also possible to impose more
  general conditions on the arguments. For instance, one may sometimes wish
  to write the following kind of code:

  <\scm-code>
    (tm-define (my-replace what by)

    \ \ <em|default-implementation>)

    \;

    (tm-define (my-replace what by)

    \ \ (:require (== what by))

    \ \ (noop))
  </scm-code>

  Besides <scm|tm-define>, several other added language primitives support
  the contextual overloading mechanism. For instance, <scm|kbd-map> and
  <scm|menu-bind> support overloading on mode. The <scm|tm-define-macro> and
  <scm|tm-property> primitives are analogous to <scm|tm-define>.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>