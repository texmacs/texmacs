<TeXmacs|1.0.6.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Mutator tags>

  A mutator tag is of the form <explain-macro|mutator|body|cmd>, where
  <src-arg|body> is the visible body of the tag and <src-arg|cmd> a secure
  <value|scheme> script which is called periodically and which is allowed to
  modify the <src-arg|body> of the mutator (or even any other part of the
  document). During the execution of <src-arg|cmd> the function
  <verbatim|mutator-path> yields the position of the <src-arg|body> in the
  document tree.

  Mutator tags are a particularly interesting feature of <TeXmacs> for
  producing highly interactive documents. For instance, inside computer
  algebra sessions, the output is retrieved inside a mutator tag (which
  automatically removes itself when the output is complete). In the future,
  mutators might be used for live conferencing or in interfaces with proof
  systems.

  <\remark>
    Mutator tags only work properly when the explicitly occur in the
    document; they will not work if they merely occur in the body of a macro,
    even if the body of the mutator remains accessible.

    Indeed, the current implementation of <TeXmacs> searches for mutator tags
    in all documents after a small period of inactivity. Documents which are
    known not to contain mutator tags are ignored during this research. Of
    course, this implementation is both quite efficient and incompatible with
    the macro system. So there is room for future improvements.
  </remark>

  <\remark>
    For efficiency reasons, it is recommended that mutator tags mainly modify
    there own bodies and not other parts of the document, except at really
    exceptional occasions.
  </remark>

  <paragraph|The <verbatim|mutator> plug-in>

  A very simple example with two types of mutators is provided in the
  <verbatim|mutator> plug-in. It provides the user with two keyboard
  shortcuts <key|C-F11> and <key|C-F12>, which respectively insert the
  current time and some blinking text. The plug-in consists of the single
  file

  <\verbatim>
    \ \ \ \ <example-plugin-link|mutator/progs/init-mutator.scm>
  </verbatim>

  The <key|C-F11> key simply inserts the continuously updated time into the
  main text:

  <\cpp-fragment>
    (kbd-map ("C-F11" (insert '(mutator "" "(mutate-date)"))))
  </cpp-fragment>

  The secure <value|scheme> routine <verbatim|mutate-date> is defined as
  follows:

  <\scheme-fragment>
    (tm-define (mutate-date)

    \ \ (:secure #t)

    \ \ (let* ((p (the-mutator-path))

    \ \ \ \ \ \ \ \ \ (date (var-eval-system "date +\\"%H:%M:%S\\"")))

    \ \ \ \ (tm-assign-diff p date)))
  </scheme-fragment>

  The <scm|tm-assign-diff> command is convenient, because it only modifies
  the document if a real change occurred.

  The insertion of blinking content is slightly more complex, since it also
  takes into account the current content of the mutator tag. The <key|C-F12>
  key inserts a blinking text into the main text and puts the cursor after
  the text in the body of the mutator:

  <\cpp-fragment>
    (kbd-map ("C-F12" (insert-go-to '(mutator "text" "(mutate-blink)")
    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '(0
    4))))
  </cpp-fragment>

  The secure <value|scheme> routine <verbatim|mutate-blink> is defined as
  follows:

  <\scheme-fragment>
    (tm-define (mutate-blink)

    \ \ (:secure #t)

    \ \ (let* ((mod (lambda (x y) (* y (- (/ x y) (floor (/ x y))))))

    \ \ \ \ \ \ \ \ \ (p (the-mutator-path))

    \ \ \ \ \ \ \ \ \ (t (tm-subtree p))

    \ \ \ \ \ \ \ \ \ (s (string-\<gtr\>number (var-eval-system "date
    +\\"%S\\"")))

    \ \ \ \ \ \ \ \ \ (e (mod s 4)))

    \ \ \ \ (if (and (\<less\>= e 1) (not (match? t '(strong :%1))))

    \ \ \ \ \ \ \ \ (tm-ins-unary p 'strong))

    \ \ \ \ (if (and (\<gtr\>= e 2) (match? t '(strong :%1)))

    \ \ \ \ \ \ \ \ (tm-rem-unary p))))
  </scheme-fragment>

  <\remark>
    Notice that the above examples are only meant to illustrate the use of
    mutators. Ideally speaking, dates and blinking content should not make
    use of mutators, since mutators continuously <em|modify> the document
    (think about undoing changes, for instance). In the future, we plan to
    add primitives like animations and movies to <TeXmacs>, for which the
    content remains fixed, but whose presentation changes over time.
  </remark>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>