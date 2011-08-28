<TeXmacs|1.0.6.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematical and customized input>

  The <TeXmacs> meta-format allows application output to contain structured
  text like mathematical formulas. In a similar way, you may use general
  <TeXmacs> content as the input for your application. By default, only the
  text part of such content is kept and sent to the application as a string.
  Moreover, all characters in the range 0--31 are ignored, except for
  <verbatim|'\\t'> and <verbatim|'\\n'> which are transformed into spaces.
  There are two methods to customize the way input is sent to your
  application. First of all, the configuration option

  <\scheme-fragment>
    (:serializer ,<em|routine>)
  </scheme-fragment>

  specifies a scheme function for converting <TeXmacs> trees to string input
  for your application, thereby overriding the default method. This method
  allows you for instance to treat multi-line input in a particular way or
  the perform transformations on the <TeXmacs> tree.

  The <scm|:serialize> option is a very powerful, but also a very abstract
  way to customize input: it forces you to write a complete input
  transformation function. In many circumstances, the user really wants to
  rewrite two dimensional mathematical input to a more standard form, like
  rewriting <no-break><with|mode|math|<frac|a|b>> to <verbatim|((a)/(b))>.
  Therefore, a second way for customizing the input is to use the command

  <\scheme-fragment>
    \ (plugin-input-converters <em|myplugin>

    \ \ \ <em|rules>)
  </scheme-fragment>

  This command specifies input conversion rules for <verbatim|<em|myplugin>>
  for ``mathematical input'' and reasonable defaults are provided by
  <TeXmacs>. Each rule is of one of the following two forms:

  <\description>
    <item*|Leaf transformation rules>

    Given two strings <verbatim|<em|symbol>> and <verbatim|<em|conversion>>,
    the rule

    <\scheme-fragment>
      (<verbatim|<em|symbol>> <verbatim|<em|conversion>>)
    </scheme-fragment>

    specifies that the <TeXmacs> symbol <verbatim|<em|symbol>> should be
    converted to <verbatim|<em|conversion>>.

    <item*|Tag transformation rules>

    Given a symbol <verbatim|<em|tag>> and a <value|scheme> function
    <verbatim|<em|routine>>, the rule

    <\scheme-fragment>
      (<em|tag> <em|routine>)
    </scheme-fragment>

    specifies that <verbatim|<em|routine>> will be used as the conversion
    routine for <verbatim|<em|tag>>. This routine should just write a string
    to the standard output. The <value|scheme> function
    <scheme-code|plugin-input> may be used for the recursive transformation
    of the arguments of the tag.
  </description>

  <paragraph*|The <verbatim|input> plug-in>

  The <verbatim|input> plug-in demonstrates the use of customized
  mathematical input. It consists of the files

  <\verbatim>
    \ \ \ \ <example-plugin-link|input/Makefile>

    \ \ \ \ <example-plugin-link|input/packages/session/input.ts>

    \ \ \ \ <example-plugin-link|input/progs/init-input.scm>

    \ \ \ \ <example-plugin-link|input/progs/input-input.scm>

    \ \ \ \ <example-plugin-link|input/src/input.cpp>
  </verbatim>

  The <value|scheme> configuration code in <verbatim|init-input.scm> is given
  by

  <\scheme-fragment>
    (plugin-configure input

    \ \ (:require (url-exists-in-path? "input.bin"))

    \ \ (:initialize (input-initialize))

    \ \ (:launch "input.bin")

    \ \ (:session "Input"))
  </scheme-fragment>

  Here <verbatim|input-initialize> is an initialization routine which adds
  the new input conversion rules in a lazy way:

  <\scheme-fragment>
    (define (input-initialize)

    \ \ (import-from (texmacs plugin plugin-convert))

    \ \ (lazy-input-converter (input-input) input))
  </scheme-fragment>

  In other words, the module <verbatim|input-input.scm> will only be loaded
  when we explicitly request to make a conversion. The conversion rules in
  <verbatim|input-input.scm> are given by

  <\scheme-fragment>
    (plugin-input-converters input

    \ \ (frac input-input-frac)

    \ \ (special input-input-special)

    \ \ ("\<less\>vee\<gtr\>" "\|\|")

    \ \ ("\<less\>wedge\<gtr\>" "&&"))
  </scheme-fragment>

  This will cause <with|mode|math|\<vee\>> and <with|mode|math|\<wedge\>> to
  be rewritten as <verbatim|\|\|> and <verbatim|&&> respectively. Fractions
  <with|mode|math|<frac|a|b>> are rewritten as <verbatim|((a):(b))> using the
  routine

  <\scheme-fragment>
    (define (input-input-frac t)

    \ \ (display "((")

    \ \ (plugin-input (car t))

    \ \ (display "):(")

    \ \ (plugin-input (cadr t))

    \ \ (display "))"))
  </scheme-fragment>

  In the additional style file <verbatim|input.ts> we also defined some
  additional markup <markup|special>:

  <\tm-fragment>
    <inactive*|<assign|special|<macro|body|<block|<tformat|<cwith|1|1|1|1|cell-background|pastel
    green>|<table|<row|<cell|<arg|body>>>>>>>>>
  </tm-fragment>

  This tag is rewritten using the special conversion rule

  <\scheme-fragment>
    (define (input-input-special t)

    \ \ (display "[[[SPECIAL:")

    \ \ (plugin-input (car t))

    \ \ (display "]]]"))
  </scheme-fragment>

  As to the <value|cpp> code in <verbatim|input.cpp>, the startup banner
  automatically puts the shell session in mathematical input mode:

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "command:(session-use-math-input #t)"

    \ \ \ \ \ \<less\>\<less\> DATA_END;

    cout \<less\>\<less\> "Convert mathematical input into plain text";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  In the main loop, we content ourselves the reproduce the input as output:

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> buffer;

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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