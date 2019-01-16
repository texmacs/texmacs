<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Function definition and contextual overloading>

  Conventional programming languages often provide some means to overload
  certain functions depending on the types of the arguments. <TeXmacs>
  provides additional context-based overloading mechanisms, which require the
  use of the <scm|tm-define> construct for function definitions (and
  <scm|tm-define-macro> for macro definitions). Definition with
  <scm|tm-define> also allows the specification of properties of the
  function/macro: arguments, synopsis, etc.

  Furthermore, one may use <scm|tm-property> for associating additional
  properties, such as interactivity or default values for the arguments, of a
  function <em|which is already defined>, specifically functions exported
  from <c++> code through the glue.

  <\explain>
    <scm|(tm-define <scm-arg|head> <scm-args|options>
    <scm-args|body>)><explain-synopsis|<TeXmacs> function definition>

    <scm|(tm-define-macro <scm-arg|head> <scm-args|options>
    <scm-args|body>)><explain-synopsis|<TeXmacs> macro definition>
  <|explain>
    <TeXmacs> function and macro declarations are similar to usual
    declarations based on <scm|define> and <scm|define-macro>, except for the
    additional list of <scm-arg|options> and the fact that all functions and
    macros defined using <scm|tm-define> and <scm|tm-define-macro> are
    public. Each option is of the form <scm|(:<scm-arg|kind>
    <scm-args|arguments>)> and the <scm-arg|body> starts at the first element
    of the list following <scm-arg|head> which is not of this form. Available
    options are <scm|:type>, <scm|:synopsis>, <scm|:returns>, <scm|:note>,
    <scm|:argument>, <scm|:default>, <scm|:proposals>, <scm|:secure>,
    <scm|:check-mark>, <scm|:interactive> and <scm|:balloon>.
  </explain>

  <\explain>
    <scm|(tm-property <scm-arg|head> <scm-args|options>)><explain-synopsis|<TeXmacs>
    properties definition>
  <|explain>
    <scm|tm-property> allows the declaration of <TeXmacs> properties for
    functions which have already been defined, specifically for functions
    exported through the glue. Available options are <scm|:type>,
    <scm|:synopsis>, <scm|:returns>, <scm|:note>, <scm|:argument>,
    <scm|:default>, <scm|:proposals>, <scm|:secure>, <scm|:check-mark>,
    <scm|:interactive> and <scm|:balloon>.
  </explain>

  <paragraph*|Contextual overloading>

  We will first describe the most important <scm|:require> option for
  contextual overloading, which was already discussed
  <hlink|before|../overview/overview-overloading.en.tm>.

  <\explain>
    <scm|(:require <scm-arg|cond>)><explain-synopsis|argument based
    overloading>
  <|explain>
    This option specifies that one necessary condition for the declaration to
    be valid is that the condition <scm-arg|cond> is met. This condition may
    involve the arguments of the function.

    As an example, let us consider the following definitions:

    <\scm-code>
      (tm-define (special t)

      \ \ (and-with p (tree-outer t)

      \ \ \ \ (special p)))

      \;

      (tm-define (special)

      \ \ (:require (tree-is? t 'frac))

      \ \ (tree-set! t `(frac ,(tree-ref t 1) ,(tree-ref t 0))))

      \;

      (tm-define (special)

      \ \ (:require (tree-is? t 'rsub))

      \ \ (tree-set! t `(rsup ,(tree-ref t 0))))
    </scm-code>

    The default implementation of <scm|special> is to apply <scm|special> to
    the parent <scm|p> of <scm|t> as long as <scm|t> is not the entire
    document itself. The two overloaded cases apply when <scm|t> is either a
    fraction or a right subscript.

    Assuming that your cursor is inside a fraction inside a subscript,
    calling <scm|special> will swap the numerator and the denominator. On the
    other hand, if your cursor is inside a subscript inside a fraction, then
    calling <scm|special> will change the subscript into a superscript.

    When the conditions of several (re)declarations are met, then the last
    redeclaration will be used. Inside a redeclaration, one may also use the
    <scm|former> keyword in order to explicitly access the former value of
    the redefined symbol.
  </explain>

  <\explain>
    <scm|(:mode <scm-arg|mode>)><explain-synopsis|mode-based overloading>
  <|explain>
    This option is equivalent to <scm|(:require (<scm-arg|mode>))> and
    specifies that the definition is only valid when we are in a given
    <scm-arg|mode>. New modes are defined using <scm|texmacs-modes> and modes
    can inherit from other modes.
  </explain>

  <\explain>
    <scm|(texmacs-modes . <scm-arg|modedefs>)> <explain-synopsis|define new
    texmacs modes>
  <|explain>
    Use this macro to define new modes that you can use for contextual
    overloading, for instance in <scm|kbd-map>. Modes may be made dependent
    on other modes. This macro takes a variable number of definitions as
    arguments, each of the form <scm|(mode-name conditions . dependencies)>.
    End your <scm|mode-name> and any dependencies with one <scm|%>, like
    this:

    <\scm-code>
      (texmacs-modes

      \ \ (in-verbatim% (inside? 'verbatim) in-text%)

      \ \ (in-tt% (inside? 'tt)))
    </scm-code>

    When creating new modes remember to place first the faster checks
    (against booleans, etc.) for speed.
  </explain>

  <paragraph*|Other options for function and macro declarations>

  Besides the contextual overloading options, the <scm|tm-define> and
  <scm|tm-define-macro> primitives admit several other options for attaching
  additional information to the function or macro. We will now describe these
  options and explain how the additional information attached to functions
  can be exploited.

  <\warning>
    A current limitation of the implementation is that functions overloaded
    using <scm|:require> and <scm|:mode> cannot have different options. This
    means in particular that you cannot specify different values for
    <scm|:synopsis> depending on the context.
  </warning>

  <\explain>
    <scm|(:synopsis <scm-arg|short-help>)><explain-synopsis|short
    description>
  <|explain>
    This option gives a short description of the function or macro, in the
    form of a string <scm-arg|short-help>. As a convention, <scheme>
    expressions may be encoded inside this string by using the
    <verbatim|@>-prefix. For instance:

    <\scm-code>
      (tm-define (list-square l)

      \ \ (:synopsis "Appends the list @l to itself")

      \ \ (append l l))
    </scm-code>

    The synopsis of a function is used for instance in order to provide a
    short help string for the function. In the future, we might also use it
    for help balloons describing menu items.
  </explain>

  <\explain>
    <scm|(:argument <scm-arg|var> <scm-arg|description>)>

    <scm|(:argument <scm-arg|var> <scm-arg|type>
    <scm-arg|description>)><explain-synopsis|argument description>
  <|explain>
    This option gives a short <scm-arg|description> of one of the arguments
    <scm-arg|var> to the function or macro. Such a description is used for
    instance for the prompts, when calling the function interactively. For
    these uses, the second format allows for the specification of a
    <scm-arg|type> which changes how the widgets/prompts work. Some allowed
    values are <scm|"string">, the default, and <scm|"file"> and
    <scm|"directory">. If any of the last two is specified, tab completion in
    the interactive prompt will traverse the file system.
  </explain>

  <\explain>
    <scm|(:returns <scm-arg|description>)><explain-synopsis|return value
    description>
  <|explain>
    This option gives a short <scm-arg|description> of the return value of
    the function or macro.
  </explain>

  <\explain>
    <scm|(:type (-\<gtr\> <scm-arg|from> <scm-arg|to>))><explain-synopsis|type
    conversion description>
  <|explain>
    This option specifies that a function or macro performs a conversion from
    the data type <scm-arg|from> to the data type <scm-arg|to>.
  </explain>

  <tmdoc-copyright|2007--2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>