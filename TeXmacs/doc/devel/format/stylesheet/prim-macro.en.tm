<TeXmacs|1.0.7.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Macro primitives>

  Macros can be used to define new tags and to build procedural abstractions
  in style files.

  Older versions of <TeXmacs> used to make a distinction between macros (all
  children accessible) and functions (no accessible child). In modern
  <TeXmacs> there are only macros: the accessibility of children is
  determined heuristically and can be controlled with
  <markup|drd-props>.<htab|5mm>

  <\explain>
    <explain-macro|macro|var-1|<with|mode|math|\<cdots\>>|var-n|body><explain-synopsis|macro
    of fixed arity>
  <|explain>
    This primitive returns a macro (the <TeXmacs> analogue of a
    <with|mode|math|\<lambda\>>-expression) with <with|mode|math|n>
    arguments, named after the literal strings <src-arg|var-1> until
    <src-arg|var-n>.

    New tags are defined by storing macros in the environment. Most of the
    time, macros are stored without scope with <markup|assign>, but it is
    sometimes useful to redefine a tag locally within the scope of a
    <markup|with>. For example, itemized and enumerated environment redefine
    <markup|item> locally.

    <\example>
      Definition of the <markup|abbr> tag

      <\tm-fragment>
        <inactive*|<assign|abbr|<macro|x|<group|<arg|x>>>>>
      </tm-fragment>
    </example>

    Storing a <markup|macro> in the environment defines a tag whose arity is
    fixed to the number of arguments taken by the macro.
  </explain>

  <\explain>
    <explain-macro|arg|var|index-1|<with|mode|math|\<cdots\>>|index-n><explain-synopsis|retrieve
    macro arguments>
  <|explain>
    This primitive is used to retrieve the arguments of a macro within its
    body. For instance, <explain-macro|arg|var> expands the content of the
    macro argument with name <src-arg|arg> (literal string). Of course, this
    argument must be defined by a <markup|macro> containing the <markup|arg>
    tag.

    This tag is similar to <markup|value>, but differs in important ways:

    <\itemize>
      <item>The argument namespace is distinct from the environment,
      <explain-macro|arg|var> and <explain-macro|value|var> will generally
      evaluate to different values (although you should not rely on this).

      <item>The value of <markup|arg> retains the position of the macro
      argument in the document tree, that makes it possible to edit the
      arguments of a macro-defined tag while it is active.
    </itemize>

    When more than one arguments are specified,
    <explain-macro|arg|var|index-1|<with|mode|math|\<cdots\>>|index-n>
    expands to a subtree of the argument <src-arg|var>. The value of the
    named argument must be a compound tree (not a string). The operands
    <src-arg|var> until <src-arg|index-n> must all evaluate to positive
    integers and give the path to the subtree of the macro argument.
  </explain>

  <\explain>
    <explain-macro|xmacro|var|body><explain-synopsis|macro with a variable
    arity>
  <|explain>
    This primitive returns a macro (the <TeXmacs> analogue of a
    <with|mode|math|\<lambda\>>-expression) capable of taking any number of
    arguments. The arguments are stored in the macro variable with name
    <src-arg|var> (a literal string) during the evaluation of the
    <src-arg|body>. The <with|mode|math|i>-th individual argument can then be
    accessed using <explain-macro|arg|var|i>.
  </explain>

  <\explain>
    <explain-macro|map-args|foo|root|var>

    <explain-macro|map-args|foo|root|var|first>

    <explain-macro|map-args|foo|root|var|first|last><explain-synopsis|map a
    tag on subtrees of an argument>
  <|explain>
    This primitive evaluates to a tree whose root is labeled by
    <src-arg|root> and whose children are the result of applying the macro
    <src-arg|foo> to the children of the macro argument with name
    <src-arg|var>.

    By default, the macro <src-arg|foo> is applied to all children. If
    <src-arg|first> has been specified, then we rather start at the
    <with|mode|math|i>-th child of <src-arg|var>, where <with|mode|math|i> is
    the result of evaluating <src-arg|first>. If <src-arg|last> has been
    specified too, then we stop at the <with|mode|math|j>-th child of
    <src-arg|var> (the <with|mode|math|j>-th child not being included), where
    <with|mode|math|j> is the result of evaluating <src-arg|last>. In this
    last case, the arity of the returned tree is therefore
    <with|mode|math|j-i>.

    Stated otherwise, <markup|map-args> applies <src-arg|foo> to all subtrees
    of the macro argument <src-arg|var> (or a range of subtrees if
    <src-arg|first> and <src-arg|last> are specified) and collect the result
    in a tree with label <src-arg|root>. In addition, the second argument to
    <src-arg|foo> gives its position of the first argument in the expansion
    of <src-arg|var>.

    The <markup|map-args> is analogue to the <scheme> function
    <verbatim|map>. Since <TeXmacs> use labeled trees, the label of the
    mapping list must also be specified.

    <\example>
      Comma-separated lists.

      The <markup|comma-separated> tag has any arity (though it does not make
      much sense with arity zero) and typeset its operands interspersed with
      commas.

      <\tm-fragment>
        <inactive*|<assign|comma-extra|<macro|x|, <arg|x>>>>

        <inactive*|<assign|comma-separated|<xmacro|args|<style-with|src-compact|none|<arg|args|0><map-args|comma-extra|concat|args|1>>>>>
      </tm-fragment>
    </example>
  </explain>

  <\explain>
    <explain-macro|eval-args|var><explain-synopsis|macro with a variable
    arity>
  <|explain>
    This primitive evaluates to the tree with the same label as the expansion
    of the argument <src-arg|var> and whose subtrees are the result of the
    evaluation of the subtrees of the expansion of <src-arg|var>.
  </explain>

  <\explain>
    <explain-macro|compound|foo|arg-1|<with|mode|math|\<cdots\>>|arg-n><explain-synopsis|expand
    an unnamed macro>
  <|explain>
    This primitive is useful to expand macros which are the result of a
    computation: it applies the macro which is the result of the evaluation
    of <src-arg|foo> to the arguments <src-arg|arg-1> until <src-arg|arg-n>.
    The <markup|compound> primitive is useful in call-back and lambda
    programming idioms, where a <def-index|higher-level macro> is given a
    macro as an operand, which it may later apply under certain conditions or
    with operands which are not known to the client code.

    Actually, in the current implementation, <src-arg|foo> may either
    evaluate to a macro or to a literal string which gives the name of a
    macro. However, we discourage users to rely on the second case.

    <\example>
      Lambda programming with macros.

      In the code below, <explain-macro|filter|pred|t> expects a macro
      <src-arg|pred> and a tuple <src-arg|t> on input and returns a tuple
      containing the elements of <src-arg|t> for which <src-arg|pred>
      evaluates to <verbatim|true>.

      <\tm-fragment>
        <inactive*|<assign|filter|<macro|pred|t|<style-with|src-compact|none|<if|<equal|<length|<arg|t>>|0>|<tuple>|<style-with|src-compact|none|<merge|<style-with|src-compact|none|<if|<compound|<arg|pred>|<look-up|<arg|t>|0>>|<tuple|<look-up|<arg|t>|0>>|<tuple>>>|<filter|<arg|pred>|<range|<arg|t>|1|<length|<arg|t>>>>>>>>>>>
      </tm-fragment>

      As an application, we may define a macro <explain-macro|evens|t>, which
      expects <src-arg|t> to be a tuple containing integers, and which
      returns the tuple of integers in <src-arg|t> which are divisible by 2.

      <\tm-fragment>
        <inactive*|<assign|evens|<macro|t|<filter|<macro|x|<equal|<mod|<arg|x>|2>|0>>|<arg|t>>>>>
      </tm-fragment>
    </example>
  </explain>

  <\explain>
    <explain-macro|drd-props|var|prop-1|val-1|<with|mode|math|\<cdots\>>|prop-n|val-n><explain-synopsis|set
    <abbr|D.R.D.> properties of a tag>
  <|explain>
    The arity and children accessibility of tags defined by macros are
    determined heuristically by default. The <markup|drd-props> primitive
    overrides this default for the environment variable (usually a macro)
    with name <src-arg|var>. The currently supported property-value pairs
    are:

    <\description-dash>
      <item*|(arity, <with|mode|math|n>)<verbatim|>>Sets the arity to the
      given fixed value <with|mode|math|n> (literal integer).

      <item*|(accessible, all)>Make it impossible to deactivate the tag with
      normal editor actions. Inaccessible children become effectively
      uneditable.

      <item*|(accessible, none)>Make it impossible to position the caret
      within the tag when it is active, so children can only be edited when
      the tag is inactive.
    </description-dash>
  </explain>

  <\explain>
    <explain-macro|get-label|expression><explain-synopsis|label of an
    expression>
  <|explain>
    Returns the label of the tree obtained when evaluating
    <src-arg|expression>.
  </explain>

  <\explain>
    <explain-macro|get-arity|expression><explain-synopsis|arity of an
    expression>
  <|explain>
    Returns the label of the tree obtained when evaluating
    <src-arg|expression>.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>