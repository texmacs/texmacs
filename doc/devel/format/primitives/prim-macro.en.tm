<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Macro primitives>

  Macros can be used to define new tags and to build abstraction with
  procedures in style files.

  Older versions of <TeXmacs> used to make a distinction between macros (all
  children accessible) and functions (no accessible child). In modern
  <TeXmacs> there are only macros: the accessibility of children is
  determined heuristically and can be controlled with
  <markup|drd-props>.<htab|5mm>

  <big-table|<tag-info-table|<tformat|<table|<row|<cell|<strong|Operator>>|<cell|<strong|Arity>>|<cell|<strong|Accessible>>|<cell|>|<cell|<strong|Process>>|<cell|<strong|Usage>>>|<row|<cell|>|<cell|>|<cell|<strong|border>>|<cell|<strong|children>>|<cell|>|<cell|>>|<row|<cell|<markup|macro>>|<cell|<with|mode|math|n+1>>|<cell|Yes>|<cell|All
  but last>|<cell|Macro>|<cell|Style>>|<row|<cell|<markup|arg>>|<cell|<with|mode|math|1+n>>|<cell|Yes>|<cell|None>|<cell|Macro>|<cell|Style>>|<row|<cell|<markup|xmacro>>|<cell|<with|mode|math|2>>|<cell|Yes>|<cell|First>|<cell|Macro>|<cell|Style>>|<row|<cell|<markup|map-args>>|<cell|<with|mode|math|3,4,5>>|<cell|Yes>|<cell|None>|<cell|Macro>|<cell|Style>>|<row|<cell|<markup|eval-args>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Functional>|<cell|Style>>|<row|<cell|<markup|compound>>|<cell|<with|mode|math|1+n>>|<cell|Yes>|<cell|All
  but first>|<cell|Macro>|<cell|Style>>|<row|<cell|<markup|drd-props>>|<cell|<with|mode|math|1+*2*(n+1)>>|<cell|Yes>|<cell|None>|<cell|Typesetting>|<cell|Style>>|<row|<cell|<markup|get-label>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Functional>|<cell|Style>>|<row|<cell|<markup|get-arity>>|<cell|<with|mode|math|1>>|<cell|Yes>|<cell|None>|<cell|Functional>|<cell|Style>>|<row|<cell|<markup|action>>|<cell|<with|mode|math|2,3>>|<cell|Yes>|<cell|First>|<cell|Typesetting>|<cell|Style>>|<row|<cell|<markup|flag>>|<cell|2>|<cell|Yes>|<cell|None>|<cell|Typesetting>|<cell|Style>>>>>|Macro
  primitives>

  <\description>
    <item*|<markup|macro>>Macro of fixed arity

    <verbatim|(macro <var|body>)> creates a macro without argument,
    <var|body> may be any document fragment.

    <verbatim|(macro <var|name1> ... <var|nameN> <var|body>)> creates a macro
    taking <with|mode|math|N> arguments, named after the literal strings
    <var|name1> to <var|nameN>.

    New tags are defined by storing macros in the environment. Most of the
    time, macros are stored without scope with <markup|assign>, but it is
    sometimes useful to redefine a tag locally within the scope of a
    <markup|with>. For example, itemized and enumerated environment redefine
    <markup|item> locally.

    <\example>
      Definition of the <markup|abbr> tag

      <\scheme-fragment>
        (assign "abbr" (macro "x" (group (arg "x"))))
      </scheme-fragment>
    </example>

    Storing a <markup|macro> in the environment defines a tag whose arity is
    fixed to the number of arguments taken by the macro.

    <item*|<markup|arg>>Access arguments of a macro

    <verbatim|(arg <var|name>)> expands to the content of the argument
    <var|name> (literal string). This argument name must be defined by a
    <markup|macro> containing the <markup|arg> tag.

    This tag is similar to <markup|value>, but differs in important ways:

    <\itemize>
      <item>The argument namespace is distinct from the environment,
      <verbatim|(arg <var|x>)> and <verbatim|(value <var|x>)> will generally
      evaluate to different values.

      <item>The value of <markup|arg> retains the position of the macro
      argument in the document tree, that makes it possible to edit the
      arguments of a macro-defined tag while it is active.
    </itemize>

    <verbatim|(arg <var|name> <var|i1> ... <var|iN>)> expands to a subtree of
    the argument <var|name>. The value of the named argument must be a
    compound tree (not a string). The operands
    <var|i1><with|mode|math|\<ldots\>><var|iN> must all evaluate to positive
    integers and give the path to the subtree or

    <item*|<markup|xmacro>>Macro of variable arity

    <verbatim|(xmacro <var|name> <var|body>)> creates a macro of any arity,
    <var|name> (literal string) is bound the whole tag which is defined by
    the macro.

    Individual operands can be accessed using the form <verbatim|(arg
    <var|name> <var|i>)>.

    <item*|<markup|map-args><markup|>>Map a tag on subtrees of an argument

    <verbatim|(map-args <var|tag> <var|root> <var|name>)> evaluates to a tree
    whose label is <var|root> and whose operands are trees whose labels are
    <var|tag> and whose operands are subtrees of the value of <var|name> and
    their position in the values of <var|name>. All operands must be literal
    strings.

    <verbatim|(map-args <var|tag> <var|root> <var|name> <var|i>)> starts
    mapping with the subtree of the value of <var|name> whose index is
    <var|i> (evaluates to an integer). If the value of <var|name> has arity
    <with|mode|math|n>, then <with|mode|math|0\<leqslant\>i\<leqslant\>n> and
    the result tree has arity <with|mode|math|n-i>.

    <verbatim|(map-args <var|tag> <var|root> <var|name> <var|i> <var|j>)>
    ends mapping with the subtree of the value of <var|name> whose index is
    <var|j> (evaluates to an integer). With the same <with|mode|math|i> and
    <with|mode|math|n> as previously, <with|mode|math|j> must satisfy
    <with|mode|math|0\<leqslant\>i\<leqslant\>j\<leqslant\>n> and the result
    tree has arity <with|mode|math|n-i-j>.

    Put more simply, <markup|map-args> applies <var|tag> to all subtrees of
    argument <var|name> (or a range of subtrees if <var|i> or <var|j> are
    specified) and collect the result in a tree with label <var|root>. In
    addition, the second argument to <var|tag> gives its position of the
    first argument in the expansion of <var|name>.

    It is actually an alter-ego of the <value|scheme> function
    <verbatim|map>. Since <TeXmacs> use labelled trees, the label of the
    mapping list must also be specified.

    <\example>
      Comma-separated

      The <markup|comma-separated> tag has any arity (though it does not make
      much sense with arity zero) and typeset its operands interspersed with
      commas.

      <\scheme-fragment>
        (assign "comma-extra" (macro "x"

        \ \ \ \ (concat ", " (arg "x"))))

        (assign "comma-separated" (xmacro "args"

        \ \ \ \ (concat (arg "args" "0")

        \ \ \ \ \ \ \ \ \ \ \ \ (map-args "comma-extra" "concat" "args"
        "1"))))
      </scheme-fragment>
    </example>

    <item*|<markup|eval-args>>Force evaluation of subtrees of an argument

    <verbatim|(eval-args <var|name>)> evaluates to the tree with the same
    label as the expansion of the argument <var|name> and whose subtrees are
    the result of the evaluation of the subtrees of the expansion of
    <var|name>.

    <todo|How is that practically useful?>

    <item*|<markup|compound>>Expand an unnamed macro

    <verbatim|(compound <var|macro> <var|arg1> ... <var|argN>)> applies
    <var|macro> to the arguments <var|arg1><with|mode|math|\<ldots\>><var|argN>.

    If <var|macro> is the literal string <verbatim|"<var|tag>">, it is
    equivalent to <verbatim|(<var|tag> <var|arg1> ... <var|argN>)>.

    The <var|macro> operand may be a literal string (trivial case) or
    expression which evaluates to a string <emdash>which is the name of a
    macro<emdash> or to a <markup|macro> expression. The operands
    <var|arg1><with|mode|math|\<ldots\>><var|argN> may be anything that
    <var|macro> can handle.

    The <markup|compound> primitive is useful when the name or the expression
    of the macro is computed. This is useful in call-back and lambda
    programming idioms, where a <def-index|higher-level macro> is given a
    macro as an operand, which it may later apply under certain conditions or
    with operands which are not known the client code.

    <\example>
      Lambda programming with macros

      <verbatim|(filter <var|pred> <var|t>)> expects a macro (name or
      expression) as <var|pred> and a tuple as <var|t>, it returns a tuple
      containing the elements of <var|t> for which <var|pred> evaluates to
      <verbatim|true>.

      <\scheme-fragment>
        (assign "filter" (macro "pred" "t"

        \ \ \ \ \ (if (equal "0" (length (arg "t")))

        \ \ \ \ \ \ \ \ \ (tuple)

        \ \ \ \ \ \ \ (merge (if (compound (arg "pred")

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (look-up (arg
        "t") "0"))

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (tuple (look-up (arg "t") "0"))

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (tuple))

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ (filter (arg "pred")

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (range (arg "t") "1"

        \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (length
        (arg "t"))))))))
      </scheme-fragment>

      <verbatim|(evens <var|t>)> expects <var|t> to be a tuple containing
      integers, it returns the tuple of integers in <var|t> which are
      divisible by 2.

      <\scheme-fragment>
        (assign "evens" (macro "t"

        \ \ \ \ (filter (macro "x" (equal "0" (mod (arg "x") "2")))

        \ \ \ \ \ \ \ \ \ \ \ \ (arg "t"))))
      </scheme-fragment>
    </example>

    <\remark>
      <markup|compound> for dispatching

      In <TeXmacs> 1.0.3, <markup|compound> is also needed to correctly
      implement dispatching when the expansion may be typeset as a block.
      This is required to work around an internal limitation in the
      typesetter.
    </remark>

    <item*|<markup|><markup|drd-props>>Set <abbr|D.R.D.> properties of a tag

    The arity and children accessibility of tags defined by macros are
    determined heuristically by default. The <markup|drd-props> primitive
    overrides this default.

    <verbatim|(drd-props <var|tag> <var|prop1> <var|val1> ... <var|propN>
    <var|valN>)> sets the <abbr|D.R.D.> properties of <var|tag> (literal
    string), which must be defined by a macro.

    The supported property-value pairs are:

    <\description-dash>
      <item*|<verbatim|"arity" <var|n>>>Sets the arity to the given fixed
      value <var|n> (literal integer).

      <item*|<verbatim|"accessible" "all">>Make it impossible to deactivate
      the tag with normal editor actions. Inaccessible children become
      effectively uneditable.

      <item*|<verbatim|"accessible" "none">>Make it impossible to position
      the caret within the tag when it is active, so children can only be
      edited when the tag is inactive.
    </description-dash>

    <item*|<markup|get-label>>Label of a tree

    <item*|<markup|get-arity>>Arity of a tree

    <item*|<markup|action>>Bind a <value|scheme> script to mouse click

    <item*|<markup|flag>>Display an informative flag
  </description>

  <tmdoc-copyright|2004|David Allouche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
    <associate|preamble|false>
  </collection>
</initial>