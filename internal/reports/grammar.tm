<TeXmacs|1.0.3.8>

<style|tmdoc>

<\body>
  <\make-title>
    <title|<TeXmacs> grammar tools>

    <author|Joris van der Hoeven>

    <\address>
      Home sweet home.
    </address>
  </make-title>

  <\abstract>
    The aim of the <TeXmacs> grammar tools is to provide general functions
    for parsing and pretty printing mathematical formulas or programs
    according to classical LALR grammars. We provide a language for
    describing

    <\enumerate>
      <item>Lexical symbol groups.

      <item>Typographical rules associated to lexical symbol groups.

      <item>Rewriting rules.

      <item>A module system for grammars.
    </enumerate>
  </abstract>

  <section|General architecture>

  Creating a suite of <TeXmacs> grammar-tools has several aims:

  <\itemize>
    <item>Provide tools for maintaining a correspondance between several
    representations of mathematical expressions or computer programs
    (typically a semantic one like a Scheme-expression and a physical
    representation on the screen).

    <item>Use this correspondance to enhance the rendering of editing
    behaviour for mathematical expressions (copy and paste of semantic
    subtrees, syntactic highlighting, <abbr|etc.>).

    <item>Make it possible to locally enrich grammars with new rules
    (priorities, extra notations, extra typographic rules, <abbr|etc.>).

    <item>Associating precise typographic rules to the grammar being used.

    <item>Make it easier for extern software to communicate with <TeXmacs>.
  </itemize>

  As to the grammars being used, one has basically two options:

  <\itemize>
    <item>A fixed set of expression types (binary infix, prefix, <abbr|etc.>)
    and priorities.

    <item>LALR grammars.
  </itemize>

  We have opted for the second approach. It is classical that this approach
  covers the first approach, but in a verbose way. As we will see in section
  <reference|param-grammar>, the concept of ``parameterized grammars'' will
  allow us to cover the first approach in a more natural, less verbose way:
  new sets of expression types will correspond to parameterized grammar
  modules and inequalities between priorities to productions (thereby
  providing a more fine-grained control over priorities, which are not
  necessarily linearly ordered).

  The grammars mainly provide us with rewriting algorithms, mainly parsers
  and pretty printers. Although the grammars work on lists on a given
  alphabet of terminal symbols, it is easy to cover trees as well, by
  serialize them using special symbols.

  As to the editing of mathematical formulas, the rewriting tools provide us
  with the possibility to <em|choose> a preferred representation. When
  choosing a representation close to rendering (as currently in <TeXmacs>)
  editing is simple, but obtaining structured semantic information more
  cumbersome. When choosing a semantic representation (like Philippe
  Audebaud, and as planned by the <with|mode|math|\<Omega\>>-group; also used
  for <TeXmacs> style-file editing), the editing is a bit more cumbersome,
  but structured editing operations easier. I am not sure which choice is
  best, but I <em|do> believe that the incorporation of powerful rewriting
  tools into <TeXmacs> will make this issue less and less important.

  <section|Lexical symbol groups>

  The lexer produces lists of terminal symbols of user-definable types. New
  types can be defined (or further enriched) using instructions of the form

  <\scheme-fragment>
    (terminal <em|symbol> <em|regexpr-1> ... <em|regexpr-n>)
  </scheme-fragment>

  Here <verbatim|<em|symbol>> denotes the name of a terminal symbol and
  <verbatim|<em|regexpr-1>> until <verbatim|<em|regexpr-n>> regular
  expressions. For instance, one may define

  <\scheme-fragment>
    (terminal plus "+")

    (terminal times "*")

    (terminal id (repeat (or (range "a" "z") (range "A" "Z") "_")))

    (terminal open "(")

    (terminal close ")")
  </scheme-fragment>

  In extension grammars, these terminal symbols may be further enriched:

  <\scheme-fragment>
    (terminal plus "\<less\>oplus\<gtr\>")

    (terminal times "\<less\>otimes\<gtr\>")
  </scheme-fragment>

  For instance, the input string <verbatim|hop+x> would be lexed as
  <verbatim|((hop id) (+ plus) (x id))>.\ 

  In order to lex <TeXmacs>-trees, we enrich the alphabet with special
  symbols of the form <verbatim|\\<em|tag>>, <verbatim|\|<em|tag>> and
  <verbatim|/<em|tag>> for encoding trees. For instance, the tree

  <\equation*>
    <tree|concat|sin x+|<tree|frac|b|c>>
  </equation*>

  corresponding to <with|mode|math|sin x+<frac|b|c>> would be encoded as
  <verbatim|(\\concat 's' 'i' 'n' ' ' 'x' \|concat \\frac 'b' \|frac 'c'
  /frac /concat)>. It may be useful to add a facility like

  <\scheme-fragment>
    (ignore \\concat \|concat /concat)
  </scheme-fragment>

  to the lexer language for ignoring certain tokens or comments. Similarly,
  it may be useful to add a primitive <verbatim|accept> which specifies which
  tags are accepted (all other being ignored).

  Scheme trees may encoded using special symbols <verbatim|[> and
  <verbatim|]> in a similar way. For instance the scheme expression
  <verbatim|(+ (sin x) (/ b c))> corresponding to the tree

  <\equation*>
    <tree|+|<tree|sin|x>|<tree|/|b|c>>
  </equation*>

  might be encoded by <verbatim|[ "+" [ "sin" "x" ] [ "b" "c" ] ]>.

  <section|Typographical rules>

  Typographical rules are associated to terminal symbols. For instance:

  <\scheme-fragment>
    (penalty plus (invalid 30))

    (spacing plus (default default))
  </scheme-fragment>

  It might also be interesting to allow grammars to export additional
  <TeXmacs> macro definitions:

  <\scheme-fragment>
    (macro my-notation (macro "x" "y" ...))
  </scheme-fragment>

  <section|Rewriting rules>

  Context-free grammars are both used for parsing purposes and pretty
  printing. A context-free rewriting rule is of the form

  <\scheme-fragment>
    (rule <em|symbol> <em|source> <em|destination>)
  </scheme-fragment>

  Here <verbatim|<em|symbol>> is a non-terminal symbol, <verbatim|<em|rule>>
  a list of terminal or non-terminal symbols and <verbatim|<em|production>> a
  list of terminal symbols or symbols or numbers. For instance, the following
  grammar can be used to parse simple arithmetic expressions:

  <\scheme-fragment>
    (rule AddExpr (MultExpr plus AddExpr) ([ 2 1 3 ]))

    (rule AddExpr (MultExpr) (1))

    (rule MultExpr (RadExpr times MultExpr) ([ 2 1 3 ]))

    (rule MultExpr (RadExpr) (1))

    (rule RadExpr (id) (1))

    (rule RadExpr (open AddExpr close) (2))
  </scheme-fragment>

  Here <verbatim|plus>, <verbatim|times>, <verbatim|id>, <verbatim|open> and
  <verbatim|close> are terminal symbols. Typically a list like <verbatim|((a
  id) (+ <no-break>plus) (b id) (+ plus) (c id))> would produce <verbatim|([
  + a [ + b c ] ])>.

  In a similar way, the following grammar may be used in order for pretty
  printing trees produced using the above grammar:

  <\scheme-fragment>
    (rule AddExpr ([ MultExpr plus AddExpr ]) (2 1 3))

    (rule AddExpr (MultExpr) (1))

    (rule MultExpr ([ RadExpr times MultExpr ]) (2 1 3))

    (rule MultExpr (RadExpr) (1))

    (rule RadExpr (id) (1))

    (rule RadExpr (AddExpr) (open 2 close))
  </scheme-fragment>

  Notice that the last rule of this grammar introduces a circular ambiguity.
  The correct way of parsing such ambiguous grammars is to use the shortest
  derivation tree.

  <\remark>
    The parsing tool should be should be able to partially cope with
    ambiguity as follows:

    <\enumerate>
      <item>It should resolve circular ambiguities in the expected way.

      <item>It should accept general ambiguous grammars (and produce
      ambiguous parsing tables). When parsing, we may use a small look-ahead,
      and chose an arbitrary production for highly ambiguous expression.

      <item>When parsing an ambiguous expression, we should be able to return
      a list of positions where we made arbitrary choices for productions.
    </enumerate>
  </remark>

  <section|Grammar modules>

  New lexer rules, typographical rules and rewriting rules may be grouped
  together into grammar modules. For instance:

  <\scheme-fragment>
    (grammar (Arith-terminals)

    \ \ (terminal plus "+")

    \ \ (terminal times "*")

    \ \ (terminal id (repeat (or (range "a" "z") (range "A" "Z") "_")))

    \ \ (terminal open "(")

    \ \ (terminal close ")"))
  </scheme-fragment>

  Grammars may be inherited:

  <\scheme-fragment>
    (grammar (Arith-parser)

    \ \ (import (Arith-terminals))

    \ \ (rule AddExpr (MultExpr plus AddExpr) ([ 2 1 3 ]))

    \ \ (rule AddExpr (MultExpr) (1))

    \ \ (rule MultExpr (RadExpr times MultExpr) ([ 2 1 3 ]))

    \ \ (rule MultExpr (RadExpr) (1))

    \ \ (rule RadExpr (id) (1))

    \ \ (rule RadExpr (open AddExpr close) (2)))
  </scheme-fragment>

  <section|Paramerized grammars and meta-rules><label|param-grammar>

  An interesting feature is that we allow grammar specifications to take
  parameters:

  <\scheme-fragment>
    (grammar (Associative-infix which)

    \ \ (rule (:: Expr which) ((:: Expr+ which) which (:: Expr which))

    \ \ \ \ \ \ \ \ ([ 2 1 3]))

    \ \ (rule (:: Expr which) (:: Expr+ which) (1))

    \ \ (rule (:: Expr which) (RadExpr) (1))

    \ \ (spacing which (default default)))
  </scheme-fragment>

  The <verbatim|::> is used to indicate scoping. For instance <verbatim|(::
  Expr plus)> stands for <verbatim|Expr::plus>. Now consider the following
  grammar

  <\scheme-fragment>
    (grammar (Arith-primitive-parser)

    \ \ (import (Arith-terminals))

    \ \ (import (Associative-infix plus))

    \ \ (import (Associative-infix times))

    \ \ (rule RadExpr (id) (1))

    \ \ (rule RadExpr (open AddExpr close) (2)))
  </scheme-fragment>

  Notice that we have not yet specified which operation among <verbatim|plus>
  and <verbatim|times> has priority. This has been done on purpose, for it
  allows us to deal with underspecification. The user may enrich the above
  language in a local context in order to specify additional priorities:

  <\scheme-fragment>
    (grammar (Arith-parser)

    \ \ (import (Arith-primitive-parser))

    \ \ (rule Expr+::plus (Expr::times) (1)))
  </scheme-fragment>

  Another use of scoping is the possibility to define different grammars (one
  for parsing, two for pretty printing, <abbr|etc.>) and to selectively use
  them. For instance, <verbatim|Arith-parser::Expr::plus> may be used as a
  non-terminal symbol even if <verbatim|Arith-parser> has not been imported.
</body>