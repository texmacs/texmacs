<TeXmacs|1.99.11>

<style|<tuple|generic|british|old-dots>>

<\body>
  <doc-data|<doc-title|The Scala Language>>

  <section|Syntax Highlight>

  <subsection|Literals>

  <paragraph|Integer Literals>

  <\verbatim-code>
    integerLiteral ::= (decimalNumeral \| hexNumeral) [`L' \| `l']

    decimalNumeral ::= `0' \| nonZeroDigit {digit}

    hexNumeral ::= `0' (`x' \| `X') hexDigit {hexDigit}

    digit ::= `0' \| nonZeroDigit

    nonZeroDigit ::= `1' \| ... \| `9'
  </verbatim-code>

  <\scala-code>
    // decimal-literal suffix(optional)

    10, 10l, 10L

    // hex-literal suffix(optional)

    0x10, 0x10l, 0x10L, 0X10, 0X10l, 0X10L
  </scala-code>

  <paragraph|Floating Point Literals>

  <\shell-code>
    floatingPointLiteral \ ::= \ digit {digit} `.' digit {digit}
    [exponentPart] [floatType]

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \| \ `.' digit {digit}
    [exponentPart] [floatType]

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \| \ digit {digit}
    exponentPart [floatType]

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \| \ digit {digit}
    [exponentPart] floatType

    exponentPart \ \ \ \ \ \ \ \ \ ::= \ (`E' \| `e') [`+' \| `-'] digit
    {digit}

    floatType \ \ \ \ \ \ \ \ \ \ \ \ ::= \ `F' \| `f' \| `D' \| `d'
  </shell-code>

  <\scala-code>
    10f, 10F, 10d, 10D

    1.2

    1.2000

    1.2e10

    1.2E10

    1.3e-100

    3.14159f

    3.14159F

    3.14159d

    3.14159D
  </scala-code>

  <paragraph|Boolean Literals>

  <\verbatim-code>
    booleanLiteral \ ::= \ `true' \| `false'
  </verbatim-code>

  <\scala-code>
    true

    false
  </scala-code>

  <paragraph|Character Literals>

  <\verbatim-code>
    characterLiteral \ ::=\ 

    \ \ `'' (charNoQuoteOrNewline \| UnicodeEscape \| charEscapeSeq) `''
  </verbatim-code>

  <\scala-code>
    ('a', '\\u0041', '\\n', '\\t')
  </scala-code>

  <paragraph|String Literals>

  <\scala-code>
    \;

    "hello"

    "hello world"

    "hello world\\n"

    """hello world"""

    null
  </scala-code>

  <paragraph|Expressions>

  <\scala-code>
    if (x \<gtr\> 10) x + 1

    else x -1

    \;

    while (x \<gtr\> 10) {

    \ \ x = x + 1

    }

    \;

    for (x \<less\>- 1 to 10) {

    \ \ println(x)

    }

    \;

    val x = 1

    var y = 2
  </scala-code>

  <paragraph|string interop>

  <\scala-code>
    s"Hello, $name"

    s"Hello, ${name}"
  </scala-code>

  <section|References>

  <\itemize>
    <item><hlink|Scala Language Specification#Lexical
    Syntax|https://scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html>
  </itemize>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-10|<tuple|2|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1|?>>
    <associate|auto-4|<tuple|2|?>>
    <associate|auto-5|<tuple|3|?>>
    <associate|auto-6|<tuple|4|?>>
    <associate|auto-7|<tuple|5|?>>
    <associate|auto-8|<tuple|6|?>>
    <associate|auto-9|<tuple|7|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Syntax
      Highlight> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|4tab>|Numbers
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.15fn>>

      <with|par-left|<quote|4tab>|Literals
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.15fn>>

      <with|par-left|<quote|4tab>|Expressions
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4><vspace|0.15fn>>

      <with|par-left|<quote|4tab>|string interop
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|0.15fn>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>References>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>