<TeXmacs|1.99.11>

<style|<tuple|generic|british|old-dots>>

<\body>
  <doc-data|<doc-title|The Python Language>>

  <\session|python|default>
    <\output>
      Python 2.7.10 (default, Feb 22 2019, 21:55:15)\ 

      [GCC 4.2.1 Compatible Apple LLVM 10.0.1 (clang-1001.0.37.14)]

      Python plugin for TeXmacs.

      Please see the documentation in Help -\<gtr\> Plugins -\<gtr\> Python
    </output>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      "hello"
    <|unfolded-io>
      hello
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      "hello world"
    <|unfolded-io>
      hello world
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      "hello world\\n"
    <|unfolded-io>
      hello world
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      """hello world"""
    <|unfolded-io>
      hello world
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      1.2
    <|unfolded-io>
      1.2
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      1.2e10
    <|unfolded-io>
      12000000000.0
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      1.2000
    <|unfolded-io>
      1.2
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      1.2E10
    <|unfolded-io>
      12000000000.0
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      1.3e-100
    <|unfolded-io>
      1.3e-100
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      0xEE
    <|unfolded-io>
      238
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      0xEE
    <|unfolded-io>
      238
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      314159L
    <|unfolded-io>
      314159
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      314159l
    <|unfolded-io>
      314159
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      0b1101
    <|unfolded-io>
      13
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      0B1101
    <|unfolded-io>
      13
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|unfolded-io>
      .001
    <|unfolded-io>
      0.001
    </unfolded-io>

    <\input>
      \<gtr\>\<gtr\>\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <section|Number>

  <subsection|Integer Literals>

  <\shell-code>
    integer \ \ \ \ \ ::= \ decinteger \| bininteger \| octinteger \|
    hexinteger

    decinteger \ \ ::= \ nonzerodigit (["_"] digit)* \| "0"+ (["_"] "0")*

    bininteger \ \ ::= \ "0" ("b" \| "B") (["_"] bindigit)+

    octinteger \ \ ::= \ "0" ("o" \| "O") (["_"] octdigit)+

    hexinteger \ \ ::= \ "0" ("x" \| "X") (["_"] hexdigit)+

    nonzerodigit ::= \ "1"..."9"

    digit \ \ \ \ \ \ \ ::= \ "0"..."9"

    bindigit \ \ \ \ ::= \ "0" \| "1"

    octdigit \ \ \ \ ::= \ "0"..."7"

    hexdigit \ \ \ \ ::= \ digit \| "a"..."f" \| "A"..."F"
  </shell-code>

  <\python-code>
    # decimal-literal suffix(optional)

    10, 10j, 10J

    # octal-literal suffix(optional)

    0o10, 0O10

    # hex-literal suffix(optional)

    0x10, 0X10

    # binary-literal suffix(optional)

    0b10, 0B10
  </python-code>

  <subsection|Floating Point Literals>

  <\python-code>
    floatnumber \ \ ::= \ pointfloat \| exponentfloat

    pointfloat \ \ \ ::= \ [digitpart] fraction \| digitpart "."

    exponentfloat ::= \ (digitpart \| pointfloat) exponent

    digitpart \ \ \ \ ::= \ digit (["_"] digit)*

    fraction \ \ \ \ \ ::= \ "." digitpart

    exponent \ \ \ \ \ ::= \ ("e" \| "E") ["+" \| "-"] digitpart
  </python-code>

  <\python-code>
    3.14, 10., .001, 1e100, 3.14e-10, 0e0, 3.14_15_93
  </python-code>

  <subsection|Imaginary Literals>

  <\shell-code>
    imagnumber ::= \ (floatnumber \| digitpart) ("j" \| "J")
  </shell-code>

  <\python-code>
    3.14j, 10.j, 10j, .001j, 1e100j, 3.14e-10j, 3.14_15_93j

    \;

    # bad cases

    0x10j, 0o10j, 0b10j
  </python-code>

  <section|References>

  <\itemize>
    <item><hlink|The Python Language Reference(3.7)|https://docs.python.org/3.7/reference/index.html>
  </itemize>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1.2|?>>
    <associate|auto-4|<tuple|1.3|?>>
    <associate|auto-5|<tuple|2|?>>
    <associate|auto-6|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Number>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>References>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>