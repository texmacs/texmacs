<TeXmacs|1.99.11>

<style|<tuple|generic|british>>

<\body>
  <doc-data|<doc-title|The Scala Language>>

  <section|Syntax Highlight>

  <paragraph|Literals>

  <\scala-code>
    0

    1

    2

    3

    "hello"

    "hello world"

    "hello world\\n"

    """hello world"""

    1.2

    1.2000

    1.2e10

    1.2E10

    1.3e-100

    3.14159f

    3.14159F

    3.14159d

    3.14159D

    0xEE

    999999l

    999999L

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
    <item><hlink|Scala Language Specification|https://scala-lang.org/files/archive/spec/2.13/>
  </itemize>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?|../../../../.TeXmacs/texts/scratch/no_name_167.tm>>
    <associate|auto-2|<tuple|1|?|../../../../.TeXmacs/texts/scratch/no_name_167.tm>>
    <associate|auto-3|<tuple|2|?|../../../../.TeXmacs/texts/scratch/no_name_167.tm>>
    <associate|auto-4|<tuple|3|?|../../../../.TeXmacs/texts/scratch/no_name_167.tm>>
    <associate|auto-5|<tuple|2|?|../../../../.TeXmacs/texts/scratch/no_name_167.tm>>
  </collection>
</references>