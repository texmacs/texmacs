<TeXmacs|1.0.3.11>

<style|source>

<\body>
  See a LaTeX math command as a TeXmacs expression via plugin

  <assign|latexer|<macro|x|<extern|latexer|<quote-arg|x>>>>

  \;

  Example of macro which calls a secure scheme command

  <assign|inv|<macro|x|<extern|(lambda (x) `(frac "1" ,x))|<quote-arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>