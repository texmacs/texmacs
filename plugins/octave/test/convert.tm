<TeXmacs|1.99.12>

<style|<tuple|generic|british>>

<\body>
  <doc-data|<doc-title|Tests on octave/convert>>

  <section|num2scm>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (0))
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (0.0))
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (int32 (0)))
    <|unfolded-io>
      <with|mode|math|0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (int64 (0)))
    <|unfolded-io>
      <with|mode|math|0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (1))
    <|unfolded-io>
      <with|mode|math|1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (-1))
    <|unfolded-io>
      <with|mode|math|-1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (0.1))
    <|unfolded-io>
      <with|mode|math|0.1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (-0.1))
    <|unfolded-io>
      <with|mode|math|-0.1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (3 + 42i))
    <|unfolded-io>
      <with|mode|math|3+42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (3-42i))
    <|unfolded-io>
      <with|mode|math|3-42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (42i))
    <|unfolded-io>
      <with|mode|math|42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (-42i))
    <|unfolded-io>
      <with|mode|math|-42\<cdot\>\<b-i\>>
    </unfolded-io>

    <\input>
      octave\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>num2scm>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>