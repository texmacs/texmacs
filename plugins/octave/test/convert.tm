<TeXmacs|1.99.12>

<style|<tuple|generic|british>>

<\body>
  <doc-data|<doc-title|Tests on octave/convert>>

  <section|num2scm>

  <subsection|int32/int42/float scalar>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      0
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

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
      0.0
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
      int32 (0)
    <|unfolded-io>
      <with|mode|math|0>
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
      int64 (0)
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
      1
    <|unfolded-io>
      <with|mode|math|1>
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
      -1
    <|unfolded-io>
      <with|mode|math|-1>
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
      0.1
    <|unfolded-io>
      <with|mode|math|0.1>
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
      -0.1
    <|unfolded-io>
      <with|mode|math|-0.1>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (num2scm (-0.1))
    <|unfolded-io>
      <with|mode|math|-0.1>
    </unfolded-io>

    <\input>
      octave\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <subsection|complex scalar>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      3 + 42i
    <|unfolded-io>
      <with|mode|math|3+42\<cdot\>\<b-i\>>
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
      3 - 42i
    <|unfolded-io>
      <with|mode|math|3-42\<cdot\>\<b-i\>>
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
      42i
    <|unfolded-io>
      <with|mode|math|42\<cdot\>\<b-i\>>
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
      -42i
    <|unfolded-io>
      <with|mode|math|-42\<cdot\>\<b-i\>>
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

  <section|mat2scm>

  <\session|octave|default>
    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      zeros(3, 3)
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      ones(3, 3)
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (mat2scm (zeros (3, 3)))
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      flush_scheme (mat2scm (ones (3, 3)))
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>|<row|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>|<cell|<with|mode|math|1>>>>>>>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      zeros(1, 1)
    <|unfolded-io>
      <with|mode|math|0.0>
    </unfolded-io>

    <\unfolded-io>
      octave\<gtr\>\ 
    <|unfolded-io>
      zeros(2, 2)
    <|unfolded-io>
      <with|mode|math|math-display|true|<matrix|<tformat|<table|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>|<row|<cell|<with|mode|math|0.0>>|<cell|<with|mode|math|0.0>>>>>>>
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
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1.2|?>>
    <associate|auto-4|<tuple|2|?>>
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