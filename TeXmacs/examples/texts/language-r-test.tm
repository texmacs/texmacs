<TeXmacs|1.99.12>

<style|<tuple|generic|chinese|old-dots>>

<\body>
  <doc-data|<doc-title|The R Language>>

  <section|Highlight>

  <subsection|<hlink|Identifiers|https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Identifiers>>

  <subsection|Numbers>

  <\session|r|default>
    <\unfolded-io>
      <with|color|red|\<gtr\> >
    <|unfolded-io>
      1L
    <|unfolded-io>
      1L

      [1] 1
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|\<gtr\> >
    <|unfolded-io>
      0x10L
    <|unfolded-io>
      0x10L

      [1] 16
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|\<gtr\> >
    <|unfolded-io>
      1000000L
    <|unfolded-io>
      1000000L

      [1] 1000000
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|\<gtr\> >
    <|unfolded-io>
      1e6L
    <|unfolded-io>
      1e6L

      [1] 1000000
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|\<gtr\> >
    <|unfolded-io>
      2i
    <|unfolded-io>
      2i

      [1] 0+2i
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|\<gtr\> >
    <|unfolded-io>
      4.1i
    <|unfolded-io>
      4.1i

      [1] 0+4.1i
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|\<gtr\> >
    <|unfolded-io>
      1e-2i
    <|unfolded-io>
      1e-2i

      [1] 0+0.01i
    </unfolded-io>

    <\input>
      <with|color|red|\<gtr\> >
    <|input>
      \;
    </input>
  </session>

  <\r-code>
    # Valid integer contants

    1L, 0x10L, 1000000L, 1e6L

    1.1L, 1e-3L, 0x1.1p-2

    # Valid complex contants

    2i 4.1i 1e-2i
  </r-code>

  <section|References>

  <\itemize>
    <item><hlink|R Language Definition: Constants|https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Literal-constants>
  </itemize>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|1.2|?>>
    <associate|auto-4|<tuple|2|?>>
    <associate|auto-5|<tuple|2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Highlight>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|4tab>|number <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.15fn>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>References>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>