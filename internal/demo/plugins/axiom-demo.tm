<TeXmacs|1.0.0.6>

<style|<tuple|generic|axiom>>

<\body>
  <\session|axiom|default>
    <\output>
      Axiom Computer Algebra System (Release 2.3)

      Linux 2.2 for Intel i386 (AXLUX23NA)

      -----------------------------------------------------------------------\
      ------

      \ \ \ Issue )copyright to view copyright notices.

      \ \ \ Issue )summary for a summary of useful system commands.

      \ \ \ Issue )quit to leave AXIOM and return to shell.

      -----------------------------------------------------------------------\
      ------

      \;
    </output>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      differentiate(x^x^x,x)
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      a: Symbol := 'a
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      b: Symbol := 'b
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      coef := Fraction(Integer)
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      group := LieExponentials(Symbol, coef, 4)
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      lpoly := LiePolynomial(Symbol, coef)
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      poly := XPBWPolynomial(Symbol, coef)
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      ea := exp(a::lpoly)$group
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      eb := exp(b::lpoly)$group
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      g: group := ea*eb
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      g :: poly
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      log(g)$group
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      g1: group := inv(g)
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      g*g1
    </input>

    <\input|<\with|color|red>
      <with|mode|math|\<rightarrow\>>\ 
    </with>>
      \;
    </input>
  </session>

  \;
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|5mm>
    <associate|page medium|automatic>
    <associate|shrinking factor|4>
    <associate|page right margin|5mm>
    <associate|page top margin|5mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|5mm>
    <associate|reduction page left margin|25mm>
    <associate|page height|757760unit>
    <associate|page bottom margin|5mm>
    <associate|reduction page top margin|15mm>
    <associate|page width|1015808unit>
    <associate|language|english>
  </collection>
</initial>
