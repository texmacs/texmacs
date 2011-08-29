<TeXmacs|1.0.7.11>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\tmdoc-title>
    Example <name|Maxima> session
  </tmdoc-title>

  Here follows a sample session, which was started using
  <menu|Insert|Session|Maxima>. A nice tutorial by Andrey Grozin is available
  from the <TeXmacs> web site.

  <\session|maxima|default>
    <\output>
      GCL (GNU Common Lisp) \ Version(2.4.4) ¶g¤@ \ 2¤ë 17 15:25:31 CST 2003

      Licensed under GNU Library General Public License

      Contains Enhancements by W. Schelter

      Maxima 5.9.0 http://maxima.sourceforge.net

      Distributed under the GNU Public License. See the file COPYING.

      Dedicated to the memory of William Schelter.

      This is a development version of Maxima. The function bug_report()

      provides bug reporting information.

      \;
    </output>

    <\unfolded-io>
      <with|color|red|(<with|math-font-family|rm|%i>1) >
    <|unfolded-io>
      f(x) := x + sin(x)
    <|unfolded-io>
      <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o1>)
      >>f<around*|(|x|)>\<assign\>x+sin <around*|(|x|)>>>
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|(<with|math-font-family|rm|%i>1) >
    <|unfolded-io>
      diff (f(x), x)
    <|unfolded-io>
      <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o2>)
      >>cos <around*|(|x|)>+1>>
    </unfolded-io>

    <\unfolded-io>
      <with|color|red|(<with|math-font-family|rm|%i>1) >
    <|unfolded-io>
      integrate (f(x), x)
    <|unfolded-io>
      <math|<with|math-display|true|<text|<with|font-family|tt|color|red|(<with|math-font-family|rm|%o3>)
      >><frac|x<rsup|2>|2>-cos <around*|(|x|)>>>
    </unfolded-io>

    <\input>
      <with|color|red|(<with|math-font-family|rm|%i>4) >
    <|input>
      \;
    </input>
  </session>

  <tmdoc-copyright|2003|Chu-Ching Huang>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>