<TeXmacs|1.0.7.11>

<style|<tuple|tmdoc|maxima>>

<\body>
  <\tmdoc-title>
    Example <name|Yacas> session
  </tmdoc-title>

  You may start a <name|Yacas> session using <menu|Insert|Session|Yacas>.
  Here follows a short demo:

  <\session|yacas|defau>
    <\output>
      \;
    </output>

    <\unfolded-io|yacas] >
      f(x):= x+Sin(x);
    <|unfolded-io>
      <\math>
        <math-up|True>
      </math>
    </unfolded-io>

    <\unfolded-io|yacas] >
      D(x) f(x);
    <|unfolded-io>
      <\math>
        cos x+1
      </math>
    </unfolded-io>

    <\unfolded-io|yacas] >
      Integrate(x,0,Pi) f(x);
    <|unfolded-io>
      <\math>
        <frac|\<pi\><rsup|2>|2>+2
      </math>
    </unfolded-io>

    <\input|yacas] >
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