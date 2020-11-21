<TeXmacs|1.99.16>

<style|<tuple|tmdoc|british|old-lengths>>

<\body>
  <tmdoc-title|Demo Shell Session>

  <\session|shell|default>
    <\unfolded-io|Shell] >
      echo "Hello, TeXmacs"
    <|unfolded-io>
      Hello, TeXmacs
    </unfolded-io>

    <\unfolded-io|Shell] >
      echo $TEXMACS_PATH
    <|unfolded-io>
      /usr/share/TeXmacs
    </unfolded-io>

    <\unfolded-io|Shell] >
      python --version
    <|unfolded-io>
      Python 3.7.1
    </unfolded-io>

    <\input|Shell] >
      \;
    </input>
  </session>

  With <menu|Focus|Output|Show timings> checked

  <\session|shell|default>
    <\unfolded-io|Shell] >
      sleep 3
    <|unfolded-io>
      <timing|3.042 sec>
    </unfolded-io>

    <\input|Shell] >
      \;
    </input>
  </session>

  <tmdoc-copyright|2020|Darcy Shen>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>