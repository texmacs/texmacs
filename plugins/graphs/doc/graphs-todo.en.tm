<TeXmacs|1.99.9>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Possible improvements>

  <\itemize-dot>
    <item>Re-implement the FeynMF/Xy-pic/Asymptote/DraTeX plugins using
    Python and share the common part of the code. Remember to check the
    usability on Windows. Currently, these plugins are implemented using
    Shell scripting and not available for Windows.

    <item>Pasting existing code from a terminal into TeXmacs requires the
    annoying ``paste verbatim''. Can this be avoided?

    <item>Advanced control (for FeynMF): currently the
    begin{fmfgraph*}(diagw,diagh) stuff is hard-coded into the plugin, which
    however makes it work pretty clean. Perhaps a separate plugin
    feynmf-expert for more complicated stuff. Or a startup option via
    texmacs? E.g. typing ``expert'' switches to expert mode.

    <item>How to make the plugin produce output in a switch?
  </itemize-dot>

  <tmdoc-copyright|2019|Maarten Wegewijs|Joris van der Hoeven and
  Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|par-hyphen|normal>
    <associate|preamble|false>
  </collection>
</initial>