<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Special markup for sessions>

  The <tmdtd|session> <abbr|d.t.d.> provides the following environments for
  computer algebra sessions:

  <\explain|<explain-macro|session|body>>
    Environment for marking a session. All macros below are only for use
    inside sessions.
  </explain>

  <\explain|<explain-macro|input|prompt|body>>
    An input field with a <src-arg|prompt> and the actual input.
  </explain>

  <\explain|<explain-macro|output|body>>
    An output field.
  </explain>

  <\explain|<explain-macro|textput|body>>
    Fields with ordinary text. These may for instance be used for comments
    and explanations.
  </explain>

  <\explain|<explain-macro|errput|body>>
    This macro is used inside output fields for displaying error messages.
  </explain>

  In fact, these environments are based on environments of the form
  <markup|<em|lan>-session>, <markup|<em|lan>-input>,
  <markup|<em|lan>-output>, <markup|<em|lan>-textput> and
  <markup|<em|lan>-errput> for every individual language <markup|<em|lan>>.

  If language-specific environments do not exist, then
  <markup|generic-session>, <markup|generic-input>, <markup|generic-output>,
  <markup|generic-textput> and <markup|generic-errput> are taken instead. It
  is recommended to base the language-specific environments on the generic
  ones, which may have different implementations according to the style
  (<abbr|e.g.> the <tmstyle|varsession> package). For this purpose, we also
  provide the <markup|generic-output*> environment, which is similar to
  <markup|generic-output>, except that margins remain unaltered.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>