<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing numbered textual environments>

  <TeXmacs> provides three standard types of numbered textual environments:
  theorem-like environments, remark-like environments and exercise-like
  environments. The following aspects of these environments can be easily
  customized:

  <\itemize>
    <item>Adding new environments.

    <item>Modifying the rendering of the environments.

    <item>Numbering the theorems in a different way.
  </itemize>

  <paragraph|Defining new environments>

  First of all, new environments can be added using the meta-macros
  <markup|new-theorem>, <markup|new-remark> and <markup|new-exercise>. These
  environments take two arguments: the name of the environment and the name
  which is used for its rendering. For instance, you may wish to define the
  environment <markup|experiment> by

  <\tm-fragment>
    <inactive*|<new-theorem|experiment|Experiment>>
  </tm-fragment>

  When available in the <TeXmacs> dictionnaries, the text ``Experiment'' will
  be automatically translated when your document is written in a foreign
  language. In the section about <hyper-link|how to define new
  environments|../../../main/styles/std-dtds/env-manage-dtd.en.tm>, it is
  also explained how to define other numbered textual environments (besides
  theorems, remarks and exercises).

  <paragraph|Customization of the rendering>

  The principal rendering of the environments can be customized by redefining
  the <markup|render-theorem>, <markup|render-remark> and
  <markup|render-exercise> macros. These macros take the <src-arg|name> of
  the environment (like ``Theorem <no-break>1.2'') and its <src-arg|body> as
  arguments. By default, the rendering of remarks is the same as the
  rendering of theorems except that their bodies are typeset in an upright
  font. Hence, redefining the <markup|render-theorem> macro will also affect
  the rendering of remarks. For instance, if you want theorems to appear in a
  slightly indented way, with a slanted body, then you may redefine
  <markup|render-theorem> as follows:

  <\tm-fragment>
    <inactive*|<assign|render-theorem|<macro|which|body|<style-with|src-compact|none|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<right-flush><vspace|1fn>|<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>>>>
  </tm-fragment>

  This redefinition produces the following effect:

  <\with|render-theorem|<macro|which|body|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<right-flush><vspace|1fn>|<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>>
    <\theorem>
      This is a theorem which has been typeset in a slanted font.
    </theorem>

    <\remark>
      The rendering of remarks is based on the rendering of theorems, except
      that the bodies are typeset in an upright font.
    </remark>
  </with>

  Instead of redefining the entire rendering, the user might just wish to
  customize the way names of theorems are rendered or redefine the separator
  between the name and the body. As the user may have noticed by examining
  the above redefinition of <markup|render-theorem>, these aspects are
  controlled by the macros <markup|theorem-name> and <markup|theorem-sep>.
  For instance, consider the following redefinitions:

  <\tm-fragment>
    <inactive*|<assign|theorem-name|<macro|name|<with|color|dark
    red|font-series|bold|<arg|name>>>>>

    <inactive*|<assign|theorem-sep|<macro|: >>>
  </tm-fragment>

  Then theorem-like environments will be rendered as follows:

  <\with|theorem-name|<macro|name|<with|color|dark
  red|font-series|bold|<arg|name>>>|theorem-sep|<macro|: >>
    <\proposition>
      This proposition is rendered in is a fancy way.
    </proposition>
  </with>

  <paragraph|Customization of the numbering>

  \;

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>