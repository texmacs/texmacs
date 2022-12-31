<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english>>

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

  <paragraph*|Defining new environments>

  First of all, new environments can be added using the meta-macros
  <markup|new-theorem>, <markup|new-remark> and <markup|new-exercise>. These
  environments take two arguments: the name of the environment and the name
  which is used for its rendering. For instance, you may wish to define the
  environment <markup|experiment> by

  <\tm-fragment>
    <inactive*|<new-theorem|experiment|Experiment>>
  </tm-fragment>

  When available in the <TeXmacs> dictionaries, the text \PExperiment\Q will
  be automatically translated when your document is written in a foreign
  language. In the section about <hlink|how to define new
  environments|../../../main/styles/env/env-base-dtd.en.tm>, it is also
  explained how to define other numbered textual environments (besides
  theorems, remarks and exercises).

  <paragraph*|Customization of the rendering>

  The principal rendering of the environments can be customized by redefining
  the <markup|render-theorem>, <markup|render-remark> and
  <markup|render-exercise> macros. These macros take the <src-arg|name> of
  the environment (like \PTheorem <no-break>1.2\Q) and its <src-arg|body> as
  arguments. For instance, if you want theorems to appear in a slightly
  indented way, with a slanted body, then you may redefine
  <markup|render-theorem> as follows:

  <\tm-fragment>
    <inactive*|<assign|render-theorem|<\macro|which|body>
      <padded-normal|1fn|1fn|<surround|<theorem-name|<arg|which><theorem-sep>>||<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>
    </macro>>>
  </tm-fragment>

  This redefinition produces the following effect:

  <\with|render-theorem|<\macro|which|body>
    <padded-normal|1fn|1fn|<surround|<theorem-name|<arg|which><theorem-sep>>||<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>
  </macro>>
    <\theorem>
      This is a theorem which has been typeset in a slanted font.
    </theorem>
  </with>

  By default, the theorems are rendered as remarks with the only difference
  that their bodies are typeset in an italic font. Hence, redefining the
  <markup|render-remark> macro will also affect the rendering of theorems.
  The default <markup|render-proof> macro is also based on
  <markup|render-remark>.

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

  <paragraph*|Customization of the numbering>

  In the sections about <hlink|counters and counter
  groups|../../../main/styles/std/std-counter-dtd.en.tm>, it is explained how
  to customize the counters of numbered environments for particular purposes.
  For instance, by redefining <markup|inc-theorem>, you may force theorems to
  reset the counter of corollaries:

  <\tm-fragment>
    <inactive*|<style-with|src-compact|none|<quasi|<style-with|src-compact|none|<assign|inc-theorem|<macro|<compound|<unquote|<value|inc-theorem>>><reset-corollary>>>>>>>
  </tm-fragment>

  Notice the trick with <markup|quasi> and <markup|unquote> in order to take
  into account additional action which might have been undertaken by the
  previous value of the macro <markup|inc-theorem>.

  The following code from <verbatim|number-long-article.ts> is used in order
  to prefix all standard environments with the number of the current section:

  <\tm-fragment>
    <inactive*|<assign|section-clean|<macro|<reset-subsection><reset-std-env>>>>

    <inactive*|<assign|display-std-env|<macro|nr|<section-prefix><arg|nr>>>>
  </tm-fragment>

  <tmdoc-copyright|1998\U2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>