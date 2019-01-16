<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Built-in environment variables>

  The way <TeXmacs> <hlink|typesets|../basics/typesetting.en.tm> documents is
  influenced by so called <em|environment variables>. The <hlink|style-sheet
  language|../stylesheet/stylesheet.en.tm> uses a so called <em|environment>
  (or context) to store both environment variables and
  <hlink|macros|../stylesheet/prim-macro.en.tm>. The environment variables
  are subdivided into two categories: built-in variables and additional
  variables provided by style files. Built-in variables usually affect the
  layout, while additional variables mostly serve computational purposes. In
  the next sections of this chapter, we will describe all built-in
  environment variables.

  A typical built-in environment variable is <src-var|color>. The value of an
  environment variable may be <hlink|changed|../stylesheet/prim-env.en.tm>
  permanently using <markup|assign> and temporarily using the <markup|with>
  primitive:

  <\tm-fragment>
    Some <with|color|dark red|colored> text.
  </tm-fragment>

  <\tm-fragment>
    <inactive*|Some <with|color|dark red|colored> text.>
  </tm-fragment>

  Counters are typical environment variables defined in style-sheets.

  <\tm-fragment>
    <\enumerate>
      <item>A weirdly

      <assign|item-nr|3><item>numbered list...
    </enumerate>
  </tm-fragment>

  <\tm-fragment>
    <inactive*|<\enumerate>
      <item>A weirdly

      <assign|item-nr|3><item>numbered list...
    </enumerate>>
  </tm-fragment>

  The typesetting language uses <def-index|dynamic scoping> of variables.
  That means that macros can access and modify variables in their calling
  context. In the previous example, the <markup|enumerate> macro locally
  initializes <src-var|item-nr> to <math|0> (uses <markup|with>) and the
  <markup|item> macro increments it by one and shows its value. Since
  <markup|enumerate> locally redefines <src-var|item-nr>, the original value
  of <src-var|item-nr> is restored on exit.

  Each document comes with an <hlink|initial
  environment|../basics/tm-docs.en.tm#init-env> with the initial values of
  environment values, <abbr|i.e.> their values just before we typeset the
  document. If an environment variable does not occur in the initial
  environment, then its initial value defaults to its value after typesetting
  the document style and possible additional packages. The initial
  environment before typesetting the style files and packages is built-in
  into the editor.

  Some variables, like header and footer variables, must be set inside the
  document, their initial environment value is ignored. Generally, they
  should be set by header and sectioning markup.

  <\traverse>
    <branch|General environment variables|env-general.en.tm>

    <branch|Specifying the current font|env-font.en.tm>

    <branch|Typesetting mathematics|env-math.en.tm>

    <branch|Paragraph layout|env-par.en.tm>

    <branch|Page layout|env-page.en.tm>

    <branch|Table layout|env-table.en.tm>

    <branch|Editing source trees|env-src.en.tm>

    <branch|Miscellaneous environment variables|env-misc.en.tm>
  </traverse>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>