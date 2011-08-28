<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Local customization>

  Even though <TeXmacs> tries hard to render source code in a nice way
  following the global rendering options that you specified, the readability
  of the source code often needs to be further enhanced locally. In source
  mode, this can be done using the menus <menu|Source|Activation> and
  <menu|Source|Presentation>. Any local hints on how to render source code
  are automatically removed from the document when it is being used as a
  style file or package.

  First of all, for certain pieces of content the user may prefer to see them
  in their ``activated'' form instead as dead source code. This may for
  instance be the case for embedded images, or for mathematical symbols, like
  in

  <\tm-fragment>
    <with|mode|math|<inactive*|<assign|R|<macro|<active*|\<bbb-R\>>>>>>
  </tm-fragment>

  Such an active presentation may also be preferred for certain more complex
  macros:

  <\tm-fragment>
    <with|mode|math|<inactive*|<assign|diag|<macro|var|dim|<active*|<matrix|<tformat|<table|<row|<cell|<inactive*|<arg|var>><rsub|1>>|<cell|>|<cell|\<b-0\>>>|<row|<cell|>|<cell|\<ddots\>>|<cell|>>|<row|<cell|\<b-0\>>|<cell|>|<cell|<inactive*|<arg|var>><rsub|<inactive*|<arg|dim>>>>>>>>>>>>>
  </tm-fragment>

  A piece of code can be activated by selecting it and using
  <menu|Source|Activation|Activate> or <shortcut|(make-mod-active 'active*)>. Similarly, a piece of
  content may be deactivated using <key|<group|M->-> (we used this in the
  second example above for the rendering of the arguments <src-arg|var> and
  <src-arg|dim>). Activation and deactivation either apply to the whole tree,
  or to the root only (<abbr|e.g.> <menu|Source|Activation|Activate once>).

  Another way to customize the rendering is to override some of the global
  rendering options. This is mainly interesting for controlling more
  precisely which tags have to be stretched across several lines and which
  tags have to be represented in a compact fashion. For instance, the
  <markup|concat> tag can be used in order to concatenate content, as well as
  for specifying a block of sequential statements, or a combination of both.
  For instance, in the piece of code

  <\tm-fragment>
    <inactive*|<assign|my-section|<macro|title|<style-with|src-compact|none|<header-hook|<arg|title>><toc-hook|<arg|title>><my-section-title|<arg|title>>>>>>
  </tm-fragment>

  we have stretched the <markup|concat> tag along several lines using
  <menu|Source|Presentation|Stretched> (notice that this implies the
  <markup|concat> tag to appear explicitly, so as to avoid confusion with the
  <markup|document> tag). Similarly, if a part of the concatenation were to
  be displayed as usual, then one may use <menu|Source|Presentation|Compact>:

  <\tm-fragment>
    <inactive*|<assign|my-section|<macro|title|<style-with|src-compact|none|<header-hook|<arg|title>><toc-hook|<arg|title>><style-with|src-compact|all|<with|font-series|bold|Section:>
    <arg|title>>>>>>
  </tm-fragment>

  At present, we did not implement a way to mark arguments as inline or
  block, but we might do this later.

  A final way to customize the rendering of source code is to apply an
  arbitrary macro using <menu|Source|Presentation|Apply macro> or
  <menu|Source|Presentation|Apply macro once>. This macro will be
  automatically removed when you use your document as a style file or
  package.

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