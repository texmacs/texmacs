<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Documents are trees>

  <apply|TeXmacs> represents all texts by trees (for a fixed text, the
  corresponding tree is called the <expand|def-index|edit tree>). The inner
  nodes of such a tree are labeled by standard <expand|def-index|operators>
  of type <verbatim|tree_label> (see <verbatim|Basic/Data/tree.gen.h>). The
  labels of the leaves of the tree are strings, which are either invisible
  (such as lengths or macro definitions), or visible (the real text).
  <TeXmacs> trees can be written using different notations. For instance, the
  tree

  <\expand|quote>
    <with|mode|math|<tree|concat|x+y+|<tree|frac|1|2>|+|<tree|sqrt|y+z>>>
  </expand>

  represents the formula

  <\expand|tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </expand>

  and can also be written as

  <\expand|scheme-fragment>
    (concat

    \ \ "x+y"

    \ \ (frac "1" "2")

    \ \ "+"

    \ \ (sqrt "y+z"))
  </expand>

  in <value|scheme> notation.

  The meaning of the text and the way it is typeset essentially depends on
  the current environment. The environment mainly consists of a hash table
  which maps environment variables to their tree values. The current
  language, the current font and the current color are examples of system
  environment variables; new variables can be defined by the user. For
  instance, the <value|scheme> expression

  <\expand|scheme-fragment>
    (concat

    \ \ "Some "

    \ \ (with "color" "blue" "blue")

    \ \ " text.")
  </expand>

  represents the document fragment

  <\expand|tm-fragment>
    Some <with|color|blue|blue> text
  </expand>

  The <TeXmacs> primitive <verbatim|with> indicates a local change of an
  environment variable.

  In the sequel, we will describe in more detail the different stardard
  <apply|TeXmacs> operators and environment variables. It should be noticed
  that the <apply|TeXmacs> data format is still subject to change. In the
  last section we will describe these changes. Usually, the changes will not
  be noticed by the user, since they are always accompanied by conversion
  programs which automatically update to the new format. However, they are
  sometimes important for developers, although most changes just concern the
  addition of new primitives.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|edit tree>|<pageref|idx-1>>

      <tuple|<tuple|operators>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
