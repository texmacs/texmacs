<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Typing mathematical symbols>

  The Greek characters are obtained in <TeXmacs> using the
  <prefix|math:greek>-key. For instance, <key|math:greek a>
  yields<nbsp><math|\<alpha\>> and <key|math:greek G> yields
  <math|\<Gamma\>>. Similarly, <prefix|math:bold>, <prefix|math:cal>,
  <prefix|math:frak> and <prefix|math:bbb> can be used in order to type bold,
  calligraphic, fraktur and blackboard bold characters. For instance,
  <shortcut|\<frak-m\>> yields<nbsp><math|\<frak-m\>>, <key|S-F6 R>
  yields<nbsp><math|\<bbb-R\>> and <shortcut|\<b-cal-Z\>> yields
  <math|\<b-cal-Z\>>.

  Greek characters can also be obtained as \Pvariants\Q of Latin characters
  using the <key|var>-key. For instance, <key|p var> yields <math|\<pi\>>.
  The <key|var>-key is also used for obtaining variants of the Greek letters
  themselves. For instance, both <key|math:greek p var> and <key|p var var>
  yield <math|\<varpi\>>. An alternative way to enter blackboard bold
  characters is to type the same capital twice. For instance, <key|Z Z>
  yields<nbsp><math|\<bbb-Z\>>.

  Some symbols admit many variants. For instance, <key|\<less\>> yields
  <math|\<less\>>, <key|\<less\> var> yields <math|\<in\>>, <key|\<less\> var
  var> yields <math|\<subset\>>, <key|\<less\> var var var> yields
  <math|\<prec\>>, and so on. You may \Pcycle back\Q among the variants
  using<nbsp><key|S-var>. For instance, <key|\<less\> var var S-var> is
  equivalent to <key|\<less\> var>.

  Many other mathematical symbols are obtained by \Pnatural\Q
  key-combinations. For instance, <key|- \<gtr\>> yields
  <math|<op|\<rightarrow\>>>, <key|- - \<gtr\>> yields
  <math|<op|\<longrightarrow\>>> and <key|\<gtr\> => yields
  <math|<op|\<geqslant\>>>. Similarly, <key|\| var -> yields
  <math|<op|\<vdash\>>>, <key|\| - \<gtr\>> yields <math|<op|\<mapsto\>>> and
  <key|- \<gtr\> \<less\> -> yields <math|<op|\<rightleftarrows\>>>. The
  following general rules hold in order to enter mathematical symbols:

  <\description>
    <item*|<key|tab>>is the main key for obtaining variants. For instance,
    <key|\<gtr\> => yields <math|<op|\<geqslant\>>>, but <key|\<gtr\> = var>
    yields<nbsp><math|<op|\<geq\>>>. Similarly, <key|\<less\> var var> yields
    <math|<op|\<subset\>>>, <key|\<less\> var var => yields
    <math|<op|\<subseteq\>>> and <key|\<less\> var var = var> yields
    <math|<op|\<subseteqq\>>>. Also, <key|P var> yields<nbsp><math|\<wp\>>
    and <key|e var> yields the constant <math|\<mathe\>=exp<around|(|1|)>>.

    <item*|<key|@>>is used for putting symbols into circles or boxes. For
    instance, <key|@ +> yields <math|<op|\<oplus\>>> and <key|@ x> yields
    <math|<op|\<otimes\>>>. Similarly, <key|@ var +> yields
    <math|<op|\<boxplus\>>>.

    <item*|<key|/>>is used for negations. For instance, <key|= /> yields
    <math|<op|\<neq\>>> and <key|\<less\> = /> yields
    <math|<neg|\<leqslant\>>>. Notice that <key|\<less\> = var var /> yields
    <math|<op|\<nleqq\>>>, while <key|\<less\> = var var / var> yields
    <math|<op|\<lneqq\>>>.

    <item*|<key|!>>is used after arrows in order to force scripts to be
    placed above or below the arrow. For instance, <key|- - \<gtr\> ^ x>
    yields <math|<op|\<longrightarrow\><rsup|x> >>, but <key|- - \<gtr\> ! ^
    x> yields <math|\<longrightarrowlim\><rsup|x>>.
  </description>

  The logical relations <math|\<wedge\>> and <math|\<vee\>> are obtained
  using <key|&> and <key|%>. The operators <math|\<cap\>> and <math|\<cup\>>
  are natural variants <key|& var> and <key|% var>. Various miscellaneous
  symbols can be obtained using the <prefix|math:symbol> prefix.

  Notice that certain symbols with a different mathematical meaning are
  sometimes denoted in a<nbsp>similar way; such symbols are called
  <em|homoglyphs>. For instance, the vertical bar <math|\|> can be used as
  a<nbsp>separator for defining sets <math|R<rsup|\<gtr\>>=<around*|{|x\<in\>R\|x\<gtr\>0|}>>,
  but also as the binary relation \Pdivides\Q<nbsp><rigid|<math|11\<divides\>1001>>.
  Often, but not always, homoglyphs admit a different spacing. The most
  annoying ambiguity is between invisible multiplication <math|x*y> and
  function application <math|sin x>, which are entered using the shortcuts
  <key|*> <abbr|resp.> <key|space>.

  In order to facilitate certain automated treatments of your documents, such
  as mathematical syntax checking, we incite authors to pay attention to the
  homoglyph problem when entering formulas. For more information on this
  issue and how <TeXmacs> can assist you to use the appropriate notations, we
  refer to our section on the <hlink|semantics of mathematical
  symbols|../semantics/man-semantics-symbols.en.tm>.

  <tmdoc-copyright|1998--2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>