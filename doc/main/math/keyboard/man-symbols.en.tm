<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Typing mathematical symbols>

  The Greek characters are obtained in <apply|TeXmacs> by combining the hyper
  modifier key <key|H-> with a letter. For instance, <key|H-a> yields
  <with|mode|math|\<alpha\>> and <key|H-G> yields <with|mode|math|\<Gamma\>>.
  <apply|hyper-link|Recall|../../start/man-conventions.en.tm> that the
  <key|F5> key is equivalent to <key|H->, so that <with|mode|math|\<rho\>>
  can also be obtained by typing <key|F5 r>. Similarly, <key|F6>, <key|F7>,
  <key|F8> and <key|S-F6> can be used in order to type bold, calligraphic,
  fraktur and blackboard bold characters. For instance, <key|F8 m> yields
  <with|mode|math|\<frak-m\>>, <key|S-F6 R> yields <format|no line
  break><with|mode|math|\<bbb-R\>> and <key|F6 F7 Z> yields
  <with|mode|math|\<b-cal-Z\>>.

  Greek characters can also be obtained as ``variants'' of Latin characters
  using the <key|<key-variant>>-key. For instance, <key|p
  <key-variant>> yields <with|mode|math|\<pi\>>. The
  <key|<key-variant>>-key is also used for obtaining variants of the
  Greek letters themselves. For instance, both <key|H-p <key-variant>>
  and <key|p <key-variant> <key-variant>> yield
  <with|mode|math|\<varpi\>>.

  Many other mathematical symbols are obtained by ``natural''
  key-combinations. For instance, <key|- \<gtr\>> yields
  <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>> yields
  <with|mode|math|\<longrightarrow\>> and <key|\<gtr\> => yields
  <with|mode|math|\<geqslant\>>. Similarly, <key|\| -> yields
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> yields
  <with|mode|math|\<mapsto\>> and <key|- \<gtr\> \<less\> -> yields
  <with|mode|math|\<rightleftarrows\>>. Some general rules hold in order to
  obtain variants of symbols:

  <\description>
    <expand|item*|<key|tab>>is the main key for obtaining variants. For
    instance, <key|\<gtr\> => yields <with|mode|math|\<geqslant\>>, but
    <key|\<gtr\> = <key-variant>> yields <format|no line
    break><with|mode|math|\<geq\>>. Similarly, <key|\<less\>
    <key-variant>> yields <with|mode|math|\<prec\>>, <key|\<less\>
    <key-variant> => yields <with|mode|math|\<preccurlyeq\>> and
    <key|\<less\> <key-variant> = <key-variant>> yields
    <with|mode|math|\<preceq\>>. Also, <key|P <key-variant>> yields
    <with|mode|math|\<wp\>> and <key|e <key-variant>> yields the
    constant <with|mode|math|\<mathe\>=exp(1)>. You may ``cycle back'' using
    <key|S-<key-variant>>.

    <expand|item*|<key|@>>is used for putting symbols into circles or boxes.
    For instance, <key|@ +> yields <with|mode|math|\<oplus\>> and <key|@ x>
    yields <with|mode|math|\<otimes\>>. Similarly, <key|@
    <key-variant> +> yields <with|mode|math|\<boxplus\>>.

    <expand|item*|<key|/>>is used for negations. For instance, <key|= />
    yields <with|mode|math|\<neq\>> and <key|\<less\> = /> yields
    <with|mode|math|<neg|\<leqslant\>>>. Notice that <key|\<less\> =
    <key-variant> <key-variant> /> yields
    <with|mode|math|\<nleqq\>>, while <key|\<less\> = <key-variant>
    <key-variant> / <key-variant>> yields
    <with|mode|math|\<lneqq\>>.

    <expand|item*|<key|!>>is used after arrows in order to force scripts to
    be placed above or below the arrow. For instance, <key|- - \<gtr\> ^ x>
    yields <with|mode|math|\<longrightarrow\><rsup|x> >, but <key|- - \<gtr\>
    ! ^ x> yields <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Several other symbols which cannot be entered naturally in the above way
  are obtained using the <key|S-F5> prefix. Here follows a short table of
  such symbols:

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|1|-1|2|2|cell
  halign|c>|<cwith|1|-1|4|4|cell halign|c>|<cwith|1|-1|2|2|cell
  rborder|1ln>|<table|<row|<cell|Shortcut>|<cell|Symbol>|<cell|Shortcut>|<cell|Symbol>>|<row|<cell|<expand|kbd-symb|a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<expand|kbd-symb|n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<expand|kbd-symb|u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<expand|kbd-symb|v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<expand|kbd-symb|w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Some
  symbols which cannot be obtained using general rules in a natural way.>

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
    <associate|gly-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Some symbols which cannot be obtained using general rules
      in a natural way.|<pageref|gly-1>>
    </associate>
  </collection>
</auxiliary>
