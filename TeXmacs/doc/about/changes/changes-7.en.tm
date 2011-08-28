<TeXmacs|1.0.2.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Macro expansion (1.0.2.3 -- 1.0.2.7)>

  An important internal change concerning the data format has been made:
  macro expansions and function applications like

  <\scheme-fragment>
    (expand <em|tag> <em|arg-1> ... <em|arg-n>)
  </scheme-fragment>

  <\scheme-fragment>
    (apply <em|tag> <em|arg-1> ... <em|arg-n>)
  </scheme-fragment>

  are now replaced by hard-coded tags

  <\scheme-fragment>
    (<em|tag> <em|arg-1> ... <em|arg-n>)
  </scheme-fragment>

  Moreover, functions have systematically been replaced by macros. The few
  built-in functions which may take an arbitrary number of arguments have
  been rewritten using the new <markup|xmacro> construct. If you ever wrote
  such a function yourself, then you will need to rewrite it too.

  The new approach favorites a uniform treatment of macros and functions and
  makes the internal representation match with the corresponding
  <name|Scheme> representation. More and more information about tags will
  gradually be stored in the <abbr|D.R.D.> (Data Relation Definition). This
  information is mostly determined automatically using heuristics.

  Notice that some perverse errors might arise because of the above changes.
  Please keep copies of your old files and report any suspicious behaviour to
  us.

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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>