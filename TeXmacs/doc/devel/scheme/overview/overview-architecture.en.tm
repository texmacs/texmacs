<TeXmacs|1.0.6.10>

<style|tmdoc>

<\body>
  <tmdoc-title|General architecture of the <value|scheme> API>

  When programming <value|scheme> extensions of <TeXmacs>, it may be useful
  to be conscious of the internal architecture of the <value|scheme> modules
  inside <TeXmacs> (see figure <reference|scheme-api-fig>).

  <big-figure|<with|gr-edit-grid-aspect|<tuple|<tuple|axes|none>|<tuple|1|none>|<tuple|4|none>>|gr-mode|<tuple|edit|cline>|gr-fill-color|black|gr-line-width|1ln|<graphics||<with|fill-color|grey|<cline|<point|-6.5|-0.5>|<point|6.5|-0.5>|<point|6.5|-4>|<point|-6.5|-4>>>|<with|fill-color|pastel
  grey|<cline|<point|-4|-2>|<point|4|-2>|<point|4|-1>|<point|-4|-1>>>|<with|fill-color|pastel
  grey|<cline|<point|-6|-1>|<point|-4.5|-1>|<point|-4.5|-2.5>|<point|-0.25|-2.5>|<point|-0.25|-3.5>|<point|-6|-3.5>>>|<with|fill-color|pastel
  grey|<cline|<point|0.25|-2.5>|<point|4.5|-2.5>|<point|4.5|-1>|<point|6|-1>|<point|6|-3.5>|<point|0.25|-3.5>>>|<with|text-at-halign|center|text-at-valign|center|<text-at|<value|scheme>
  routines from glue|<point|3.25|-3>>>|<with|text-at-halign|center|text-at-valign|center|<text-at|Standard
  <value|scheme> language|<point|-3.25|-3>>>|<with|<line|<point|-2.25|-2>>>|<with|<line|<point|-2.25|-2>>>|<with|fill-color|black|color|none|<cline|<point|-2.5|-2.25>|<point|-2.25|-2>|<point|-2|-2.25>>>|<with|fill-color|black|color|none|<cline|<point|1.75|-2.25>|<point|2|-2>|<point|2.25|-2.25>>>|<with|fill-color|black|color|none|<cline|<point|-0.25|-0.75>|<point|0|-0.5>|<point|0.25|-0.75>>>|<with|fill-color|black|color|none|<cline|<point|-5.5|-0.75>|<point|-5.25|-0.5>|<point|-5|-0.75>>>|<with|fill-color|black|color|none|<cline|<point|5|-0.75>|<point|5.25|-0.5>|<point|5.5|-0.75>>>|<with|line-width|2ln|<line|<point|-2.25|-2.5>|<point|-2.25|-2.25>>>|<with|line-width|2ln|<line|<point|2|-2.5>|<point|2|-2.25>>>|<with|line-width|2ln|<line|<point|0|-1>|<point|0|-0.75>>>|<with|line-width|2ln|<line|<point|-5.25|-1>|<point|-5.25|-0.75>>>|<with|line-width|2ln|<line|<point|5.25|-1>|<point|5.25|-0.75>>>|<with|fill-color|pastel
  grey|<cline|<point|-6.5|0>|<point|6.5|0>|<point|6.5|-0.5>|<point|-6.5|-0.5>>>|<with|fill-color|grey|<cline|<point|-6.5|1>|<point|-6.5|3.5>|<point|-0.5|3.5>|<point|-0.5|1>>>|<with|fill-color|grey|<cline|<point|0.5|3.5>|<point|6.5|3.5>|<point|6.5|1>|<point|0.5|1>>>|<with|fill-color|pastel
  grey|<cline|<point|-6|2.5>|<point|-6|1.5>|<point|-4.5|1.5>|<point|-4.5|2.5>>>|<with|fill-color|pastel
  grey|<cline|<point|-4.25|1.5>|<point|-2.75|1.5>|<point|-2.75|2.5>|<point|-4.25|2.5>>>|<with|fill-color|pastel
  grey|<cline|<point|-2.5|1.5>|<point|-1|1.5>|<point|-1|2.5>|<point|-2.5|2.5>>>|<with|fill-color|pastel
  grey|<cline|<point|1|2.5>|<point|1|1.5>|<point|2.5|1.5>|<point|2.5|2.5>>>|<with|fill-color|pastel
  grey|<cline|<point|2.75|2.5>|<point|2.75|1.5>|<point|4.25|1.5>|<point|4.25|2.5>>>|<with|fill-color|pastel
  grey|<cline|<point|4.5|1.5>|<point|6|1.5>|<point|6|2.5>|<point|4.5|2.5>>>|<with|text-at-halign|center|text-at-valign|center|<text-at|Plug-ins|<point|3.5|3>>>|<with|text-at-halign|center|text-at-valign|center|<text-at|Internal
  modules|<point|-3.5|3>>>|<with|line-width|2ln|<line|<point|-3.5|0>|<point|-3.5|1>>>|<with|line-width|2ln|<line|<point|3.5|0>|<point|3.5|1>>>|<with|line-width|2ln|<line|<point|-0.5|2.25>|<point|0.5|2.25>>>|<with|fill-color|black|color|none|<cline|<point|-3.75|0.75>|<point|-3.5|1>|<point|-3.25|0.75>>>|<with|fill-color|black|color|none|<cline|<point|3.25|0.75>|<point|3.5|1>|<point|3.75|0.75>>>|<with|fill-color|black|color|none|<cline|<point|0.25|2.5>|<point|0.5|2.25>|<point|0.25|2>>>|<with|text-at-halign|center|text-at-valign|center|<text-at|Language
  extensions, utilities and libraries|<point|0|-1.5>>>>>|<label|scheme-api-fig>Schematic
  organization of the <value|scheme> API.>

  <paragraph*|Built-in <value|scheme> commands>

  On the very basic level, one has the standard <value|scheme> language, with
  some enhancements by the <name|Guile> implementation (these extensions are
  used as least as possible, for future portability). The standard
  <value|scheme> language is enriched by some routines implemented in the C++
  part of <TeXmacs> and exported to <value|scheme> via the glue. If you
  unpacked the source code of <TeXmacs> in <verbatim|<em|source-dir>>, then
  you can find a full list of the routines exported by the glue in the files

  <verbatim| \ \ \ <em|source-dir>/src/Guile/Glue/build-glue-base.scm<new-line>
  \ \ \ <em|source-dir>/src/Guile/Glue/build-glue-editor.scm<new-line>
  \ \ \ <em|source-dir>/src/Guile/Glue/build-glue-server.scm>

  <paragraph*|Extensions to <value|scheme> and further utilities>

  Above the standard <value|scheme> language and the extra routines from the
  glue, <TeXmacs> comes with a second level of language extensions, utilities
  and libraries. The corresponding <value|scheme> files can be found in the
  directories

  <verbatim| \ \ \ $TEXMACS_PATH/progs/kernel<new-line>
  \ \ \ $TEXMACS_PATH/progs/utils>

  Roughly speaking, the functionality provided by this second level is the
  following:

  <\itemize>
    <item>A certain number of frequently used
    <hlink|abbreviations|../utils/utils-abbrevs.en.tm>, like <scm|==> for
    <scm|equal?>.

    <item>General language extensions for <hlink|contextual
    overloading|overview-overloading.en.tm>, <hlink|logical
    programming|overview-meta.en.tm>, <abbr|etc.>

    <item><TeXmacs>-specific language extensions for the definition of
    <hlink|menus|../utils/utils-menus.en.tm>, <hlink|keyboard
    shortcuts|../utils/utils-keyboard.en.tm>, <abbr|etc.>

    <item>Additional routines for <hlink|<TeXmacs> content
    manipulation|overview-content.en.tm> and pattern matching.

    <item>Further utilities and libraries for common types like strings and
    lists.
  </itemize>

  Whereas the modules in <verbatim|$TEXMACS_PATH/progs/kernel> are
  automatically loaded, all modules in <verbatim|$TEXMACS_PATH/progs/utils>
  have to be explicitly included.

  <paragraph*|Internal modules and plug-ins>

  The remaining <value|scheme> extensions of <TeXmacs> are regrouped into
  <em|internal modules> which usually correspond to a particular type of
  content. For instance, the directories

  <verbatim| \ \ \ $TEXMACS_PATH/progs/source<new-line>
  \ \ \ $TEXMACS_PATH/progs/math<new-line> \ \ \ $TEXMACS_PATH/progs/table>

  respectively contain routines for editing source code, mathematics and
  tables. Exceptions are the internal modules <verbatim|content> and
  <verbatim|fonts>, which rather correspond to a particular type of
  functionality. Each internal module corresponds to a group of files, each
  of which corresponds to an individual <em|<TeXmacs> module>. The internal
  modules are designed to be as independent as possible.

  From the <value|scheme> point of view, the structure of a plug-in is very
  similar to that of an internal module. Each plug-in defines a collection of
  <value|scheme> programs in its <verbatim|progs> subdirectory. Although
  distinct plug-ins may in principle depend on each other, they are usually
  designed in a way which makes them as independent as possible.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>