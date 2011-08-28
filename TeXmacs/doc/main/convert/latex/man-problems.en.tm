<TeXmacs|1.0.5.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Limitations of the current <LaTeX> converters>

  <paragraph*|Limitations of the <TeXmacs> to <LaTeX> converter>

  Some of the <TeXmacs> primitives have no analogues in <LaTeX>. When
  converting such primitives from <TeXmacs> into <LaTeX>, they will usually
  be either ignored or replaced by an approximative translation. A (probably
  incomplete) list of <TeXmacs> features with no <LaTeX> counterparts is as
  follows:

  <\itemize>
    <item>Left primes.

    <item>Big separators between big parentheses.

    <item>Trees.

    <item>Certain features of tables (background color, cell span, vertical
    alignment, <abbr|etc.>).

    <item>Complex user macros.

    <item>Vertical spaces ``before'' and ``after''.

    <item>Indentation flags ``before'' and ``after''.

    <item>Most types of interactive content: hyperlinks, actions, sessions,
    tags for the presentation mode, animations and sounds, etc.
  </itemize>

  In addition, several issues are only partially implemented:

  <\itemize>
    <item>Non standard fonts.

    <item>Certain table properties

    <item>Style parameters.
  </itemize>

  Of course, there are also differences between the typesetting algorithms
  used by <TeXmacs> and <TeX>/<LaTeX>, so the <TeXmacs> to <LaTeX> is not
  intended to be <em|wysiwyg>.

  <paragraph*|Limitations of the <LaTeX> to <TeXmacs> converter>

  As explained in the introduction, the conversion of <LaTeX> documents into
  <TeXmacs> is more problematic than conversions the other way around. Only a
  subset of <LaTeX> can be converted to <TeXmacs> in a fully reliable way.
  This subset comprises virtually all common constructs, including macro
  definitions and the additional macros uses by the <TeXmacs> to <LaTeX>
  converter. However, the converter has no knowledge about style parameters.
  In particular, it cannot be used for the conversion of <LaTeX> style files.

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

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
  </collection>
</initial>