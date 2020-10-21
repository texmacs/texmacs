<TeXmacs|1.0.7.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Traversal of a presentation>

  One major family of markup tags for presentations concerns the traversal of
  the document during a presentation. The keys <key|F10> and <key|F11> are
  used respectively for going back and forth in the presentation. The keys
  <key|F9> and <key|F12> are used to go to the start <abbr|resp.> end of the
  presentation. When using the <tmstyle|beamer> style or when enabling the
  ``presentation tool'' in the <menu|Tools> menu, a <menu|Dynamic> menu and
  additional icons will appear, which can also be used for the traversal of
  your presentation.

  The most basic traversal tag is called a ``switch'', and allows the user to
  show different pieces of text in successive and mutually exclusive manner.
  The entire presentation itself usually consists of a <markup|screens>
  switch, where the pieces are the successive ``slides'' of the presentation.
  After selection of the <tmstyle|beamer> style, this switch can be inserted
  using <menu|Focus|Screens> or <menu|Insert|Fold|Switch|Screens>. You may
  jump from one screen to another one using <key|pageup> and <key|pagedown>.

  Inside a switch, new ``branches'' can be inserted after or before the
  currently visible branch using <menu|Focus|Insert argument after> or
  <menu|Focus|Insert argument before>. Besides the <markup|screens> switch,
  you may use <menu|Insert|Fold|Switch|Standard> to insert paragraph-wide
  switches, and <menu|Insert|Fold|Switch|Tiny> to insert inline switches
  (similarly to displayed and inline formulas).

  Another popular way to traverse is presentation is to progressively unroll
  content. This can be done by inserting an <markup|unroll> tag using
  <menu|Insert|Fold|Unroll>. Using a ``hack'' this tag can be combined with
  the <markup|itemize> and <markup|enumerate> tags: first create the list
  environment, but remove the first (automatically inserted) <markup|item>
  tag. Next insert the unroll tag. When pressing <key|enter> inside the
  unroll tag, new items are created; you still have to use <menu|Focus|Insert
  argument after> for inserting new branches to the unroll structure (in
  particular, several items could be unrolled at once).

  A variant of unrolling is unfolding. This is basically an unroll tag with
  exactly two branches, but different variants are available in
  <menu|Insert|Fold|Folded> depending on the desired rendering. In
  particular, some of the renderings display a button which may be pushed in
  order to fold or unfold some content. The input-output fields inside
  computer algebra sessions are also foldable. Similarly, the tags in
  <menu|Insert|Fold|Summarize> are switches with two branches, again with
  different kinds of rendering.

  When using <TeXmacs> in combination with an external plug-in, such as a
  computer algebra system, you will notice that all input-output fields in
  <hlink|sessions|../interface/man-session-basic.en.tm> are foldable. In
  addition, you can create so called ``<hlink|executable
  switches|../interface/man-scripting-language.en.tm>'' using the items in
  the <menu|Insert|Fold|Executable> submenu. This allows you to switch back
  and forth between a given input to the system and the corresponding output.

  All markup for the traversal of presentations may be nested in a natural
  way. In the <menu|Insert|Fold|Traversal> menu, you may specify whether
  unrolled and folded structures should be folded back after traversal.

  <tmdoc-copyright|2010|Joris van der Hoeven>

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