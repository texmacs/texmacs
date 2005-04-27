<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|General principles for customization>

  Style files and packages basically enrich the current typesetting
  environment with a combination of

  <\itemize>
    <item>Environment variables.

    <item>Tags for the end-user.

    <item>Customizable macros.
  </itemize>

  Furthermore, they may define some tags for intern implementation purposes,
  which will not be documented in this manual. They may also specify some
  logical properties of tags using the <markup|drd-props> primitive.

  Environment variables are almost always attributes for controlling the
  rendering of content, or counters for sections, equations, <abbr|etc.>.
  Although several simple tags for the end-user like <markup|strong> may be
  redefined in your own style files, this practice is not recommended for
  more complex tags like <markup|section>. Indeed, the <markup|section> tag
  involves many things like resetting subcounters, entering the title into
  the table of contents and so on. Therefore, special additional macros are
  provided the customization of such tags, like <markup|section-title>,
  <markup|section-clean> and <markup|section-toc>.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>