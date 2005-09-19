<TeXmacs|1.0.5.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Overview of the <value|scheme> extension language>

  One major characteristic of <TeXmacs> is the possibility to extend the
  editor using the <name|Guile>-<value|scheme> <em|extension language>. Such
  extensions can be simple, like a personal boot file containing frequently
  used keyboard shortcuts, or more complex, like a plug-in with special
  editing routines for a particular type of documents. The <value|scheme>
  language can also be used interactively from within the editor or invoked
  by special markup like ``actions''.

  In this chapter, we give an overview of why and how to use <value|scheme>
  from within <TeXmacs>. The first sections provide sufficient information
  for someone who wants to program some basic customization of the keyboard
  and menus. The latter sections give an introduction to the general
  architecture of the <value|scheme> API and some important features and
  particularities of way <value|scheme> is used within <TeXmacs>. The reading
  of the overview is highly recommended to anyone who wants to make
  non-trivial use of <value|scheme> inside <TeXmacs>.

  More complete documentation about the <value|scheme> modules provided by
  <TeXmacs> is available from the <menu|Help|Scheme extensions> menu. We also
  recommend the following on-line manuals about <value|scheme> and its
  <name|Guile> implementation:

  <\itemize-minus>
    <item><hlink|The <value|scheme> programming
    language|http://www.scheme.com/tspl2d/index.html>.

    <item><hlink|Guile reference manual|http://latakia.dyndns.org/~ruhl/guile-doc/guile_toc.html>.
  </itemize-minus>

  For further information about <value|scheme>, we refer to
  <hlink|<verbatim|http://www.schemers.org>|http://www.schemers.org>. As a
  general rule, we also encourage users to take a look at the <TeXmacs>
  source code for concrete examples on how to use <value|scheme> from within
  <TeXmacs>.

  <\traverse>
    <branch|Why <TeXmacs> uses <value|scheme> as its extension
    language|overview-why.en.tm>

    <branch|When and how to use <value|scheme>|overview-start.en.tm>

    <branch|General architecture of the <value|scheme>
    API|overview-architecture.en.tm>

    <branch|The module system and lazy definitions|overview-lazyness.en.tm>

    <branch|Contextual overloading|overview-overloading.en.tm>

    <branch|Meta information and logical programming|overview-meta.en.tm>

    <branch|The <TeXmacs> content model|overview-content.en.tm>

    <branch|Standard utilities|overview-utilities.en.tm>
  </traverse>

  <tmdoc-copyright|2005|Joris van der Hoeven>

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