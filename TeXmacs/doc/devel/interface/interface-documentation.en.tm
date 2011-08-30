<TeXmacs|1.0.7.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Writing documentation>

  Documentation for your plug-in <verbatim|myplugin> should be put in the
  <verbatim|doc> subdirectory of the main directory <verbatim|myplugin>. We
  recommend to write at least the following three documentation files:

  <\description-long>
    <item*|<verbatim|myplugin.en.tm>>This file should mainly contain a
    <markup|traverse> tag with links to the other documentation files, as
    described in the section ``<hlink|traversing the <TeXmacs>
    documentation|$TEXMACS_DOC_PATH/about/contribute/documentation/traversal.en.tm>''.

    <item*|<verbatim|myplugin-abstract.en.tm>>This file should contain a
    short description of the purpose of the plugin-in. If appropriate, then
    you should also describe how to get the plug-in and how to install it.
    The contents of this file should also be suitable for publication on the
    web site of <TeXmacs>.

    <item*|<verbatim|myplugin-demo.en.tm>>This file should contain a short
    demonstration of your plug-in, such as an example session.
  </description-long>

  The first two files are mandatory, if you want your plug-in to show up in
  the <menu|Help|Plug-ins> menu. Please refrain from putting too many images
  in the documentation files, so as to keep the size of the documentation
  reasonable when integrated into the main <TeXmacs> distribution.

  <tmdoc-copyright|2011|Joris van der Hoeven>

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