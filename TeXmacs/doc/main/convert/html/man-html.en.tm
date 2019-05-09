<TeXmacs|1.99.9>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Converters for <name|Html> and <name|MathML>>

  <paragraph|<name|Html generation>>

  <TeXmacs> supports reasonably good converters to <name|Html> and
  <name|MathML>. A document can be exported to <name|Html> using
  <menu|File|Export|Html>. <TeXmacs> makes moderate use of <name|Css> in
  order to improve the presentation of the generated <name|Html>.

  By default, <TeXmacs> does its best in order to render formulas using
  existing <name|Html>/<name|Css> primitives. When selecting
  <menu|Edit|Preferences|Converters|TeXmacs-\<gtr\>Html|Use MathML>, all
  formulas will be exported as <name|MathML>. Notice that this requires you
  to save the generated documents using the <verbatim|.xhtml> extension.

  Similarly, the user may force <TeXmacs> to export all mathematical formulas
  as images using <menu|Edit|Preferences|Converters|TeXmacs-\<gtr\>Html|Export
  formulas as images>. If your destination file is called
  <verbatim|name.html>, then the images are stored in the same directory in
  files <verbatim|name-1.png>, <rigid|<verbatim|name-2.png>> and so on. Even
  when formulas are not exported as images, notice that all graphics drawn
  using <TeXmacs> are exported in this way. In particular, the exportation of
  a <TeXmacs> file with pictures may give rise to the creation of additional
  image files. You may also force arbitrary content to be exported as an
  image using <menu|Format|Specific|Image>.

  <TeXmacs> also provides a facility for the creation of entire websites. For
  this, you just have to regroup the files for your website into a single
  directory. Using <menu|Tools|Web|Create website> you may now convert all
  <TeXmacs> files in this directory to <name|Html> files in a new directory.
  The conversion procedure recursively traverses all subdirectories and all
  non-<TeXmacs> files are simply copied.

  <paragraph|Customized <name|Html> generation>

  The following <TeXmacs> environment variables can be used to customize the
  <name|Html> generation:

  <\description>
    <item*|<src-var|html-title>>The title of your exported document.

    <item*|<src-var|html-css>>A cascaded style sheet for your exported
    document.

    <item*|<src-var|html-head-javascript-src>>An external <name|Javascript>
    file to be executed before the body.

    <item*|<src-var|html-head-javascript>>A <name|Javascript> script to be
    executed before the body.
  </description>

  You may also use the following macros:

  <\indent>
    <\explain>
      <explain-macro|html-class|class|body>

      <explain-macro|html-div-class|class|body>
    <|explain>
      Associate a CSS class to the content <src-arg|body>, optionally inside
      a separate <verbatim|div> tag.
    </explain>

    <\explain>
      <explain-macro|html-style|style|body>

      <explain-macro|html-div-style|class|body>
    <|explain>
      Associate a CSS style to the content <src-arg|body>, optionally inside
      a separate <verbatim|div> tag.
    </explain>

    <\explain|<explain-macro|html-javascript-src|src>>
      Execute a <name|Javascript> script from the file <src-arg|src>.
    </explain>

    <\explain|<explain-macro|html-javascript|code>>
      Execute the <name|Javascript> script <compound|src-arg|code>.
    </explain>
  </indent>

  In addition, given a macro <markup|my-tag>, you may customize the rendering
  of the tag when exporting to <name|Html> by defining a macro
  <markup|tmhtml-my-tag> with the same number of arguments. For instance, by
  putting the declaration

  <\tm-fragment>
    <inactive*|<assign|tmhtml-strong|<macro|body|<with|color|red|font-series|bold|<arg|body>>>>>
  </tm-fragment>

  inside your style file, all strong text will be exported to <name|Html>
  using a bold red font.

  <paragraph|Html importation>

  <TeXmacs> also contains a rudimentary input converter for <name|Html> in
  <menu|File|Import|Html>. Most of HTML 2.0 and parts of HTML 3.0 are
  currently supported. However, no browsing facilities have been added yet.
  The converter also contains a reasonably complete input converter for
  embedded <name|MathML> fragments.

  When importing HTML documents, files whose names start with
  <verbatim|http:> or <verbatim|ftp:> will be downloaded from the web using
  <verbatim|wget>. If you compiled <TeXmacs> yourself, then you can download
  <verbatim|wget> from\ 

  <\verbatim>
    \ \ ftp://ftp.gnu.org/pub/gnu/wget/
  </verbatim>

  In the binary distributions, we have included <verbatim|wget>.

  <tmdoc-copyright|1998--2019|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>