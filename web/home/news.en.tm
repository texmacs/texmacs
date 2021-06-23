<TeXmacs|2.1>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Home|News><tmweb-title|Latest news about
  <TeXmacs>|<tmweb-home-links>>

  <section*|Version 2.1 has been released>

  This version of <TeXmacs> consolidates many developments that took place in
  the last decade. Most importantly, the interface is now based on <name|Qt>,
  which allowed us develop native versions for <name|Linux>, <name|MacOS>,
  and <name|Windows>. <TeXmacs> has evolved from a scientific text editor
  into a scientific office suite, with an integrated presentation mode,
  technical drawing editor, versioning tools, bibliography tool, etc. The
  typesetting quality has continued to improve with a better support of
  microtypography and a large variety of fonts. The converters for <LaTeX>
  and <name|Html> have also been further perfected and <TeXmacs> now comes
  with a native support for <name|Pdf>.

  <section*|Tricks for references and citations>

  <TeXmacs> 1.99.15 and 1.99.16 contains several interesting new features for
  references and citations:

  <\itemize>
    <item>By selecting the <verbatim|preview-ref> style package (from
    <menu|Utilities>), the target of references and citations are displayed
    in a tooltip.

    <item>When your cursor is just behind a label, reference, or citation,
    the <menu|Focus|Search> menu contains several items for finding labels,
    references, and citations that use the same key or that are incorrect or
    broken.

    <item>When entering a reference, you may obtain the label from its
    numeric value. If you also selected the <verbatim|preview-ref> package,
    then the target label text is shown in a preview window.

    <item>You may use the <verbatim|smart-ref> package to make references
    appear like \PTheorem 2.1\Q or Theorems 1.5, 1.7, and 2.3\Q instead of
    mere numbers.
  </itemize>

  <section*|Improved Html export>

  Version 1.99.14 of <TeXmacs> comes with a highly improved converter to
  <name|Html>. The resulting webpages should display nicely on a variety of
  devices, from smartphones to large computer screens.

  In <menu|Edit|Preferences|Convert|Html|CSS stylesheet>, you may specify a
  CSS stylesheet to be used for the generated <name|Html> pages. For
  instance, the \Pdark colored\Q theme has been used by the author of
  <TeXmacs> for generating his personal website. A typical article with
  mathematical formulas from this website looks as follows:

  <\indent>
    <slink|http://www.texmacs.org/joris/ffnlogn/ffnlogn.html>
  </indent>

  <section*|Release of the new <name|Qt> port of <TeXmacs>>

  From version 1.0.7.12 of <TeXmacs> on, the default graphical interface of
  <TeXmacs> is based on the <hlink|<name|Qt><nbsp>toolkit|http://qt.nokia.com/products/>
  instead of the classical X11 system. As a consequence, we also provide new
  ports for <name|Windows> and <name|MacOS>, with a native look and feel.

  <section*|Version 1.0.7 has been released>

  The main improvements with respect to <TeXmacs>-1.0.6 are:

  <\itemize>
    <item>Improved <name|Scheme> sessions and syntactic highlighting of
    <name|Scheme> programs.

    <item>Improved tool for drawing graphical pictures.

    <item>Basic CJK support.

    <item>Replacement of <name|Proclus> plug-in by integrated linking tool.

    <item>New plug-ins for <name|Cadabra>, <name|Feynmf> and <name|Sage>.
  </itemize>

  <section*|Version 1.0.6 has been released>

  The main improvements with respect to <TeXmacs>-1.0.5 are:

  <\itemize>
    <item>Improved <name|<LaTeX>> and <name|Html> converters and support for
    <name|MathML>.

    <item>An <math|\<alpha\>>-version of a graphical mode for drawing
    technical pictures.

    <item>Default use of <name|Type 1> fonts.

    <item>New plug-ins for <name|Maple> and <name|Mathematica>.

    <item>Simple animations.

    <item>Faster booting.

    <item>Started documentation of the <name|Scheme> <acronym|API>.
  </itemize>

  <section*|Version 1.0.5 has been released>

  The main improvements with respect to <TeXmacs>-1.0.4 are:

  <\itemize>
    <item>The Windows port of <TeXmacs> is <hlink|available
    now|../download/windows.en.tm>.

    <item>Addition of the (still imperfect) <verbatim|svmono>,
    <verbatim|svjour>, <verbatim|elsart> and <verbatim|ifac> styles.

    <item>A more complete documentation on how to write style files.

    <item>German translation of the manual by Dietmar Jung.

    <item>Several minor improvements in performance.

    <item>Updated and improved Maxima plug-in for upcoming version 5.9.2 of
    Maxima.

    <item>Addition of a plug-in for Dra<TeX>.
  </itemize>

  <section*|A beta-version of the Windows port has been released>

  A beta-version of the Windows port is <hlink|available
  now|../download/windows.en.tm>. The new version comes with

  <\itemize>
    <item>An improved installer.

    <item>Support for Windows 98 and all versions after that.

    <item>Full image support.

    <item>Direct integration with <name|Ghostscript> for printing.

    <item>Faster and more correct display of anti-aliased characters.

    <item>Support of <name|Aspell> and experimental communication by pipes.
  </itemize>

  Please try the beta-version and <hlink|let us
  know|../contact/contact.en.tm> about possible problems!

  <section*|Help save the French Imprimerie Nationale heritage>

  Sign <hlink|petition|http://www.garamonpatrimoine.org/petition.html> to
  help save the heritage of the French \S Imprimerie Nationale \T.

  Click on the English flag for an English translation of the text.

  <section*|Version 1.0.4 has been released>

  The main improvements with respect to <TeXmacs>-1.0.3 are:

  <\itemize>
    <item>A <hlink|very experimental Windows
    version|http://www.texmacs.org/tmweb/download/windows.en.html> as well as
    the official inclusion into <name|Cygwin>.

    <item>Higher reactivity for editing large documents and faster booting
    and loading of files.

    <item>A lot of new documentation about the TeXmacs document format,
    environment variables and tags.

    <item>Possibility to work with TrueType fonts instead of Type 3 fonts.

    <item>New interfaces with <name|Python>, <name|Clisp>, <name|Cmucl> and
    <name|Matlab>.

    <item>Improved mode for editing style files.

    <item>Thorough reorganization of the style files, implementation of
    "mutator" tags, and location-awareness of internal trees.

    <item>Support for the Danish language and partial translation of the
    documentation into Polish.
  </itemize>

  <section*|Version 1.0.3 has been released>

  The main improvements with respect to <TeXmacs>-1.0.2 are:

  <\itemize>
    <item>An export filter for <name|Postscript> and <name|Pdf> which
    supports scalable fonts.

    <item>An interface with <name|FreeType 2> for the support of several new
    fonts.

    <item><TeXmacs> has become \PDRD aware\Q, which means that properties can
    be associated to primitive and user defined tags.

    <item>The <TeXmacs> website has been redesigned and generated using
    <TeXmacs>.

    <item>A better <name|Html> output filter and several minor improvements
    in the <LaTeX> import converter.

    <item>Several improvements in the plug-ins and support for the free
    <name|Axiom> system.

    <item>Scheme support for plugging in new data types and converters.

    <item>Support for the Slovene language.
  </itemize>

  <section*|Version 1.0.2 has been released>

  The main improvements with respect to <TeXmacs>-1.0.1 are:

  <\itemize>
    <item>Improved user & developer support for plugins + documentation.

    <item>Plugins for <name|Graphviz>, <name|GNUplot>, <name|Eukeides>.

    <item>Improved converters for <name|Html>.

    <item>Possibility to save and load as <name|XML>.

    <item>Manual available in French, Italian, Portuguese and Spanish.

    <item>Informative flags for otherwise invisible tags.

    <item>Cleaner interface with <name|Guile> and module system.
  </itemize>

  <tmdoc-copyright|1999\U2019|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>