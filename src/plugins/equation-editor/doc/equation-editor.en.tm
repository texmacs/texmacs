<TeXmacs|2.1.2>

<style|<tuple|tmdoc|british>>

<\body>
  <tmdoc-title|Using <TeXmacs> as an equation
  editor><label|sec-equation-editor><label|sec-equation-editor>

  With this <name|equation editor plugin>, <TeXmacs> can operate as a
  <em|wysiwyg> equation editor for external client applications such as
  <name|Inkscape> or <name|LibreOffice>. It hence provides an alternative to
  the use of <em|e.g.> <name|LibreOffice> <name|Math> or commercial programs
  such as <name|MathType><trademark>.

  <section*|How it works>

  Within enabled client applications (see below), one can simply create and
  revise embedded <TeXmacs> document fragment. In practice, this opens a
  <TeXmacs> window with the fragment that needs revision (or a dummy new
  equation if no equation was selected in the client) and display a simple
  toolbar indicating that <TeXmacs> is operating as an equation editor.

  <\with|par-mode|center>
    <image|screenshot_LO_Writer.png|14cm|||>
  </with>

  The user can then modify the equation at will and, when pressing the
  <key|Done> button, the temporary fragment document is closed and an updated
  fragment image is sent to the requesting application for replacing the
  initial one.

  In the client application, such <TeXmacs> fragments are actually embedded
  as <name|Svg> images that contain the <TeXmacs> source code of the
  corresponding fragment, enabling later revision.

  <paragraph|Client-side scripts>

  On the client side, some scripts are required for communicating with
  <TeXmacs> and for handling appropriately the image fragments in the client.
  As of 2022, this plugin provides the clients-side scripts for
  <name|Inkscape> and <name|LibreOffice> only. The client-side functionality
  could probably be acheived with any scriptable application that accepts
  <name|Svg> images, (<name|Scribus>, <name|MS Office>,<text-dots>)\ 

  This plugin can perform the client-side installation, see below.

  <paragraph|Client-Plugin communication modes>

  The client-side script first tries to connect with a running <TeXmacs>
  instance on the local machine through socket communications. If that fails,
  it will launch a new <TeXmacs> instance and communicate via pipes. Socket
  communication are <em|much> faster since they spare the boot-up time of
  <TeXmacs>. You can control wether or not <TeXmacs> accepts socket
  communications<note-ref|+25ltJz8sCNs6YSp><\note-footnote>
    Security-wise, opening this socket server is not a big issue because this
    plugin can only receive and perform well-defined limited operations.
  </note-footnote|+25ltJz8sCNs6YSp> from client apps to this plugin using the
  plugins' preference settings. Depending on the configuration, these
  settings are accessible either as a tab in <TeXmacs>' global preference
  widget, or through the <menu|Tools|Equ. editor plugin> menu item. If the
  plugin's preferences are not accessible, use <menu|Tools|Update|Plugins>
  and they should appear.\ 

  Note that for the existing clients, <strong|the equation revision process
  is scrictly modal> : no action can be performed in any window of the client
  until the user terminates the revision in <TeXmacs>, either by pressing the
  \ <key|Done><kbd|> \ or <key|Cancel> button.

  <paragraph*|Other Requirements>

  For this plugin to work, <hlink|an appropriate converter to <name|Svg> must
  be installed|../../../doc/main/convert/man-graphics-export.en.tm>. It will
  suggest a few, if it cannot find one.

  When the plugin is called from a client, it checks that
  <menu|Edit|Preferences|Converters|TeXmacs -\<gtr\> image|Format> is set to
  <name|Svg>, and changes it if needed.\ 

  <paragraph|Known Issues>

  When creating an <name|Svg> equation image by selecting part of an existing
  document, if the selection contains a macro call with the macro definition
  elsewhere in the document, the macro definition will not be stored in the
  SVG image. Hence, upon re-edition, this equation won't render correctly. To
  overcome this issue, you should create a style file where you would place
  the macro definition, and attach this style file to your document (the
  style files in use <em|are stored> in the <name|Svg> images).

  <section*|Using <TeXmacs> with <name|LibreOffice>>

  <TeXmacs> ships with a <name|LibreOffice> extension that contain the
  client-side scripts for interacting with this plugin (and copy-pasting
  <name|Svg> with <name|Inkscape>). This <name|LibreOffice> extension can
  additionally convert other equations contained in <name|LibreOffice>
  documents to <TeXmacs>-<name|Svg> equations at a single click: <name|LO
  Math> equations, <name|MathType><trademark> equations, and <TeX>/<LaTeX>
  markup.

  You can simply initiate the installation (or upgrade) of this
  <name|LibreOffice> extension from <TeXmacs>, through the <menu|Tools|Equ.
  editor plugin> menu, or, when available, through the plugin preference
  widget <menu|Edit|Preferences|Plugins|Equation-editor>. This installs the
  client-side scripts in the appropriate subdirectory of your user's
  preference folder for LibreOffice. This extension installation can also be
  started from LibreOffice, and it can be managed like any other LibreOffice
  extension.

  <hlink|The detailed documentation for this <name|LibreOffice> extension is
  available at GitHub|https://github.com/slowphil/SVG_and_TeXmacs_for_LibreOffice/blob/master/README.md>
  together with the code.\ 

  <section*|Using <TeXmacs> with <name|Inkscape>>

  <paragraph|Through the system clipboard>

  Be sure that the <menu|Edit|Preferences|Converters|TeXmacs -\<gtr\>
  image|Format> is set to <name|Svg>. Then inserting a <TeXmacs> equation in
  an <name|Inkscape> drawing is straightforward: select the equation, use the
  menu item <menu|Edit|Copy to...|Image>, and paste into <name|Inkscape>. In
  <name|Inkscape>, a <TeXmacs> equation appears as an ordinary <name|Svg>
  group.

  <paragraph|Modifying an equation within <name|Inkscape>>

  Within <name|Inkscape> select the equation you want to revise and activate
  <menu|TeXmacs equation> (see below) in <name|Inkscape>'s <menu|extension>
  menu.

  <paragraph|Installation of the Inkscape extension>

  The <menu|TeXmacs equation> extension for <name|Inkscape> can be installed
  or updated through the <menu|Tools|Equ. editor plugin> menu, or, when
  available, through the plugin preference widget
  <menu|Edit|Preferences|Plugins|Equation-editor>. This installs the
  client-side scripts in the appropriate subdirectory of your user's
  preference folder for <name|Inkscape>. The extension should then appear in
  <name|Inkscape>'s <menu|extension> menu the next time you start
  <name|Inkscape>.\ 

  Note that this installation is offered only when <hlink|an appropriate
  converter to <name|Svg> is installed|../../../doc/main/convert/man-graphics-export.en.tm#external
  converters>, and a the user's <name|Inkscape> preferences folder is found
  in the usual location. If you are using non-standard permission settings
  the automated install may fail; in that case you may use the following
  instructions as a guideline.

  <\folded-documentation>
    Show personalized manual install instructions
  <|folded-documentation>
    Copy the <verbatim|<extern|(lambda (x) (url-concretize (url-append
    (system-\<gtr\>url "$TEXMACS_PATH") (string-\<gtr\>url
    "plugins/equation-editor/misc/inkscape_extension/texmacs"))))|>> folder
    (with all its files) to <verbatim|<extern|(lambda (x) (url-concretize
    (url-append (if (os-mingw?) (system-\<gtr\>url "$APPDATA")
    \ (system-\<gtr\>url "~/.config"))(string-\<gtr\>url
    "inkscape/extensions"))))|>>\ 

    If you see <verbatim|<error|insecure script>> instead of file paths, you
    need to change the security preference (in
    <menu|edit|preferences|other|security>) to <verbatim|accept all scripts>
    (at least temporarily) and then refresh the display by closing/opening
    this fold or through the menu <menu|Document|Update|All>.
  </folded-documentation>

  <paragraph|Compatibility with Textext <name|Inkscape> extension>

  Equations produced with <TeXmacs> should be compatible with those of the
  <name|Textext> extension (re-editable <LaTeX> equations extension shipped
  with <name|Inkscape>): <TeXmacs> can edit <name|Textext> equations, and
  conversely people using <name|Textext<strong|>> can edit your <TeXmacs>
  equations provided they use the style file <verbatim|texmacs_latex.sty> (in
  your user's <name|Iknscape> user directory) containing the texmacs macros
  and necessary includes.
</body>

<initial|<\collection>
</collection>>