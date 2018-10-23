<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|To do list for improving the current implementation>

  <section|General architecture>

  <subsection|Implementation of better debugging tools>

  <\description>
    <item*|Error recovery>

    Mechanism to recover all C++ and scheme error messages in a dedicated
    <TeXmacs> buffer, together with foldable backtraces.

    <item*|Locations>

    Provide correct locations for the <scm|texmacs-error> primitive. Also
    provide line numbers in C++ crash reports.

    <item*|Exception semantics>

    Provide a clean exception semantics for all routines in the basic APIs.

    <item*|Throw-catch>

    Try hard to recover from errors and segmentation faults, except when
    configuring using <verbatim|--enable-debug>.

    <item*|Test suite>

    Test-suite of <TeXmacs> documents for different features and automated
    checks for certain types of correctness.

    <item*|Memory leaks>

    Perform extensive (automated) checks for memory leaks using
    <name|Valgrind> and implement a marking algorithm.

    <item*|Unit Test>

    More and more unit tests should be written under the GTest framework.
  </description>

  <subsection|Improving the speed>

  <\description>
    <item*|Internal data representation>

    Encode the system environment variables; this will globally accelerate
    the program. Also, intermediate data during the typesetting process might
    be encoded in a more binary way.

    <item*|Coding style>

    Systematically use \Pconst ref\Q style parameter passing should both
    decrease the size of the binary and accelerate the global efficiency.

    <item*|Better caching mechanism>

    Optimize performance as a function of cost of recomputation and storage
    requirement

    <item*|Increased laziness>

    The typesetter should be made lazy in a more fundamental way (see
    <hlink|upcoming style rewriting tool|#style-rew-sec>).
  </description>

  <section|Internal C++ utilities>

  <subsection|Tree manipulation routines>

  <\description>
    <item*|Document correction>

    <label|doc-correct-routine>Transform a tree into a correct tree for the
    current style. This routine should in particular be called before or
    after any conversion to an external format.

    <item*|Downgrade <TeXmacs> documents>

    Downgrade <TeXmacs> documents to those of a previous version. This should
    be used in particular in combination with plug-ins: after communication
    of the version of <TeXmacs> for which the plug-in was designed, <TeXmacs>
    should emulate the behaviour of this, potentially older, version.
  </description>

  <subsection|Encodings and fonts>

  <\description>
    <item*|Unicode>

    Systematically use <name|Unicode>.

    <item*|Agglomerated fonts>

    Some changes have still to be made in the way font encodings are handled.
    This should make it easier to maintain fonts with characters from several
    physical fonts, virtual fonts, special characters, etc.

    <item*|Determination of logical font>

    Currently, the current logical font is determined from a fixed set of
    environment variables only. The current typesetting font should rather be
    a tree (instead of a string), which is evaluated (so that environment
    variables are replaced) and then passed to
    <verbatim|find_font(display,tree)>. The current font can then be a joined
    font and future fonts might depend on user environment variables (i.e.
    colored fonts, using more than one color).

    <item*|Rubber characters>

    <label|rubber-chars>Implement large brackets and wide delimiters for
    <name|STIX> fonts.

    Provide standard implementations using vector graphics, which do not
    depend on fonts, with many parameters for customizing width, shape, etc.
  </description>

  <section|<name|Scheme> interface>

  <subsection|General>

  <\description>
    <item*|<name|Scheme> plug-in>

    Internally present <name|Guile> as a plug-in, which could later be
    replaced by another <name|Scheme> implementation.

    <item*|Unified <TeXmacs> content interface>

    A clean interface for manipulating <TeXmacs> content (a unified interface
    for both internal <TeXmacs> trees and the associated <scheme>
    representation).

    <item*|Documentation>

    Better documentation on how to use the <scheme> extensions.

    <item*|Programming aids>

    Implement a proper parser and autocompletion mechanism. Add standard
    <abbr|IDE> code browsing tools and visual aids.
  </description>

  <subsection|Coding style>

  <\description>
    <item*|Preservation of locality>

    Replace dispatch tables by contextual overloading where possible (i.e.
    <LaTeX> export).

    <item*|Closures>

    Systematic use of closures throughout the code. In particular, we should
    base our implementation of dialogs on closures instead of continuations.

    <item*|Naming conventions>

    Rename <verbatim|drd-> <math|\<rightarrow\>> <verbatim|logic-> for
    routines from kernel/drd.

    Appropriate naming convention for routines which operate on markup: when
    to use <verbatim|tmconcat->, <verbatim|concat->, <verbatim|concat/>,
    <verbatim|tree-concat->, etc., especially for tags like "<verbatim|tree>"
    which are also used as data types.
  </description>

  <section|Typesetter>

  <subsection|General>

  <\description>
    <item*|Eequivalent for <TeX> <verbatim|\\topsep>>

    In <TeXmacs>, the <verbatim|\\topsep> always equals 0.75em. It should be
    possible to modify this. For symmetry, we might also introduce a \Pbottom
    sep\Q.

    <item*|Resizing macro expansions>

    When attempting to resize a macro application to become smaller than the
    \P<verbatim|x>\Q, the invisible markers may have undesirable
    side-effects.

    <item*|Font parameters>

    Separate environment variables for \Ptypewriter\Q, \Psans serif\Q,
    \Psmall caps\Q, \Pslant\Q, etc.

    Find rendering font as a function of selected font and language?

    <item*|Simplification of page parameters>

    Should we still reorganize? It should be easy to access to the page width
    and height and some other parameters.

    <item*|Origins of boxes>

    Boxes should not have origins; its children should have positions
    instead.

    <item*|Temporarily incorrect markup>

    Example: when pressing <shortcut|(remove-structure-upwards)> inside a
    <markup|surround> tag, we first remove the two first children of the
    surround tag (so that it obtains arity 1) and next remove the tag itself.
    The temporarily incorrect <markup|surround> tag of arity 1 may cause
    <verbatim|bridge_surround> to crash. Temporary fix: add extra nodes in
    the bridge. Better fix: implement an elementary modification
    <verbatim|assign_label>, which allows to change the surround tag into
    another tag like tuple before performing incorrect operations.

    In general: the typesetter should not crash in the case of incorrect edit
    trees.
  </description>

  <subsection|Text>

  <\description>
    <item*|Introduction of the paragraph tag>

    Distinguish between paragraphs and paragraph units.

    <item*|Page decorations (headers and footers)>

    Should the corresponding environment variables become macros? Also make
    the contents editable whenever possible.

    <item*|Fine-grained baseline skip>

    Associate baseline skip (or ascent and descent) to each individual box in
    concatenation. Next compute maximum over each line. Take maximum of
    maximum after first line and maximum before second line in order to
    obtain final value. Also for other parameters such as baselineskip?

    <item*|Precise scope of variables which affect paragraph layout>

    The scopes of several such variables is currently a bit ill-defined,
    especially when they are modified several times by macros.

    <item*|User-defined hyphenations of words>

    <item*|Oriental languages>

    Cursor movement is extremely slow for oriental languages. Line breaking
    could also be further improved.
  </description>

  <subsection|Mathematics>

  <\description>
    <item*|Fractions>

    Macro for continued fractions, such that right hand sides of fractions
    are aligned.

    Less vertical spacing in text style between numerator/denominator and
    bar.

    <item*|Equations>

    Left-numbering of equation arrays. Should be possible now using the
    extern primitive.

    <item*|Operator decorations>

    Regard <markup|above>, <markup|below>, <markup|below> and <markup|wide*>
    as operator decorations, both for typesetting purposes and for the
    standard mathematical grammar.

    <item*|Rubber brackets and wide delimiters>

    See <hlink|section on fonts|#rubber-chars>.

    <item*|Homoglyps>

    Mode for showing invisible symbols and distinguishing between other
    homoglyps. Also show alternatives for homoglyphs in the <menu|Focus>
    menu.

    <item*|Missing symbols>

    Forall and exists as big operators.

    Upright and sans serif mathematical symbols.
  </description>

  <subsection|Style sheet language>

  <\description>
    <item*|Style options>

    Introduce a mechanism for style options. For instance, style packages in
    a given directory might be executed before the actual document style and
    document packages.

    <item*|Greyed menu entries>

    Grey menu entries for parameters which cannot be changed in the style.
    For instance, a<nbsp>style might provide a limited number of font base
    sizes.

    <item*|Cleaner replacement for <markup|drd-props>>

    Replace <markup|drd-props> primitive by a primitive inside the macro,
    e.g.:

    <verbatim| \ \<less\>macro\|a\|b\|\<less\>drd-props\|\<less\>drd-prop\|a\|accessible\<gtr\>\|...\|body\<gtr\>\<gtr\>>

    and/or <markup|no-assign> tag for associating drd-properties without
    actual assignment.

    <item*|Prevent packages to be loaded twice>

    Implement a <markup|provide-package> variant of use-package, which does
    not (re)load.

    Or simply let use-package never reload the same package twice.

    <item*|Forms>

    Implement forms by letting field tags link to the main form tag.

    <item*|Interaction with <scheme>>

    Declaration of key-bindings and more complex scheme code in style
    packages.

    Declaration of style packages and macros from within a <scheme> module.
  </description>

  <section|Editor>

  <subsection|Structured text>

  <\description>
    <item*|Presentation tags>

    Should we provide icons for certain presentation tags and other
    environment variables, such as font type and size?

    <item*|Titles>

    The interface for editing document titles is buggy. In particular, we
    cannot remove authors or transform selections into titles. Also selecting
    part of the title information is buggy. Finally, the macros for rendering
    titles are overly complex.

    <item*|Inline and block content>

    Automatically turn block content into inline content when appropriate.

    <item*|Outward selection>

    Maybe use shift-mouse-click for outward selection (besides pointing
    inside a hyperlink).
  </description>

  <subsection|Editing source trees>

  <\description>
    <item*|Editing macro names>

    User-defined macros can be turned into a compound using the backspace
    key, builtin macros can't. Why is that? How do I change the name of the
    call to a buildin macro? Can I avoid removing it and creating a new one?

    If the first argument of a call to a user defined macro contains a
    <markup|document> tag, I cannot turn it into a compound any more. Why
    that?

    By the way, there seems to be no way to manually create or remove
    <markup|document> tags.

    <item*|Editing meta-information>

    Edit the "init" part of a file in source code form.

    <item*|Customizable presentation>

    Make the presentation of source code more customizable by user macros.
    Done for syntactic highlighting, but should also be done for special
    rendering of certain primitives and the rendering of tags.

    <item*|Primitive for block arguments>

    Currently, we may stretch tags. We also need a primitive for stretching
    tag arguments.

    <item*|Comments>

    Tags for short commands inside the program.

    <item*|Visual hints>

    Display informative flags at places where tags have been stretched or a
    modification to the source style has been applied.
  </description>

  <subsection|Miscellaneous>

  <\description>
    <item*|``Edit as'' facility>

    A facility <menu|Edit as> to convert a selection into any other format
    and put the result in an <markup|edit-as> tag for manual editing. When
    pressing return, reconvert back into <TeXmacs>.

    <item*|Style file assistants>

    Assistant for creating your own styles.

    Widget for editing simple macros in the preamble without entering
    preamble mode.

    <item*|Failing <cpp|as_double> and <cpp|as_string> conversions>

    When <verbatim|author> tags for the history management get large, then
    the <cpp|as_double> and <cpp|as_string> conversions may fail.

    <item*|Relaunching <TeXmacs>>

    Relaunch <TeXmacs> after change of look and feel.
  </description>

  <section|Converters>

  <\description>
    <item*|Quoting>

    Implement different routines for quoting and quote/unquote using the
    appropriate routine.

    <item*|<LaTeX>>

    <\description-dash>
      <item*|Avoid name-clashes with built-in commands>Importation of <LaTeX>
      macros whose names coincide with built-in commands may lead to
      incorrect documents (e.g. the point macro in
      <verbatim|publs/1998/zeta.tex>). We should probably
      <hlink|post-correct|#doc-correct-routine> imported documents.

      <item*|Better parsing of verbatim arguments>Some commands, like
      <verbatim|documentclass>, <verbatim|cite>, <abbr|etc.> take verbatim
      arguments, which should not be parsed in the generic way. The
      <verbatim|string_arg> function is a temporary remedy.
    </description-dash>

    <item*|Postscript>

    Use <name|PdfMark> watermarks for <name|Pdf> generation with hyperlinks
    and tables of contents.
  </description>

  <section|Graphics editor>

  <subsection|Interface>

  <\description>
    <item*|General interface>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Explanatory messages on the status bar.

        <item>Better \Pmouse button layout\Q: do as much as possible with
        left mouse button only and reserve right button for contextual menu.

        <item>Fine-grained resizing using keyboard (also allow modification
        of grain?).

        <item>Resizing using mouse.

        <item>Automatic detection of simple types of curves when dragging in
        drawing mode.

        <item>Automatic detection of simple ornaments (arrows, dots).

        <item>Cairo plug-in for nicer looking graphics.

        <item>Graphical macros and constraint-based drawings.

        <item>Documentation, reference guide (markup specification) and
        tutorial.
      </with>
    </itemize>
  </description>

  <subsection|Reorganization of <scheme> code>

  <\description>
    <item*|Clean API for graphical objet and current decorations>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Remove/restore one object (while preserving <math|z>-order).

        <item>Remove/restore a selection of objects (while preserving
        <math|z>-order).

        <item>Specify decorations for current graphical object.

        <item>Undo and cancel.
      </with>
    </itemize>

    <item*|Clean API for setting/retrieving graphical attributes>

    Possible attributes: color, line width, line style, fill color, etc.

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Selecting the value of a given attribute.

        <item>Retrieving the value of a given attribute.

        <item>Apply attributes to one or more objects.

        <item>Retrieve attributes from one (or more) objects.
      </with>
    </itemize>

    <item*|Clean API for global attributes of the entire graphics>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Set an attribute.

        <item>Retrieve an attribute.
      </with>
    </itemize>

    <item*|Clean API for simple object construction>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Different types of arguments: point, length, content, etc.

        <item>Adding an argument to the current object.

        <item>Removing an argument from a current object.

        <item>Event handlers and how to map mouse buttons to different
        actions.

        <item>Computation of decorations.
      </with>
    </itemize>

    <item*|Clean API for groups>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Adding an object / region.

        <item>Removing an object / region.

        <item>Geometric transformations.

        <item>Changing <math|z>-order.

        <item>Group / ungroup.

        <item>Copy and paste.

        <item>Computation of decorations.
      </with>
    </itemize>

    <item*|Documentation of all APIs>
  </description>

  <section|Semantic editing tools based on packrat parsing>

  <\description>
    <item*|Packrat engine>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Implement grammar rules with productions.

        <item>Implement content <name|MathML> output.

        <item>When making semantic selections, don't select ignored markup at
        the border of selections (or only if explicitly selected). For
        instance, inside maxima sessions, the
        <verbatim|<with|color|red|(%o<math|x>)>> output prefix is current
        included when selecting a complete output semantically.
      </with>
    </itemize>

    <item*|Mathematics>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Automatic correction of formulas <math|\<rightarrow\>> editing
        mode based on \Pslots\Q.

        <item>Explicit prefix-postfix, prefix-infix and symbol-prefix
        operator types for operators such as <math|!>, <math|-> <abbr|resp.>
        <math|\<mathd\>>.

        <item>Better scheme for entering big operators and large delimiters
        as symbols and not binary/ternary constructs.

        <item>Above, below, wide and wide* as operator decorations.

        <item>Upgrade wide spaces (e.g. <math|\<geqslant\>> <verbatim|1em>)
        as separating spaces.
      </with>
    </itemize>

    <item*|Universal grammar>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Overload <math|.> and <math|\<rightarrow\>> as data access
        operators.

        <item>Symbols for <math|min>and <math|max> with the appropriate
        precedence.

        <item>Notations <math|a,b\<in\>X> and <math|i=0,\<ldots\>,n>.

        <item>Several precedences for separators in
        <math|<around|[|a,b\|c,d\|e,f|]>>.
      </with>
    </itemize>

    <item*|Programming languages>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Add facility to automatically close partial environments, e.g.
        with missing <verbatim|}> or missing statement after <verbatim|if x
        then>. Applications: highlighting imcomplete input, automatic
        indentation.
      </with>
    </itemize>

    <item*|Editing>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Generalize use of mathematical focus instead of usual focus.
        For instance, the focus bar, tooltips on math symbols, using
        <verbatim|semantic_root> instead of <verbatim|innermost>, etc.

        <item>Easy way to switch between semantic and non-semantic
        selections.

        <item>Shortcut for circulating among homoglyphs.
      </with>
    </itemize>
  </description>

  <section|Universal spreadsheet>

  <name|Rationale:> We started to incorporate a \Puniversal spreadsheet\Q
  facility into <TeXmacs>. The idea is that all dependencies between the
  cells in the sheet are analyzed by <TeXmacs>, but all actual computations
  are delegated to an extern system of your choice, like one of the currently
  supported computer algebra systems. Also, the data of the spreadsheet will
  not necessarily be formatted in a rectangular table; one can also imagine
  dependencies between nodes of a tree, elements of a graph, or anything
  else.

  <\description>
    <item*|Implementation and dependencies>

    <\itemize>
      <item>It would be better to use links for dependencies between cells
      instead of going through the entire document at each re-evaluation, as
      well as links to auxiliary cache for storing the last computed values.
      This can for instance be done by defining an environment variable with
      the ID of the current cell and adding a link to this ID for any
      <markup|calc-ref> or <markup|cell-ref> inside the cell.

      <item>Implementation of the possibility to associate an alternative
      name to a cell.

      <item>Possibility to send all inputs at once to the plug-in instead of
      evaluating them one by one, so as to speed up computations.

      <item>How to deal with errors?
    </itemize>

    <item*|Interface>

    <\itemize>
      <item>The focus bar should be adapted. Inside a spreadsheet, the
      facilities for editing tables should still be available and we should
      be able to edit the input fields for <markup|cell-output> tags (at
      least when the input is a string).

      <item>Possibilitity to apply operations to subtable selections. For
      instance, when selecting part of a column and applying \Psum\Q, the sum
      of the column should be computed at the bottom of the selection.
      Similarly, applying a unary operation such as \Psin\Q might apply the
      operation to each cell in the selection and put the result right next
      to it.
    </itemize>
  </description>

  <section|Upcoming style rewriting tool><label|style-rew-sec>

  <\description>
    <item*|Reminders>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Attention to implicitly changed variables, such as
        <src-var|the-tags><compound|markup|>,
        <src-var|the-label><compound|markup|>, etc., which may cause the
        entire document to be retypeset.

        <item>Attention to <src-var|base-file-name><compound|markup|>,
        <src-var|cur-file-name><compound|markup|>, <src-var|current-part>.
        Also put these variables in <verbatim|vars.hpp>.

        <item>Replace <markup|arg> primitive by quasi-quoting mechanism. This
        seems to be quite necessary: consider
        <verbatim|\<less\>assign\|foo\|\<less\>macro\|body\|...\<gtr\>\<gtr\>>
        and a document piece <verbatim|\<less\>\\foo\<gtr\> ...
        \<less\>/foo\<gtr\>> with a large body. When making changes to the
        body; the environment for typesetting the body also changes, because
        we need to store the body in the <verbatim|foo> environment argument
        variable. For this reason, we implemented an alternative mechanism,
        which is enabled using <cpp|#define ALTERNATIVE_MACRO_EXPANSION>.

        <item>More systematic usage of symbols instead of strings.

        <item>Should <BibTeX> labels really be prefixed by <verbatim|bib->?

        <item>Links and multiple views: which view should be selected when
        following a hyperlink. More generally: refine criteria for
        automatically preferring one locus over another one in presence of
        ambiguities.
      </with>
    </itemize>

    <item*|Auxiliary data>

    There is a problem with auxiliary data in hidden markup (eg. labels,
    loci, links, indexes).

    Analogy: hidden markup <math|\<leftrightsquigarrow\>> loaded file which
    is not in a buffer. We might want to associate auxiliary data to tree
    nodes instead of files. Notice that there are different types of hidden
    structures:

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Contents should <em|not> be evaluated when hidden, but we need
        to keep track of auxiliary data. Example: a hidden chapter.

        <item>Contents are evaluated but not typeset: auxiliary data may be
        kept in macro expander. Example: fold.

        <item>Contents are not evaluated, not typeset and no auxiliary data
        are attached to the contents. Example: a comment or any piece of
        markup which has to be completely ignored.
      </with>
    </itemize>
  </description>

  <section|Upcoming markup-based GUI>

  <\description>
    <item*|Remaining issues>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Attach the action to be applied after popup input to a specific
        buffer.

        <item>Cleaner implementation of read-only parts in documents.

        <item>Cleaner implementation of size computation of widget. Also:
        notebooks and folding constructs, which may require to update the
        size later on.

        <item>Cleaner implementation of widget fields (i.e. the way we
        associate data to names).

        <item>Don't require the specification of a body for hidden input
        fields.

        <item>Argument history and browsing suggestions using keyboard.

        <item>Destroy call backs associated to a widget when the
        corresponding window is closed using another method than
        <verbatim|dismiss>. Idea: potentially associate a dismiss routine to
        each buffer.
      </with>
    </itemize>
  </description>

  <section|Management and administration>

  <\description>
    <item*|Publicity>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>List actions to be undertaken.
      </with>
    </itemize>

    <item*|Mirror sites>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>CVS mirror at Savannah and automatic update scheme.

        <item>Also search for other mirror sites (increases availability and
        visibility on the web).
      </with>
    </itemize>
  </description>

  \;

  <section|Tutorial or series of introductory articles>

  <\description>
    <item*|General introduction/quick overview>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Different purposes of <TeXmacs>.

        <item>Writing a simple text.

        <item>Style-sheets and writing a simple macro.

        <item>Running a computer algebra system.

        <item>Inserting an action tag which launches an <verbatim|xterm>.
      </with>
    </itemize>

    <item*|Writing simple structured texts>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Why are structured documents useful?

        <item>How to enter structure into <TeXmacs>, redundancy of the
        interface.

        <item>Understanding the visual indications, cursor movement.

        <item>Editing the structure (removing structure, improper nesting,
        variants, numbering).

        <item>Exploiting structure: spell checking, searching.

        <item>Reminders about correct typography.
      </with>
    </itemize>

    <item*|Writing mathematical texts>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Entering math mode, equations, equation arrays.

        <item>Simple mathematical formulas, recall cursor movement.

        <item>Mathematical symbols, variants.

        <item>Semantics of multiplication, function application and other
        implicit semantics.

        <item>Simple matrices and other tabular environments.
      </with>
    </itemize>

    <item*|Writing simple macros and style-sheets>

    <\itemize>
      <\with|par-par-sep|0fn>
        <item>Rendering of source code, what is source code?

        <item>A simple macro.

        <item>A simple style package.

        <item>Customization of some existing macros.
      </with>
    </itemize>
  </description>

  <tmdoc-copyright|1998--2011|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>