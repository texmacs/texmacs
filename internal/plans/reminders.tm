<TeXmacs|1.0.4.1>

<style|article>

<\body>
  <doc-data|<doc-title|Reminder of things to be
  done>|<doc-subtitle|Classified by subject>>

  <section|Organizational tasks>

  <\fold>
    Documentation
  <|fold>
    <\fold>
      Manual
    <|fold>
      \;
    </fold>

    <\fold>
      Tutorial or series of introductory articles (<abbr|e.g.> for Linux
      Magazine).
    <|fold>
      <\fold>
        General introduction/quick overview
      <|fold>
        Different purposes of <TeXmacs>

        Writing a simple text

        Style-sheets and writing a simple macro

        Running a computer algebra system

        Inserting an action tag which launches an <verbatim|xterm> (and
        mutator tag?)
      </fold>

      <\fold>
        Writing simple structured texts
      <|fold>
        Why are structured documents useful?

        How to enter structure into <TeXmacs>, redundancy of the interface

        Understanding the visual indications, cursor movement

        Editing the structure (removing structure, improper nesting,
        variants, numbering)

        Exploiting structure: spell checking, searching

        Reminders about correct typography
      </fold>

      <\fold>
        Writing mathematical texts
      <|fold>
        Entering math mode, equations, equation arrays

        Simple mathematical formulas, recall cursor movement

        Mathematical symbols, variants

        Semantics of multiplication, function application and other implicit
        semantics

        Simple matrices and other tabular environments
      </fold>

      <\fold>
        Writing simple macros and style-sheets
      <|fold>
        Rendering of source code, what is source code?

        A simple macro

        A simple style package

        Customization of some existing macros
      </fold>

      Etc.
    </fold>

    <\fold>
      Introductory article for mathematicians, physicists and computer
      scientists.
    <|fold>
      \;
    </fold>
  </fold>

  <\fold>
    Publicity
  <|fold>
    \;
  </fold>

  <\fold>
    Mirror sites
  <|fold>
    CVS mirror at Savannah and automatic update scheme.

    Also search for other mirror sites (increases availability and visibility
    on the web).
  </fold>

  <section|General architecture>

  <\fold>
    Implement better tools for debugging
  <|fold>
    \;
  </fold>

  <\fold>
    Hunt down memory leaks
  <|fold>
    \;
  </fold>

  <\fold>
    Implement marking algorithm
  <|fold>
    \;
  </fold>

  <section|Scheme>

  <\fold>
    Internal plug-in for Guile
  <|fold>
    \;
  </fold>

  <\fold>
    Contextual overloading
  <|fold>
    \;
  </fold>

  <\fold>
    Reimplement menus as functions or macros which return widgets
  <|fold>
    \;
  </fold>

  <\fold>
    Eliminate all places where we still do not use closures
  <|fold>
    \;
  </fold>

  <\fold>
    Better preservation of locality
  <|fold>
    <\fold>
      Dispatching should be declared in dispatch routines themselves
    <|fold>
      \;
    </fold>

    <\fold>
      Easy definition of additional properties of functions/macros and
      corresponding action
    <|fold>
      \;
    </fold>

    <\fold>
      Module exportations using <verbatim|define-public>, <abbr|etc.>
    <|fold>
      \;
    </fold>
  </fold>

  <section|Typesetter>

  <\fold>
    General
  <|fold>
    <\fold>
      Equivalent of <TeX> <verbatim|\\topsep>
    <|fold>
      In <TeXmacs>, the <verbatim|\\topsep> always equals <verbatim|0.75em>.
      It should be possible to modify this. For symmetry, we might also
      introduce a ``bottom sep''.
    </fold>

    <\fold>
      Resizing macro expansions
    <|fold>
      When attempting to resize a macro application to become smaller than
      the ``x'', the invisible markers may have undesirable side-effects.
    </fold>

    <\fold>
      Font parameters
    <|fold>
      Separate environment variables for ``typewriter'', ``sans serif'',
      ``small caps'', ``slant'', etc.

      Find rendering font as a function of selected font and language
    </fold>

    <\fold>
      Simplification of page parameters
    <|fold>
      Should we still reorganize?

      It should be easy to access to the page width and height and some other
      parameters
    </fold>
  </fold>

  <\fold>
    Text
  <|fold>
    <\fold>
      Introduction of the paragraph tag
    <|fold>
      Distinguish between paragraphs and paragraph units.
    </fold>

    <\fold>
      Page decorations (headers and footers)
    <|fold>
      Should the corresponding environment variables become macros?
    </fold>

    <\fold>
      Fine-grained baseline skip
    <|fold>
      Associate baseline skip (or ascent and descent) to each individual box
      in concatenation. Next compute maximum over each line. Take maximum of
      maximum after first line and maximum before second line in order to
      obtain final value.

      Also for other parameters as baselineskip?
    </fold>

    <\fold>
      Precise scope of variables which affect paragraph layout
    <|fold>
      \;
    </fold>
  </fold>

  <\fold>
    Mathematics
  <|fold>
    <\fold>
      Horizontal grouping instead of <verbatim|\\bigop ... \\big.>
    <|fold>
      Introduce grouping primitives in which expressions with big operands
      (and maybe large delimiters) can be encapsulated. The advantage is that
      this eases parsing and an indicative bounding box is shown.
    </fold>

    <\fold>
      Scripts to large delimiters produced by macros
    <|fold>
      Such scripts are not put at the right positions
    </fold>

    <\fold>
      Macro for fractions in continued fractions.
    <|fold>
      \;
    </fold>

    <\fold>
      Less vertical spacing in text style between numerator/denominator and
      bar
    <|fold>
      \;
    </fold>

    <\fold>
      Left-numbering of equation arrays
    <|fold>
      Should be possible now using the <verbatim|extern> primitive
    </fold>
  </fold>

  <\fold>
    Style-sheet language
  <|fold>
    <\fold>
      Introduce a style options mechanism
    <|fold>
      Option <with|mode|math|\<rightarrow\>> call back macro which can be
      enriched by several packages

      Call back macros called at end of reading style files when option
      selected

      Selected options postpended to name for style-file caching
    </fold>

    <\fold>
      Grey menu entries for parameters which cannot be changed in style
    <|fold>
      For instance, a style might provide a limited number of font base
      sizes.
    </fold>
  </fold>

  <section|Editor>

  <\fold>
    Editing source trees
  <|fold>
    <\fold>
      Edit as option
    <|fold>
      A facility "Edit as" to convert a selection into any other format and
      put the result in an "edit-as" tag for manual editing. When pressing
      return, reconvert back into <TeXmacs>.
    </fold>

    <\fold>
      More customizable presentation of source tags
    <|fold>
      Make the presentation of source code more customizable by user macros.
      Done for syntactic highlighting, but should also be done for special
      rendering of certain primitives and the rendering of tags.
    </fold>

    <\fold>
      Primitive for block arguments
    <|fold>
      Currently, we may stretch tags. We also need a primitive for stretching
      tag arguments.
    </fold>

    <\fold>
      Comments
    <|fold>
      Tags for short commands inside the program
    </fold>

    <\fold>
      Visual hints
    <|fold>
      Display informative flags at places where tags have been stretched or a
      modification to the source style has been applied
    </fold>
  </fold>

  <section|Tools and frequently used subroutines>

  <\fold>
    Quoting
  <|fold>
    Implement different routines for quoting and quote/unquote using the
    appropriate routine.
  </fold>

  <\fold>
    General caching mechanism
  <|fold>
    Optimize performance as a function of cost of recomputation and storage
    requirement
  </fold>

  \;
</body>

<\initial>
  <\collection>
    <associate|par-first|0fn>
    <associate|par-par-sep|0.5fn>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-3|<tuple|3|?>>
    <associate|auto-4|<tuple|4|?>>
    <associate|auto-5|<tuple|5|?>>
    <associate|auto-6|<tuple|6|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>General
      architecture> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>Scheme>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>Typesetter>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>Utility
      routines> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <pageref|auto-4><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>