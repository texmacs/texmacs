<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Using the tmdoc style>

  Besides the <apply|hyper-link|copyright information|copyright.en.tm> macros
  and <apply|hyper-link|traversal macros|traversal.en.tm>, which have been
  documented before, the <tmstyle|tmdoc> style comes with a certain number of
  other macros and functions, which you should use whenever appropriate:

  <\description>
    <expand|item*|<markup|key>>This macro is used to indicate keyboard input
    like <shortcut|(save-buffer)>. The specialized macros <markup|kbd-gen>,
    <markup|kbd-text>, <markup|kbd-math>, <markup|kbd-symb>,
    <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>, <markup|kbd-exec>
    and <markup|kbd-table> are used for keyboard input corresponding to a
    specific type of action or mode. For instance, <markup|kbd-math>
    corresponds to keyboard shortcuts for mathematical operations, such as
    <key|math f>, which starts a fraction.

    <expand|item*|<markup|menu>>This function with an arbitrary number of
    arguments indicates a menu like <apply|menu|File> or
    <apply|menu|Document|Language>. Menu entries are automatically translated
    by this function.

    <expand|item*|<markup|markup>>This macro is used in order to indicate a
    macro or a function like <markup|section>.

    <expand|item*|<markup|tmstyle>>This macro indicates the name of a
    <TeXmacs> style file or package like <tmstyle|article>.

    <expand|item*|<markup|tmpackage>>This macro indicates the name of a
    <TeXmacs> package like <tmpackage|std-markup>.

    <expand|item*|<markup|tmdtd>>This macro indicates the name of a <TeXmacs>
    <abbr|d.t.d.> like <tmdtd|number-env>.
  </description>

  Notice that the contents of none of the above tags should be translated
  into foreign languages. Indeed, for menu tags, the translations are done
  automatically, so as to keep the translations synchronized with the
  translations of the actual <TeXmacs> menus. In the cases of markup, styles,
  packages and <abbr|d.t.d.>s, it is important to keep the original name,
  because it often corresponds to a file name.

  The following macros and functions are used for linking and indexing
  purposes, although they should be improved in the future:

  <\description>
    <expand|item*|<markup|simple-link>>This macro takes an URL
    <with|mode|math|x> as argument and is a hyperlink with name and
    destination <with|mode|math|x>.

    <expand|item*|<markup|hyper-link>>This macro is a usual hyperlink.

    <expand|item*|<markup|concept-link>>This macro takes a concept as
    argument. Later on an appropriate hyperlink might be created
    automatically from this and the other documentation.

    <expand|item*|<markup|only-index>>Index a simple string.

    <expand|item*|<markup|def-index>>Definition of a new concept; the text is
    printed in italic and indexed.

    <expand|item*|<markup|re-index>>Reappearance of an already defined
    concept; the text is printed in roman and put in the index.
  </description>

  The following tags are also frequently used:

  <\description>
    <expand|item*|<markup|icon>>Link to an icon in a central directory like
    <verbatim|$TEXMACS_PATH/doc/images/pixmaps>.

    <expand|item*|<markup|screenshot>>Link to a screenshot. The actual
    screenshots are stored in a central directory like
    <verbatim|$TEXMACS_PATH/doc/images/screenshots>.

    <expand|item*|<markup|scheme>>The <value|scheme> language.

    <expand|item*|<markup|cpp>>The <value|cpp> language.

    <expand|item*|<markup|framed-fragment>>For displaying a piece of code in
    a nice frame.

    <expand|item*|<markup|scheme-fragment>>For multi-paragraph <value|scheme>
    code.

    <expand|item*|<markup|cpp-fragment>>For multi-paragraph <value|cpp> code.

    <expand|item*|<markup|tm-fragment>>For a piece of <TeXmacs> markup code
    in <value|scheme> format.

    <expand|item*|<markup|scheme-code>>For a short piece of <value|scheme>
    code.

    <expand|item*|<markup|cpp-code>>For a short piece of <value|cpp> code.

    <expand|item*|<markup|descriptive-table>>For descriptive tables; such
    tables can be used to document lists of keyboard shortcuts, different
    types of markup, <abbr|etc.>
  </description>

  The <tmstyle|tmdoc> style inherits from the <tmstyle|generic> style and you
  should use macros like <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|etc.> from this style whenever appropriate.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|key>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-gen>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-text>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-symb>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-big>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-large>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-ia>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-exec>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-table>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|menu>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|File>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Language>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|markup>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmstyle>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|article>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmpackage>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|std-markup>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdtd>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|number-env>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|simple-link>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hyper-link>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|concept-link>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|only-index>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|def-index>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|re-index>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|icon>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|screenshot>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|framed-fragment>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme-fragment>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cpp-fragment>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tm-fragment>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme-code>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cpp-code>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|descriptive-table>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|generic>>|<pageref|idx-41>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-42>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-43>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize>>|<pageref|idx-44>>
    </associate>
  </collection>
</auxiliary>
