<TeXmacs|1.0.7.17>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|llncs|1.0>

      <\src-purpose>
        The style for Lecture Notes in Computer Science.
      </src-purpose>

      <\src-copyright|2012>
        Joris van der Hoeven and François Poulain
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|article|std-latex|section-base>

  <active*|<src-comment|Global layout parameters>>

  <assign|tex-odd-side-margin|<macro|63pt>>

  <assign|tex-even-side-margin|<macro|63pt>>

  <assign|tex-text-width|<macro|12.2cm>>

  <assign|tex-text-height|<macro|19.3cm>>

  <assign|tex-footnote-sep|<macro|7.7pt>>

  <assign|tex-margin-par-width|<macro|90pt>>

  <assign|tex-below-display-skip|<macro|<tex-above-display-skip>>>

  <active*|<src-comment|Font sizes>>

  <active*|<src-comment|Sectional macros>>

  <assign|section-title|<macro|name|<sectional-normal|<with|font-size|1.19|font-series|bold|math-font-series|bold|<vspace*|<tex-len|18pt|4pt|4pt>><hspace|0pt><arg|name><vspace|<tex-len|12pt|4pt|4pt>>>>>>

  <assign|subsection-title|<macro|name|<sectional-normal|<with|font-size|1|font-series|bold|math-font-series|bold|<vspace*|<tex-len|18pt|4pt|4pt>><hspace|0pt><arg|name><vspace|<tex-len|8pt|4pt|4pt>>>>>>

  <assign|subsubsection-title|<macro|name|<with|font-size|1|font-series|bold|math-font-series|bold|<vspace*|<tex-len|18pt|4pt|4pt>><hspace|0pt><arg|name><hspace|<tex-len|0.5em|0.22em|0.1em>>>>>

  <assign|paragraph-title|<macro|name|<with|font-size|1|font-shape|italic|<vspace*|<tex-len|12pt|4pt|4pt>><hspace|0pt><arg|name><hspace|<tex-len|0.5em|0.22em|0.1em>>>>>

  <active*|<src-comment|Theorems-like macros>>

  <active*|<src-comment|Itemize lists>>

  <assign|itemize-1|<macro|body|<list|<macro|name|<aligned-item|<with|font-series|bold|-->>>|<macro|x|<arg|x>>|<arg|body>>>>

  <assign|itemize-2|<macro|body|<list|<macro|name|<aligned-item|<math|\<bullet\>>>>|<macro|x|<arg|x>>|<arg|body>>>>

  <assign|itemize-reduce|<macro|nr|<minimum|<arg|nr>|2>>>

  <active*|<src-comment|Enumerate lists>>

  <active*|<src-comment|Counter rendering>>

  <active*|<src-comment|Customization of environments>>

  <active*|<src-comment|Headers and footers>>

  <active*|<src-comment|Rendering of floating objects>>

  <active*|<src-comment|Title rendering>>

  <active*|<src-comment|Bibliography>>

  <active*|<src-comment|Miscellaneous macros provided by the style>>

  <active*|<src-comment|Tables of contents>>

  <active*|<src-comment|Indexes and glossaries>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>