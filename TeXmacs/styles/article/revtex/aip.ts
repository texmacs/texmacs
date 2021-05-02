<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|aip|1.0>

      <\src-purpose>
        The Revtex's American Institude of Physic style.
      </src-purpose>

      <\src-copyright|2012--2012>
        Joris van der Hoeven, François Poulain
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env|header-generic|section-book|title-generic|std-latex|cite-author-year|std-automatic|two-columns|html-font-size>

  <active*|<\src-comment>
    Page layout.
  </src-comment>>

  <assign|par-first|1em>

  <assign|parindent|<value|par-first>>

  <assign|page-type|a4>

  <assign|par-columns|2>

  <assign|tex-odd-side-margin|0pt>

  <assign|tex-even-side-margin|0pt>

  <assign|tex-margin-par-width|60pt>

  <assign|tex-margin-par-sep|10pt>

  <assign|tex-top-margin|-37pt>

  <assign|tex-head-height|12pt>

  <assign|tex-head-sep|25pt>

  <assign|tex-top-skip|25pt>

  <assign|tex-text-height|665.5pt>

  <assign|tex-text-width|468pt>

  <assign|tex-column-sep|10pt>

  <assign|tex-footnote-sep|10pt>

  <assign|tex-float-sep|<texlen|14pt|2pt|4pt>>

  <assign|tex-text-float-sep|<texlen|20pt|2pt|4pt>>

  <assign|tex-in-text-sep|<texlen|14pt|2pt|4pt>>

  <active*|<\src-comment>
    Sectionning macros.
  </src-comment>>

  <style-with|src-compact|none|<assign|part-title|<style-with|src-compact|none|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|4ex><really-huge|<arg|name>><vspace|3ex><new-line>>>>>>>

  <style-with|src-compact|none|<assign|<style-with|src-compact|none|part-numbered-title>|<style-with|src-compact|none|<macro|<style-with|src-compact|all|name>|<style-with|src-compact|none|<sectional-short-bold|<vspace*|4ex><very-large|<part-text>
  <the-part><new-line><vspace*|<value|parindent>>><huge|<arg|name>><vspace|3ex><new-line>>>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|<tex-len|0.8cm|1ex|0.2ex>><with|font-family|ss|<small|<change-case|<arg|name>|UPCASE>>><vspace|0.5cm>>>>>

  <assign|section-numbered-title|<macro|name|<section-title|<sectional-prefixed|<the-section><section-sep>|<change-case|<arg|name>|UPCASE><section-post-sep>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tex-len|0.8cm|1ex|0.2ex>><with|font-family|ss|<small|<arg|name>>><vspace|0.5cm>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tex-len|0.8cm|1ex|0.2ex>><with|font-family|ss|<small|<arg|name>>><vspace|0.5cm>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-italic|<space|<parindent>><normal-size|<arg|name>><space|<value|parindent>><vspace|-1em>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|<tex-len|3.25ex|1ex|0.2ex>><space|<value|parindent>><normal-size|<arg|name>><space|<value|parindent>><vspace|-1em>>>>>

  <assign|appendix-text|Appendix>

  <assign|appendix-numbered-title|<\macro|title>
    <assign|display-equation|<macro|nr|<the-appendix><number|<arg|nr>|arabic>>>

    <assign|subsection-display|arabic><subsection-title|<appendix-text>
    <the-appendix>: <arg|title>>
  </macro>>

  <active*|<\src-comment>
    Section and environment numbering.
  </src-comment>>

  <assign|sectional-sep|<macro|<space|1.1fn>>>

  <assign|section-clean|<macro|<reset-subsection>>>

  <style-with|src-compact|all|<assign|display-std-env|<\macro|nr>
    <arg|nr>
  </macro>>>

  <assign|part-display-numbers|<macro|true>>

  <assign|section-display-numbers|<macro|true>>

  <assign|subsection-display-numbers|<macro|true>>

  <assign|subsubsection-display-numbers|<macro|true>>

  <assign|paragraph-display-numbers|<macro|true>>

  <assign|subparagraph-display-numbers|<macro|false>>

  <assign|display-section|<macro|nr|<number|<arg|nr>|Roman>.>>

  <assign|subsection-display|Alpha>

  <assign|display-subsection|<macro|nr|<number|<arg|nr>|<subsection-display>>.>>

  <assign|display-subsubsection|<macro|nr|<arg|nr>.>>

  <assign|display-paragraph|<macro|nr|<number|<arg|nr>|alpha>.>>

  <assign|display-subparagraph|>

  <active*|<\src-comment>
    Headers and footers.
  </src-comment>>

  <set-header|<no-indent><tabular|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|1|cell-halign|r>|<table|<row|<cell|<page-number>>>>>>>

  <set-footer|>

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <assign|start-page|<macro|s|>>

  <active*|<\src-comment>
    Footnotes.
  </src-comment>>

  <assign|footnote-sep|>

  <assign|display-footnote|<macro|nr|<number|<arg|nr>|alpha>>>

  <assign|render-footnote|<macro|nr|body|<style-with|src-compact|none|<\float|footnote|>
    <smaller|<with|par-mode|justify|par-left|0cm|par-right|0cm|<style-with|src-compact|none|<surround|<rsup|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<id|<hard-id|<arg|body>>>|<url|<merge|#footnr-|<arg|nr>>>>|<arg|nr>>)><footnote-sep>|<set-binding|<merge|footnote-|<arg|nr>>|<value|the-label>|body><right-flush>|<style-with|src-compact|none|<arg|body>>>>>>
  </float>>>>

  <assign|footnote|<macro|body|<style-with|src-compact|none|<next-footnote><render-footnote|<the-footnote>|<arg|body>><space|0spc><label|<merge|footnr-|<the-footnote>>><rsup|<reference|<merge|footnote-|<the-footnote>>>)>>>>

  <\active*>
    <\src-comment>
      List environments.
    </src-comment>
  </active*>

  <assign|render-list|<macro|body|<surround|<no-page-break*>|<right-flush><no-indent*>|<with|par-left|<plus|<value|<unquote|<merge|left-margin-|<plus|<itemize-level>|<enumerate-level>>>>>|<value|par-left>>|par-par-sep|<value|<unquote|<merge|par-par-sep-|<plus|<itemize-level>|<enumerate-level>>>>>|<arg|body>>>>>

  \;

  <assign|item-1|<macro|<active*|<with|mode|math|\<bullet\>>>>>

  <assign|item-2|<macro|<active*|<with|mode|math|<rigid|->>>>>

  <assign|item-3|<macro|<active*|<math|\<ast\>>>>>

  <assign|item-4|<macro|<active*|<math|\<cdot\>>>>>

  <new-list|itemize-1|<value|aligned-item>|<macro|name|<item-tag>>>

  <new-list|itemize-2|<value|aligned-item>|<macro|name|<item-tag>>>

  <new-list|itemize-3|<value|aligned-item>|<macro|name|<item-tag>>>

  <new-list|itemize-4|<value|aligned-item>|<macro|name|<item-tag>>>

  \;

  <assign|item-hsep|<macro|6pt>>

  \;

  <assign|left-margin-0|24pt>

  <assign|left-margin-1|24pt>

  <assign|left-margin-2|20pt>

  <assign|left-margin-3|16pt>

  <assign|left-margin-4|14pt>

  <assign|left-margin-5|6pt>

  <assign|left-margin-6|6pt>

  \;

  <assign|top-sep-0|<tex-len|10pt|4pt|6pt>>

  <assign|top-sep-1|<tex-len|10pt|4pt|6pt>>

  <assign|top-sep-2|<tex-len|5pt|2.5pt|1pt>>

  <assign|top-sep-3|<tex-len|2.5pt|1pt|1pt>>

  \;

  <assign|par-par-sep-0|<tex-len|5pt|2.5pt|1pt>>

  <assign|par-par-sep-1|<tex-len|5pt|2.5pt|1pt>>

  <assign|par-par-sep-2|<tex-len|2.5pt|1pt|1pt>>

  <assign|par-par-sep-3|<tex-len|0pt|1pt|1pt>>

  \;

  <assign|enumerate-level|0>

  <assign|enum-1|<macro|name|<number|<arg|name>|arabic>>>

  <assign|enum-2|<macro|name|<number|<arg|name>|alpha>>>

  <assign|enum-3|<macro|name|<number|<arg|name>|roman>>>

  <assign|enum-4|<macro|name|<number|<arg|name>|Alpha>>>

  <new-list|enumerate-1|<value|aligned-dot-item>|<macro|name|<enum-tag|<arg|name>>>>

  <new-list|enumerate-2|<value|aligned-item>|<macro|name|<enum-tag|<arg|name>>>>

  <new-list|enumerate-3|<value|aligned-dot-item>|<macro|name|<enum-tag|<arg|name>>>>

  <new-list|enumerate-4|<value|aligned-dot-item>|<macro|name|<enum-tag|<arg|name>>>>

  <assign|enumerate-reduce|<macro|nr|<plus|<mod|<minus|<arg|nr>|1>|4>|1>>>

  <active*|<\src-comment>
    Rendering of tables.
  </src-comment>>

  <assign|table-text|<macro|<localize|TABLE>>>

  <assign|list-of-tables-text|<macro|<localize|List of Tables>>>

  <assign|the-table|<macro|<number|<value|table-nr>|Roman>>>

  <assign|ruledtabular|<macro|body|<with|tabular|<value|ruled-tabular>|tabular*|<value|ruled-tabular*>|<arg|body>>>>

  <assign|table*|<macro|body|<with|par-columns|1|<arg|body>>>>

  <assign|ruled-tabular|<macro|body|<tformat|<twith|table-width|1par>|<cwith|1|1|1|-1|cell-bborder|0.5pt>|<cwith|1|1|1|-1|cell-tborder|1pt>|<cwith|-1|-1|1|-1|cell-bborder|1pt>|<arg|body>>>>

  <assign|ruled-tabular*|<macro|body|<with|par-columns|1|<tformat|<twith|table-width|1par>|<cwith|1|1|1|-1|cell-bborder|0.5pt>|<cwith|1|1|1|-1|cell-tborder|1pt>|<cwith|-1|-1|1|-1|cell-bborder|1pt>|<arg|body>>>>>

  <assign|big-table-enlarge|<macro|contents|body|<if|<greater|<times|2|<look-up|<box-info|<arg|contents>|w>|0>>|<look-up|<box-info|<space|<tex-text-width>>|w>|0>>|<with|par-columns|1|<arg|body>>|<arg|body>>>>

  <assign|big-table|<\macro|body|caption>
    <\big-table-enlarge|<arg|body>>
      <\surround|<compound|next-table>|>
        <\render-big-table>
          table
        </render-big-table|<compound|table-text>
        <compound|the-table>|<arg|body>|<arg|caption>>
      </surround>
    </big-table-enlarge>
  </macro>>

  <assign|render-big-table|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<tabular*|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<cwith|1|1|1|1|cell-lsep|1.5fn>|<cwith|1|1|1|1|cell-rsep|1.5fn>|<table|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </cell>>|<row|<cell|>>|<row|<cell|<arg|fig>>>>>>>
  </macro>>

  <active*|<\src-comment>
    Rendering of figures.
  </src-comment>>

  <assign|figure-text|<macro|<localize|FIG.>>>

  <assign|figure*|<macro|body|<with|par-columns|1|<arg|body>>>>

  <assign|list-of-figures-text|<macro|<localize|List of Figures>>>

  <assign|big-figure|<\macro|body|caption>
    <\surround|<next-figure>|>
      <render-big-figure|figure|<figure-text>
      <the-figure>|<arg|body>|<arg|caption>>
    </surround>
  </macro>>

  <assign|figure-name|<macro|name|<arg|name>>>

  <assign|caption-left-padding|0fn>

  <assign|caption-right-padding|0fn>

  <assign|figure-width|1par>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<tabular*|<tformat|<twith|table-width|<value|figure-width>>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|<value|figure-left-padding>>|<cwith|1|-1|1|-1|cell-rsep|<value|figure-right-padding>>|<cwith|2|2|1|1|cell-height|<value|figure-caption-sep>>|<cwith|3|3|1|1|cell-lsep|<value|caption-left-padding>>|<cwith|3|3|1|1|cell-rsep|<value|caption-right-padding>>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </cell>>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Bibliography.
    </src-comment>
  </active*>

  <assign|bibitem-width|2em>

  <assign|transform-bibitem|<macro|body|[<arg|body>] >>

  <assign|xtransform-bibitem|<macro|body|<rsup|<arg|body>>>>

  <assign|xrender-bibitem|<macro|text|<style-with|src-compact|none|<with|par-first|<minus|1tmpt|<value|bibitem-width>>|<yes-indent>><hspace|<maximum|0cm|<minus|<value|bibitem-width>|<box-info|<arg|text>|w.>>>><arg|text>>>>

  <assign|bib-list|<\macro|largest|body>
    <\small>
      <\with|bibitem-width|<minimum|<box-info|<transform-bibitem|<arg|largest>>.|w.>|2em>|item-hsep|<value|bibitem-hsep>|bibitem-nr|0|par-flexibility|2.0>
        <\description>
          <arg|body>
        </description>
      </with>
    </small>
  </macro>>

  <assign|bibliography-text|<macro|<rule|9cm|1pt>>>

  <assign|bibliography-text|<\macro>
    <\with|par-columns|1>
      <tabular|<tformat|<cwith|1|-1|1|-1|cell-width|1cm>|<cwith|1|-1|1|-1|cell-hmode|exact>|<cwith|1|-1|1|-1|cell-height|0.1pt>|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|1|4|6|cell-bborder|0.7pt>|<cwith|1|1|3|3|cell-bborder|0.5pt>|<cwith|1|1|7|7|cell-bborder|0.5pt>|<cwith|1|1|8|8|cell-bborder|0.4pt>|<cwith|1|1|2|2|cell-bborder|0.4pt>|<cwith|1|1|1|1|cell-bborder|0.3pt>|<cwith|1|1|9|9|cell-bborder|0.3pt>|<table|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>|<cell|>>>>>
    </with>
  </macro>>

  <assign|bibliography-text|<macro|<localize|References>>>

  <assign|render-bibliography|<\macro|name|body>
    <section*|<arg|name>>

    <arg|body>
  </macro>>

  <active*|<\src-comment>
    Title rendering.
  </src-comment>>

  <assign|doc-footnote-ref|<macro|body|<style-with|src-compact|none|<if|<quasi|<unequal|<get-arity|<unquote|<quote-arg|body>>>|0>>|<with|font-family|rm|<rsup|<doc-author-note-next>)>>>>>>

  <assign|the-doc-note|<macro|<number|<value|doc-note-nr>|alpha>>>

  <assign|by-text|<macro|<localize|>>>

  <assign|author-render-name|<macro|author|<doc-author-block|<with|font-family|ss|<arg|author>>>>>

  <assign|author-affiliation|<\macro|address>
    <surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<with|font-shape|italic|<arg|address>>>>
  </macro>>

  <assign|doc-author-main|<\macro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|author-name>>

      <unquote*|<select|<quote-arg|data>|author-affiliation>>
    </quasi>
  </macro>>

  <assign|doc-author-data-note|<xmacro|data|<\quasi>
    <unquote*|<select|<quote-arg|data>|author-homepage|<pat-any>>>

    <unquote*|<select|<quote-arg|data>|author-note|document|<pat-any>>>
  </quasi>>>

  <assign|doc-date|<macro|body|<doc-author-block|(Dated: <arg|body>)>>>

  <assign|abstract-text|>

  <assign|render-abstract|<macro|body|<surround||<vspace|2fn>|<doc-author-block|<arg|body>>>>>

  <assign|doc-authors-block|<macro|body|<doc-author-block|<arg|body>>>>

  <assign|doc-author-block|<\macro|body>
    <\with|par-columns|1>
      <\with|par-mode|right>
        <tabular|<tformat|<cwith|1|1|1|1|cell-width|400pt>|<cwith|1|1|1|1|cell-hmode|exact>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
          <arg|body>
        </cell>>>>>
      </with>
    </with>
  </macro>>

  <assign|doc-data|<xmacro|args|<with|par-columns|1|par-first|0em|<extern|doc-data|<quote-arg|args>|>>>>

  <assign|doc-render-title|<macro|x|<\surround|<vspace*|0.5fn>|>
    <doc-title-block|<font-magnify|1.682|<with|math-font-series|bold|font-series|bold|font-family|ss|<arg|x>>>>
  </surround>>>

  <assign|doc-title|<value|doc-render-title>>

  <assign|doc-title-block|<macro|body|<arg|body>>>

  <assign|doc-authors-data|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <unquote*|<quote-arg|data>>
      </quasi>
    </style-with>
  </xmacro>>

  <assign|render-doc-author|<macro|body|<arg|body>>>

  <assign|render-doc-authors|<macro|body|<arg|body>>>

  <active*|<\src-comment>
    Customization of other environments.
  </src-comment>>

  <assign|table-of-contents-text|<macro|<localize|Contents>>>

  <active*|<\src-comment>
    Specific macros from Revtex style.
  </src-comment>>

  <style-with|src-compact|none|<assign|widetext|<macro|body|<\with|par-columns|1>
    <tabular|<tformat|<twith|table-hmode|exact>|<twith|table-width|1par>|<cwith|1|1|1|-1|cell-height|1ex>|<cwith|1|1|1|-1|cell-vmode|exact>|<cwith|1|-1|1|1|cell-bborder|0.1pt>|<cwith|1|-1|1|1|cell-rborder|0.1pt>|<table|<row|<cell|>|<cell|>>>>>

    <arg|body>

    <tabular|<tformat|<twith|table-hmode|exact>|<twith|table-width|1par>|<cwith|1|1|1|2|cell-height|1ex>|<cwith|1|1|1|2|cell-vmode|exact>|<cwith|1|1|2|2|cell-lborder|0.1pt>|<cwith|1|1|2|2|cell-tborder|0.1pt>|<table|<row|<cell|>|<cell|>>>>>
  </with>>>>

  <assign|acknowledgments*|<\macro|body>
    <section*|<localize|Acknowledgments>>

    <arg|body>
  </macro>>

  <active*|<\src-comment>
    Font sizes and AIP layout from Revtex.
  </src-comment>>

  <use-package|<merge|revtex-|<value|font-base-size>|pt>>
</body>

<initial|<\collection>
</collection>>