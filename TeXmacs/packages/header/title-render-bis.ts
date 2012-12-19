<TeXmacs|1.0.7.17>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <compound|src-package|title-render|1.0>

    <\src-purpose>
      Common macros for rendering title information
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(database title-bis)>

  <\active*>
    <\src-comment>
      Special texts.
    </src-comment>
  </active*>

  <assign|by-text|<macro|<localize|by>>>

  <assign|email-text|<macro|<localize|Email>>>

  <assign|homepage-text|<macro|<localize|Web>>>

  <assign|abstract-text|<macro|<localize|Abstract>>>

  <assign|keywords-text|<macro|<localize|Keywords>>>

  <assign|AMS-class-text|<macro|<localize|A.M.S. subject classification>>>

  <assign|with-TeXmacs-text|<macro|This document has been written using the
  GNU <TeXmacs> text editor (see <hlink|<with|font-family|tt|www.texmacs.org>|http://www.texmacs.org>).>>

  <\active*>
    <\src-comment>
      Rendering the title.
    </src-comment>
  </active*>

  <assign|doc-title-block|<macro|body|<tabular*|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <\with|par-mode|center>
      <arg|body>
    </with>
  </cell>>>>>>>

  <assign|doc-make-title|<macro|body|<surround||<vspace|2fn>|<doc-title-block|<arg|body>>>>>

  <assign|doc-render-title|<macro|x|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.682|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|doc-subtitle|<macro|x|<\surround|<vspace*|0.25fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.297|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|doc-date|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><doc-title-block|<with|font-shape|italic|<arg|body>>><vspace|0.5fn>>>>

  <assign|doc-running-title|<macro|body|<if|<unequal|<arg|body>|<uninit>>|<header-title|<arg|body>>>>>

  <assign|doc-running-author|<macro|body|<if|<unequal|<arg|body>|<uninit>>|<header-author|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Rendering author infomation.
    </src-comment>
  </active*>

  <assign|doc-author-block|<\macro|body>
    <style-with|src-compact|none|<space|0pt><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-width|1par>|<table|<row|<\cell>
      <\with|par-mode|center>
        <arg|body>
      </with>
    </cell>>>>>>
  </macro>>

  <assign|author-by|<macro|body|<by-text> <arg|body>>>

  <assign|author-render-name|<macro|author|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<name|<arg|author>>>>>>

  <assign|author-affiliation|<\macro|address>
    <surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<arg|address>>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<style-with|src-compact|none|<with|font-shape|italic|<email-text><localize|:>
  ><with|font-family|tt|<arg|email>>>>>>

  <assign|author-homepage|<macro|homepage|<doc-author-block|<style-with|src-compact|none|<with|font-shape|italic|<homepage-text><localize|:>
  ><with|font-family|tt|<arg|homepage>>>>>>

  <assign|author-misc|<\macro|note>
    <arg|note>
  </macro>>

  <assign|render-doc-author|<macro|body|<\surround|<vspace*|1fn>|<vspace|1fn>>
    <\with|par-par-sep|0fn>
      <doc-title-block|<arg|body>>
    </with>
  </surround>>>

  <\active*>
    <\src-comment>
      Documents with several authors.
    </src-comment>
  </active*>

  <assign|doc-authors-block|<\macro|body>
    <style-with|src-compact|none|<space|0pt><tabular*|<tformat|<twith|table-valign|T>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0.75fn>|<cwith|1|1|1|1|cell-tsep|0.75fn>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-width|1par>|<table|<row|<\cell>
      <\with|par-mode|center>
        <arg|body>
      </with>
    </cell>>>>>>
  </macro>>

  <assign|padded-author|<macro|data|<quasi|<style-with|src-compact|none|<space|1em><authors-data|<unquote|<quote-arg|data>>><space|1em><line-break>>>>>

  <assign|render-doc-authors|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <\render-doc-author>
          <with|author-by|<macro|body|<arg|body>>|<\font-magnify|0.917>
            <space|0spc><unquote*|<map|padded-author|<quote-arg|data>>>
          </font-magnify>>
        </render-doc-author>
      </quasi>
    </style-with>
  </xmacro>>

  <\active*>
    <\src-comment>
      Rendering the abstract.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\padded-normal|2fn|1fn>
      <\with|par-left|15mm|par-right|15mm>
        <\small>
          <sectional-centered-bold|<abstract-text>><vspace|0.5fn>

          <arg|body>
        </small>
      </with>
    </padded-normal>
  </macro>>

  <assign|doc-keywords|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<keywords-text><localize|:>
  ><concat-tuple|<quote-arg|args>|, >>>>

  <assign|msc-ref|<macro|msc-id|<style-with|src-compact|none|<hlink|<arg|msc-id>|<style-with|src-compact|none|<merge|http://www.ams.org/mathscinet/search/mscbrowse.html?sk=default&sk=|<arg|msc-id>|&submit=Search>>>>>>

  <assign|doc-msc|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<AMS-class-text><localize|:>
  ><concat-tuple|<map|msc-ref|<quote-arg|args>>|, >>>>

  <\active*>
    <\src-comment>
      Rendering footnotes.
    </src-comment>
  </active*>

  <new-counter|doc-note>

  <assign|the-doc-note|<macro|<number|<value|doc-note-nr>|fnsymbol>>>

  <assign|doc-author-note-next|<macro|<inc-doc-note><the-doc-note>>>

  \;

  <assign|doc-footnote-ref|<macro|body|<style-with|src-compact|none|<if|<quasi|<unequal|<get-arity|<unquote|<quote-arg|body>>>|0>>|<rsup|<doc-author-note-next>>>>>>

  <assign|doc-footnote-sub|<macro|x|; <arg|x>>>

  <assign|doc-footnote|<macro|body|<style-with|src-compact|none|<if|<unequal|<get-arity|<quote-arg|body>>|0>|<quasi|<style-with|src-compact|none|<render-footnote|<unquote|<doc-author-note-next>>|<arg|body|0><map-args|doc-footnote-sub|concat|body|1>>>>>>>>

  \;

  <assign|doc-title-note|<macro|body|<quasi|<doc-footnote|<unquote|<quote-arg|body>>>>>>

  <assign|doc-author-note|<macro|body|<quasi|<doc-footnote|<unquote|<quote-arg|body>>>>>>

  <\active*>
    <\src-comment>
      Miscellaneous.
    </src-comment>
  </active*>

  <assign|doc-inactive|<\macro|body>
    <\quasi>
      <\surround|<vspace*|0.5fn>|>
        <doc-title-block|<style-with|src-compact|none|<inline-tag|<unquote|<get-label|<quote-arg|body>>>|<unquote*|<quote-arg|body>>>>>
      </surround>
    </quasi>
  </macro>>

  <active*|<src-short-comment|For backward compatability>>

  <assign|address-block|<macro|x|<tformat|<cwith|1|-1|1|1|cell-lsep|1.5fn>|<cwith|1|-1|-1|-1|cell-rsep|1.5fn>|<twith|table-valign|T>|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|zoom-factor|1.35021>
  </collection>
</initial>