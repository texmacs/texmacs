<TeXmacs|1.0.7.12>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|title-base|1.0|header-title|1.0>

    <\src-purpose>
      Common macros for title information
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

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
      DRD properties of tags with title and author data.

      FIXME: running title and author should be made ``hidden''.
    </src-comment>
  </active*>

  <drd-props|doc-data|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|doc-title|border|no>

  <drd-props|doc-subtitle|border|no>

  <drd-props|doc-author-data|arity|<tuple|repeat|1|1>|border|no|accessible|all>

  <drd-props|doc-date|border|no>

  <drd-props|doc-note|arity|1|border|no|accessible|all>

  <drd-props|doc-inactive|arity|1|border|no|accessible|all>

  <drd-props|doc-running-title|arity|1|accessible|all>

  <drd-props|doc-running-author|arity|1|accessible|all>

  <drd-props|doc-keywords|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|doc-AMS-class|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|author-name|border|no>

  <drd-props|author-address|border|no>

  <drd-props|author-email|border|no>

  <drd-props|author-homepage|border|no>

  <drd-props|author-note|arity|1|border|no|accessible|all>

  <drd-props|abstract|arity|1|accessible|all>

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

  <assign|doc-title|<macro|body|<doc-render-title|<style-with|src-compact|none|<arg|body><doc-footnote-ref|<quasi|<doc-data-note|<unquote*|<quote-value|the-doc-data>>>>>>>>>

  <assign|doc-subtitle|<macro|x|<\surround|<vspace*|0.25fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.297|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|doc-author|<macro|body|<\surround|<vspace*|1fn>|<vspace|1fn>>
    <\with|par-par-sep|0fn>
      <doc-title-block|<arg|body>>
    </with>
  </surround>>>

  <assign|doc-authors|<\macro|body>
    <\doc-author>
      <font-magnify|0.917|<arg|body>>
    </doc-author>
  </macro>>

  <assign|doc-date|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><doc-title-block|<with|font-shape|italic|<arg|body>>><vspace|0.5fn>>>>

  <assign|doc-running-title|<macro|body|<if|<unequal|<arg|body>|<uninit>>|<header-title|<arg|body>>>>>

  <assign|doc-running-author|<macro|body|<if|<unequal|<arg|body>|<uninit>>|<header-author|<arg|body>>>>>

  <assign|doc-inactive|<\macro|body>
    <\quasi>
      <\surround|<vspace*|0.5fn>|>
        <doc-title-block|<style-with|src-compact|none|<inline-tag|<unquote|<get-label|<quote-arg|body>>>|<unquote*|<quote-arg|body>>>>>
      </surround>
    </quasi>
  </macro>>

  <active*|<src-short-comment|For backward compatability>>

  <assign|address-block|<macro|x|<tformat|<cwith|1|-1|1|1|cell-lsep|1.5fn>|<cwith|1|-1|-1|-1|cell-rsep|1.5fn>|<twith|table-valign|T>|<arg|x>>>>

  <\active*>
    <\src-comment>
      Rendering the abstract.
    </src-comment>
  </active*>

  <assign|doc-abstract|<\macro|body>
    <\padded-normal|2fn|1fn>
      <\with|par-left|15mm|par-right|15mm>
        <\small>
          <sectional-centered-bold|<abstract-text>><vspace|0.5fn>

          <arg|body>
        </small>
      </with>
    </padded-normal>
  </macro>>

  <assign|doc-abstract*|<\macro|body|note>
    <\quasi>
      <\doc-abstract>
        <surround||<vspace|0.5fn>|<unquote|<quote-arg|body>>>

        <\with|par-par-sep|0.25fn>
          <unquote*|<arg|note>>
        </with>
      </doc-abstract>
    </quasi>
  </macro>>

  <assign|doc-keywords|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<keywords-text><localize|:>
  ><concat-tuple|<copy|<quote-arg|args>>|, >>>>

  <assign|msc-ref|<macro|msc-id|<style-with|src-compact|none|<hlink|<arg|msc-id>|<style-with|src-compact|none|<merge|http://www.ams.org/mathscinet/search/mscbrowse.html?sk=default&sk=|<arg|msc-id>|&submit=Search>>>>>>

  <assign|doc-AMS-class|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<AMS-class-text><localize|:>
  ><concat-tuple|<copy|<map|msc-ref|<quote-arg|args>>>|, >>>>

  \;

  <assign|abstract|<\macro|body>
    <style-with|src-compact|none|<with|abstract-note|<look-up|<quasi|<doc-data-abstract|<unquote*|<quote-value|the-doc-data>>>>|0>|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<equal|<get-arity|<quote-value|abstract-note>>|0>|doc-abstract|doc-abstract*>>|<arg|body>|<quote-value|abstract-note>>>>>
  </macro>>

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
      Document titles.
    </src-comment>
  </active*>

  <assign|doc-data-main|<\xmacro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|doc-title>>

      <unquote*|<select|<quote-arg|data>|doc-subtitle>>

      <unquote*|<select|<quote-arg|data>|doc-author-data>>

      <unquote*|<select|<quote-arg|data>|doc-date>>

      <unquote*|<select|<quote-arg|data>|doc-inactive>>
    </quasi>
  </xmacro>>

  <assign|doc-data-main*|<\xmacro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|doc-title>>

      <unquote*|<select|<quote-arg|data>|doc-subtitle>>

      <doc-authors-data|<unquote*|<select|<quote-arg|data>|doc-author-data>>>

      <unquote*|<select|<quote-arg|data>|doc-date>>

      <unquote*|<select|<quote-arg|data>|doc-inactive>>
    </quasi>
  </xmacro>>

  <assign|doc-note|<macro|body|>><active*|<src-short-comment|Added as a
  temporary fix for problem with doc-note>>

  <assign|doc-data-hidden|<xmacro|data|<quasi|<style-with|src-compact|none|<style-with|src-compact|none|<doc-note|<unquote*|<select|<quote-arg|data>|doc-note>>>><doc-data-bis|<unquote*|<quote-arg|data>>><doc-authors-data-bis|<unquote*|<select|<quote-arg|data>|doc-author-data>>><style-with|src-compact|none|<doc-running-title|<unquote*|<select|<quote-arg|data>|doc-title|0>>>><style-with|src-compact|none|<doc-running-title|<unquote*|<select|<quote-arg|data>|doc-running-title|0>>>><doc-running-author|<style-with|src-compact|none|<author-from-authors|<unquote*|<select|<quote-arg|data>|doc-author-data|author-name|0>>>>><style-with|src-compact|none|<doc-running-author|<unquote*|<select|<quote-arg|data>|doc-running-author|0>>>>>>>>

  <assign|doc-data-abstract|<\xmacro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|doc-keywords>>

      <unquote*|<select|<quote-arg|data>|doc-AMS-class>>
    </quasi>
  </xmacro>>

  <assign|doc-data-note|<xmacro|data|<\quasi>
    <unquote*|<select|<quote-arg|data>|doc-note|document|<pat-any>>>
  </quasi>>>

  <assign|doc-data|<\xmacro|data>
    <style-with|src-compact|none|<\surround|<assign|the-doc-data|<quote-arg|data>>|<with|doc-note-nr|0|<quasi|<doc-data-hidden|<unquote*|<quote-arg|data>>>>>>
      <\doc-make-title>
        <with|doc-note-nr|0|<\quasi>
          <style-with|src-compact|none|<compound|<unquote|<style-with|src-compact|none|<if|<lesseq|<length|<select|<quote-arg|data>|doc-author-data>>|1>|<value|doc-data-main>|<value|doc-data-main*>>>>|<unquote*|<quote-arg|data>>>>
        </quasi>>
      </doc-make-title>
    </surround>>
  </xmacro>>

  <assign|doc-data-bis|<xmacro|body|<quasi|<style-with|src-compact|none|<doc-title-note|<unquote|<quasi|<doc-data-note|<unquote*|<quote-arg|body>>>>>>>>>>

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

  <assign|author-from-authors*|<macro|x|, <arg|x>>>

  <assign|author-from-authors|<xmacro|x|<style-with|src-compact|none|<if|<unequal|<get-arity|<quote-arg|x>>|0>|<arg|x|0><map-args|author-from-authors*|concat|x|1>>>>>

  <assign|author-render-name|<macro|author|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<name|<arg|author>>>>>>

  <assign|author-name|<macro|author|<author-render-name|<style-with|src-compact|none|<author-by|<arg|author>><style-with|src-compact|none|<doc-footnote-ref|<quasi|<doc-author-data-note|<unquote*|<quote-value|the-author-data>>>>>>>>>>

  <assign|author-address|<\macro|address>
    <surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<arg|address>>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<style-with|src-compact|none|<with|font-shape|italic|<email-text><localize|:>
  ><with|font-family|tt|<arg|email>>>>>>

  <assign|author-homepage|<macro|homepage|<doc-author-block|<style-with|src-compact|none|<with|font-shape|italic|<homepage-text><localize|:>
  ><with|font-family|tt|<arg|homepage>>>>>>

  <assign|author-note|<\macro|note>
    <arg|note>
  </macro>>

  <\active*>
    <\src-comment>
      Documents with one author.
    </src-comment>
  </active*>

  <assign|doc-author-main|<\macro|data>
    <\quasi>
      <unquote*|<select|<quote-arg|data>|author-name>>

      <unquote*|<select|<quote-arg|data>|author-address>>

      <unquote*|<select|<quote-arg|data>|author-email>>

      <unquote*|<select|<quote-arg|data>|author-homepage>>
    </quasi>
  </macro>>

  <assign|doc-author-data-note|<xmacro|data|<\quasi>
    <unquote*|<select|<quote-arg|data>|author-note|document|<pat-any>>>
  </quasi>>>

  <assign|doc-author-data|<\xmacro|data>
    <\quasi>
      <\with|the-author-data|<quote-arg|data>>
        <\doc-author>
          <doc-author-block|<doc-author-main|<unquote|<quote-arg|data>>>>
        </doc-author>
      </with>
    </quasi>
  </xmacro>>

  <assign|doc-author-data-bis|<macro|body|<quasi|<style-with|src-compact|none|<doc-author-note|<unquote|<quasi|<doc-author-data-note|<unquote*|<quote-arg|body>>>>>>>>>>

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

  <assign|doc-author-data*|<macro|data|<quasi|<style-with|src-compact|none|<space|1em><with|the-author-data|<quote-arg|data>|<style-with|src-compact|none|<doc-authors-block|<doc-author-main|<unquote|<quote-arg|data>>>>>><space|1em><line-break>>>>>

  <assign|doc-authors-data|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <\with|author-by|<macro|body|<arg|body>>>
          <\doc-authors>
            <space|0spc><unquote*|<map|doc-author-data*|<quote-arg|data>>>
          </doc-authors>
        </with>
      </quasi>
    </style-with>
  </xmacro>>

  <assign|doc-authors-data-bis|<xmacro|x|<style-with|src-compact|none|<quasi|<space|0spc><unquote*|<map|doc-author-data-bis|<quote-arg|x>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>