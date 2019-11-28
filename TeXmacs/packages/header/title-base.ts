<TeXmacs|1.99.11>

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

  <use-module|(database title-markup)>

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

  <assign|PACS-class-text|<macro|<localize|P.A.C.S. subject classification>>>

  <assign|ACM-class-text|<macro|<localize|A.C.M. subject classification>>>

  <assign|arXiv-class-text|<macro|<localize|ArXiv subject classification>>>

  <assign|with-TeXmacs-text|<macro|This document has been written using the
  GNU <TeXmacs> text editor (see <hlink|<with|font-family|tt|www.texmacs.org>|http://www.texmacs.org>).>>

  <assign|noteref-sep|<macro|>>

  <assign|cite-website|<macro|<localize|This document has been written using>
  GNU <TeXmacs><if|<equal|<value|language>|french>| ; |; ><localize|see>
  <hlink|<with|font-family|tt|www.texmacs.org>|http://www.texmacs.org>.>>

  <assign|cite-TeXmacs|<xmacro|keys|<localize|This document has been written
  using> GNU <TeXmacs><nbsp><map-args|identity|cite|keys>.>>

  <\active*>
    <\src-comment>
      DRD properties of tags with title and author data.

      FIXME: running title and author should be made ``hidden''.
    </src-comment>
  </active*>

  <drd-props|doc-data|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|doc-title|border|no>

  <drd-props|doc-subtitle|border|no>

  <drd-props|doc-author|arity|<tuple|repeat|1|1>|border|no|accessible|all>

  <drd-props|doc-title-options|arity|<tuple|repeat|1|1>|border|no|accessible|no>

  <drd-props|doc-misc|border|no>

  <drd-props|doc-date|border|no>

  <drd-props|doc-note|arity|1|border|no|accessible|all>

  <drd-props|doc-inactive|arity|1|border|no|accessible|all>

  <drd-props|doc-running-title|arity|1|accessible|all>

  <drd-props|doc-running-author|arity|1|accessible|all>

  <drd-props|author-data|arity|<tuple|repeat|1|1>|border|no|accessible|all>

  <drd-props|author-name|border|no>

  <drd-props|author-affiliation|border|no>

  <drd-props|author-name-affiliation|border|no>

  <drd-props|author-email|border|no>

  <drd-props|author-homepage|border|no>

  <drd-props|author-misc|border|no>

  <drd-props|author-note|arity|1|border|no|accessible|all>

  <drd-props|abstract-data|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|abstract|arity|1|accessible|all|border|no>

  <drd-props|abstract-msc|arity|<tuple|repeat|1|1>|accessible|all|border|no>

  <drd-props|abstract-acm|arity|<tuple|repeat|1|1>|accessible|all|border|no>

  <drd-props|abstract-arxiv|arity|<tuple|repeat|1|1>|accessible|all|border|no>

  <drd-props|abstract-pacs|arity|<tuple|repeat|1|1>|accessible|all|border|no>

  <drd-props|abstract-keywords|arity|<tuple|repeat|1|1>|accessible|all|border|no>

  <\active*>
    <\src-comment>
      Document titles.
    </src-comment>
  </active*>

  <assign|doc-title-block|<macro|body|<tabular*|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <\with|par-mode|center>
      <arg|body>
    </with>
  </cell>>>>>>>

  <assign|doc-make-title|<macro|body|<surround||<vspace|2fn>|<doc-title-block|<arg|body>>>>>

  <assign|doc-make-rich-title|<\macro|notes|body>
    <\surround||<arg|notes>>
      <\doc-make-title>
        <arg|body>
      </doc-make-title>
    </surround>
  </macro>>

  <assign|doc-title|<macro|x|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.682|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|doc-subtitle|<macro|x|<\surround|<vspace*|0.25fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.297|<with|math-font-series|bold|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|doc-date|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><doc-title-block|<with|font-shape|italic|<arg|body>>><vspace|0.5fn>>>>

  <assign|doc-misc|<macro|body|<style-with|src-compact|none|<vspace*|0.5fn><doc-title-block|<arg|body>><vspace|0.5fn>>>>

  <assign|doc-note|<\macro|note>
    <arg|note>
  </macro>>

  <assign|doc-running-title|<macro|body|<header-title|<arg|body>>>>

  <assign|doc-running-author|<macro|body|<header-author|<arg|body>>>>

  <assign|doc-note-text|<macro|sym|id|body|<custom-note-text|<arg|sym>|<arg|id>|<arg|body>>>>

  <assign|doc-note-ref|<macro|sym|sep|id|body|<custom-note-ref|<arg|sym>|<arg|sep>|<arg|id>|<arg|body>>>>

  <assign|doc-footnote-text|<macro|sym|id|body|<custom-footnote-text|<arg|sym>|<arg|id>|<arg|body>>>>

  <assign|doc-data|<xmacro|args|<extern|doc-data|<quote-arg|args>|>>>

  <\active*>
    <\src-comment>
      Documents authors.
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

  <assign|author-name|<macro|author|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<name|<author-by|<arg|author>>>>>>>

  <assign|author-affiliation|<\macro|address>
    <surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<arg|address>>>
  </macro>>

  <assign|author-name-affiliation|<macro|author|address|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<name|<arg|author>>,
  <arg|address>>>>>

  <assign|author-affiliation-note|<\macro|sym|id|address>
    <author-affiliation|<\doc-note-text|<arg|sym>|<arg|id>>
      <arg|address>
    </doc-note-text>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<style-with|src-compact|none|<with|font-shape|italic|<email-text><localize|:>
  ><with|font-family|tt|language|verbatim|<arg|email>>>>>>

  <assign|author-email-note|<macro|sym|id|email|<doc-author-block|<doc-note-text|<arg|sym>|<arg|id>|<style-with|src-compact|none|<with|font-shape|italic|<email-text><localize|:>
  ><with|font-family|tt|language|verbatim|<arg|email>>>>>>>

  <assign|author-homepage|<macro|homepage|<doc-author-block|<style-with|src-compact|none|<with|font-shape|italic|<homepage-text><localize|:>
  ><with|font-family|tt|language|verbatim|<arg|homepage>>>>>>

  <assign|author-homepage-note|<macro|sym|id|homepage|<doc-author-block|<doc-note-text|<arg|sym>|<arg|id>|<style-with|src-compact|none|<with|font-shape|italic|<homepage-text><localize|:>
  ><with|font-family|tt|language|verbatim|<arg|homepage>>>>>>>

  <assign|author-misc|<\macro|body>
    <surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<arg|body>>>
  </macro>>

  <assign|author-misc-note|<macro|sym|id|misc|<doc-author-block|<doc-note-text|<arg|sym>|<arg|id>|<arg|misc>>>>>

  <assign|author-note|<\macro|note>
    <arg|note>
  </macro>>

  <assign|doc-author|<macro|body|<\surround|<vspace*|1fn>|<vspace|1fn>>
    <\with|par-par-sep|0fn>
      <doc-title-block|<arg|body>>
    </with>
  </surround>>>

  <assign|author-data|<xmacro|args|<extern|author-data|<quote-arg|args>>>>

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

  <assign|padded-author|<macro|data|<quasi|<style-with|src-compact|none|<space|1em><unquote|<quote-arg|data>><space|1em><line-break>>>>>

  <assign|doc-author*|<\macro|body>
    <\with|author-by|<macro|x|<arg|x>>>
      <doc-authors-block|<arg|body>>
    </with>
  </macro>>

  <assign|doc-authors|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <\doc-author>
          <with|doc-author|<value|doc-author*>|<\font-magnify|0.917>
            <space|0spc><unquote*|<map|padded-author|<quote-arg|data>>>
          </font-magnify>>
        </doc-author>
      </quasi>
    </style-with>
  </xmacro>>

  <\active*>
    <\src-comment>
      Abstracts.
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

  <assign|abstract-keywords|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<keywords-text><localize|:>
  ><concat-tuple|<quote-arg|args>|, >>>>

  <assign|acm-ref|<value|identity>>

  <assign|abstract-acm|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<ACM-class-text><localize|:>
  ><concat-tuple|<map|acm-ref|<quote-arg|args>>|, >>>>

  <assign|arxiv-ref|<macro|cat-id|<style-with|src-compact|none|<hlink|<arg|cat-id>|<style-with|src-compact|none|<merge|http://arxiv.org/find/all/1/cat%3A+|<arg|cat-id>|/0/1/0/all/0/1>>>>>>

  <assign|abstract-arxiv|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<arXiv-class-text><localize|:>
  ><concat-tuple|<map|arxiv-ref|<quote-arg|args>>|, >>>>

  <assign|pacs-ref|<value|identity>>

  <assign|abstract-pacs|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<PACS-class-text><localize|:>
  ><concat-tuple|<map|pacs-ref|<quote-arg|args>>|, >>>>

  <assign|msc-ref|<macro|msc-id|<style-with|src-compact|none|<hlink|<arg|msc-id>|<style-with|src-compact|none|<merge|http://www.ams.org/mathscinet/search/mscbrowse.html?sk=|<arg|msc-id>|&submit=Search>>>>>>

  <assign|abstract-msc|<xmacro|args|<style-with|src-compact|none|<no-indent><theorem-name|<AMS-class-text><localize|:>
  ><concat-tuple|<map|msc-ref|<quote-arg|args>>|, >>>>

  <assign|abstract-data|<xmacro|args|<extern|abstract-data|<quote-arg|args>>>>

  <assign|render-abstract*|<\macro|body|note>
    <\quasi>
      <\render-abstract>
        <surround||<vspace|0.5fn>|<unquote|<quote-arg|body>>>

        <\with|par-par-sep|0.25fn>
          <unquote*|<arg|note>>
        </with>
      </render-abstract>
    </quasi>
  </macro>>

  <assign|abstract|<\macro|body>
    <\render-abstract>
      <arg|body>
    </render-abstract>
  </macro>>

  <\active*>
    <\src-comment>
      Miscellaneous.
    </src-comment>
  </active*>

  <assign|doc-title-options|<macro|body|>>

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
  </collection>
</initial>