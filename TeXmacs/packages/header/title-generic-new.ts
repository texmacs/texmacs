<TeXmacs|1.0.7.17>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|title-generic|new-1.0|header-title|1.0>

    <\src-purpose>
      Titles for the generic style based on Scheme content managment.
    </src-purpose>

    <src-copyright|2012|François Poulain, Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(database title-base)>

  <active*|<\src-comment>
    **DEBUG** Print in console a typesetted texmacs tree
  </src-comment>>

  <assign|print-tree|<macro|args|<extern|print-tree|<arg|args>>>>

  <drd-props|print-tree|arity|1|accessible|all>

  <active*|<\src-comment>
    Doc-data Scheme wrapping
  </src-comment>>

  <assign|doc-data|<xmacro|args|<extern|doc-data|<quote-arg|args>>>>

  <drd-props|doc-data|arity|<tuple|repeat|1|1>|accessible|all>

  <active*|<\src-comment>
    Doc-author Scheme wrapping
  </src-comment>>

  <assign|doc-author|<macro|arg|<extern|doc-author|<quote-arg|arg>>>>

  <active*|<\src-comment>
    Generic bloc rendering
  </src-comment>>

  <assign|render-bloc-data|<macro|body|<tabular*|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<twith|table-valign|t>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
    <\center>
      <arg|body>
    </center>
  </cell>>>>>>>

  <active*|<\src-comment>
    Title rendering
  </src-comment>>

  <assign|render-title-bloc|<\macro|body>
    <\surround|<reset-count-title-note>|>
      <\render-bloc-data>
        <arg|body>
      </render-bloc-data>
    </surround>
  </macro>>

  <assign|render-title-inline|<macro|body|<with|font-base-size|17|font-series|bold|<arg|body>>>>

  <active*|<\src-comment>
    Subtitle rendering
  </src-comment>>

  <assign|render-subtitle-bloc|<value|render-bloc-data>>

  <assign|render-subtitle-inline|<macro|body|<with|font-base-size|12|font-series|bold|<arg|body>>>>

  <active*|<\src-comment>
    Date rendering
  </src-comment>>

  <assign|render-date-bloc|<value|render-bloc-data>>

  <assign|render-date-inline|<macro|body|<with|font-shape|italic|<arg|body>>>>

  <active*|<\src-comment>
    Authors inline rendering
  </src-comment>>

  <assign|render-author-inline|<\macro|body>
    <with|par-sep|0.1fn|<arg|body>>
  </macro>>

  <assign|render-author-name-inline*|<macro|body|<with|font-shape|small-caps|<by-text>
  <arg|body>>>>

  <assign|render-author-name-inline|<macro|body|<with|font-shape|small-caps|<arg|body>>>>

  <assign|render-author-affiliation-inline|<value|identity>>

  <assign|render-author-email-inline|<macro|body|<with|par-par-sep|0pt|<email-text><field-sep-text><with|font-family|tt|<arg|body>>>>>

  <assign|render-author-homepage-inline|<macro|body|<with|par-par-sep|0pt|<homepage-text><field-sep-text><hlink|<with|font-family|tt|http://<arg|body>>|http://<arg|body>>>>>

  <active*|<\src-comment>
    Authors bloc rendering
  </src-comment>>

  <assign|render-authors-sep|<space|1em><line-break>>

  <assign|render-authors-bloc|<value|render-bloc-data>>

  <assign|render-author-bloc|<\macro|body>
    <tabular*|<tformat|<twith|table-valign|t>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lsep|1em>|<cwith|1|1|1|1|cell-rsep|1em>|<cwith|1|1|1|1|cell-bsep|1em>|<cwith|1|1|1|1|cell-tsep|1em>|<table|<row|<\cell>
      <\center>
        <arg|body>
      </center>
    </cell>>>>>
  </macro>>

  <assign|render-author-bloc*|<\macro|body>
    <\with|render-author-name-inline|<value|render-author-name-inline*>>
      <render-author-bloc|<arg|body>>
    </with>
  </macro>>

  <assign|render-author-name-bloc|<value|identity>>

  <assign|render-author-affiliation-bloc|<value|identity>>

  <assign|render-author-email-bloc|<value|identity>>

  <assign|render-author-homepage-bloc|<value|identity>>

  <active*|<\src-comment>
    Notes management
  </src-comment>>

  <new-counter|count-title-note>

  <assign|count-title-note-nr|0>

  <assign|the-title-note|<macro|<number|<value|count-title-note-nr>|fnsymbol>>>

  <assign|insert-title-note-label|<macro|<next-count-title-note><the-title-note>>>

  <assign|title-note-label-name|<macro|<merge|title-|<eval|<the-count-title-note>>>>>

  <assign|title-note-ref-name|<macro|<merge|#footnote-title-|<plus|<eval|<the-count-title-note>>|1>>>>

  \;

  <assign|render-title-note-label|<macro|<space|0spc><math|<rsup|<hlink|<label|<merge|footnr-|<title-note-label-name>>><insert-title-note-label>|<title-note-ref-name>>>>>>

  <assign|render-author-misc-label|<value|render-title-note-label>>

  <assign|render-title-note-bloc|<macro|body|<render-footnote*|<insert-title-note-label>|<title-note-label-name>|<arg|body>>>>

  <assign|render-author-misc-bloc|<value|render-title-note-bloc>>

  <assign|render-title-note-inline|<value|identity>>

  <assign|render-author-misc-inline|<value|identity>>

  <active*|<\src-comment>
    drd-props
  </src-comment>>

  <drd-props|doc-date|arity|1|accessible|all>

  <drd-props|doc-note|arity|1|accessible|all>

  <drd-props|doc-title|arity|1|accessible|all>

  <drd-props|doc-subtitle|arity|1|accessible|all>

  <drd-props|doc-author|arity|1|accessible|all>

  <drd-props|author-data|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|author-name|arity|1|accessible|all>

  <drd-props|author-affiliation|arity|1|accessible|all>

  <drd-props|author-email|arity|1|accessible|all>

  <drd-props|author-homepage|arity|1|accessible|all>

  <drd-props|author-misc|arity|1|accessible|all>

  <\active*>
    <\src-comment>
      Special texts.
    </src-comment>
  </active*>

  <assign|sep-text|<macro|<localize|; >>>

  <assign|field-sep-text|<macro|<localize|: >>>

  <assign|by-text|<macro|<localize|by>>>

  <assign|email-text|<macro|<localize|Email>>>

  <assign|homepage-text|<macro|<localize|Web>>>

  <assign|abstract-text|<macro|<localize|Abstract>>>

  <assign|keywords-text|<macro|<localize|Keywords>>>

  <assign|AMS-class-text|<macro|<localize|A.M.S. subject classification>>>

  <assign|with-TeXmacs-text|<macro|This document has been written using the
  GNU <TeXmacs> text editor (see <hlink|<with|font-family|tt|www.texmacs.org>|http://www.texmacs.org>).>>
</body>

<\initial>
  <\collection>
    <associate|language|british>
    <associate|preamble|true>
  </collection>
</initial>