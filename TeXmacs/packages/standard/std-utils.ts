<TeXmacs|1.99.9>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-utils|1.0>

    <\src-purpose>
      This package contains useful macros for writing style files.
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
      Helper macros for wide block environments (work also for inline
      content, like section titles).
    </src-comment>
  </active*>

  <assign|hflush|<macro|<htab|0fn|0>>>

  <assign|right-flush|<macro|<specific|texmacs|<htab|0fn|first>>>>

  <assign|left-flush|<macro|<htab|0fn|last>>>

  <assign|wide-normal|<macro|body|<surround|<no-indent>|<specific|texmacs|<htab|0fn|first>>|<arg|body>>>>

  <assign|wide-centered|<macro|body|<surround|<no-indent><htab|0fn|last>|<htab|0fn|first>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Helper macros for vertically padded environments
    </src-comment>
  </active*>

  <assign|padding-above|0.5fn>

  <assign|padding-below|0.5fn>

  <assign|large-padding-above|1fn>

  <assign|large-padding-below|1fn>

  <assign|padded-normal|<macro|before|after|body|<surround|<vspace*|<arg|before>><no-indent>|<specific|texmacs|<htab|0fn|first>><vspace|<arg|after>>|<arg|body>>>>

  <assign|padded-centered|<macro|before|after|body|<surround|<vspace*|<arg|before>><no-indent><htab|0fn|last>|<htab|0fn|first><vspace|<arg|after>>|<arg|body>>>>

  <assign|padded-normal-titled|<\macro|before|after|body|title>
    <\surround|<vspace*|<arg|before>><no-indent>|>
      <arg|title>
    </surround>

    <\surround|<no-indent>|<specific|texmacs|<htab|0fn|first>><vspace|<arg|after>>>
      <arg|body>
    </surround>
  </macro>>

  <assign|padded|<\macro|body>
    <\padded-normal|<value|padding-above>|<value|padding-below>>
      <arg|body>
    </padded-normal>
  </macro>>

  <assign|padded*|<\macro|body>
    <\padded-normal|<value|large-padding-above>|<value|large-padding-below>>
      <arg|body>
    </padded-normal>
  </macro>>

  <assign|padded-titled|<\macro|body|title>
    <\padded-normal-titled|<value|padding-above>|<value|padding-below>>
      <arg|body>
    </padded-normal-titled|<arg|title>>
  </macro>>

  <assign|padded-center|<macro|body|<with|par-mode|center|<surround|<vspace*|<value|padding-above>><no-indent>|<vspace|<value|padding-below>>|<arg|body>>>>>

  <assign|padded-left-aligned|<macro|body|<with|par-mode|left|<surround|<vspace*|<value|padding-above>><no-indent>|<vspace|<value|padding-below>>|<arg|body>>>>>

  <assign|padded-right-aligned|<macro|body|<with|par-mode|right|<surround|<vspace*|<value|padding-above>><no-indent>|<vspace|<value|padding-below>>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Helper macros for underlined/overlined block environments (work also
      for inline content, like page headers).
    </src-comment>
  </active*>

  <assign|overlined-width|1ln>

  <assign|overlined-sep|1sep>

  <assign|underlined-width|1ln>

  <assign|underlined-sep|1sep>

  <assign|wide-bothlined|<macro|top-border|bot-border|top-sep|bot-sep|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-tborder|<arg|top-border>>|<cwith|1|1|1|1|cell-bborder|<arg|bot-border>>|<cwith|1|1|1|1|cell-tsep|<arg|top-sep>>|<cwith|1|1|1|1|cell-bsep|<arg|bot-sep>>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|wide-bothlined-titled|<macro|top-border|bot-border|top-sep|bot-sep|body|title|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|2|1|1|cell-width|1par>|<cwith|1|2|1|1|cell-lsep|0pt>|<cwith|1|2|1|1|cell-rsep|0pt>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|1|2|1|1|cell-tborder|<arg|top-border>>|<cwith|2|2|1|1|cell-bborder|<arg|bot-border>>|<cwith|1|2|1|1|cell-tsep|<arg|top-sep>>|<cwith|1|1|1|1|cell-bsep|<arg|top-sep>>|<cwith|2|2|1|1|cell-bsep|<arg|bot-sep>>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|title>
  </cell>>|<row|<\cell>
    <arg|body>
  </cell>>>>>>>>

  <assign|wide-bothlined-titled*|<macro|top-border|bot-border|top-sep|bot-sep|body|title|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|2|1|1|cell-width|1par>|<cwith|1|2|1|1|cell-lsep|0pt>|<cwith|1|2|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-tborder|<arg|top-border>>|<cwith|1|2|1|1|cell-bborder|<arg|bot-border>>|<cwith|1|1|1|1|cell-tsep|<arg|top-sep>>|<cwith|2|2|1|1|cell-tsep|<arg|bot-sep>>|<cwith|1|2|1|1|cell-bsep|<arg|bot-sep>>|<cwith|2|2|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>|<row|<\cell>
    <arg|title>
  </cell>>>>>>>>

  <assign|wide-std-bothlined|<macro|body|<wide-bothlined|<value|overlined-width>|<value|underlined-width>|<value|overlined-sep>|<value|underlined-sep>|<arg|body>>>>

  <assign|wide-std-bothlined-titled|<macro|body|title|<wide-bothlined-titled|<value|overlined-width>|<value|underlined-width>|<value|overlined-sep>|<value|underlined-sep>|<arg|body>|<arg|title>>>>

  <assign|padded-bothlined|<macro|before|after|top-border|bot-border|top-sep|bot-sep|body|<surround|<vspace*|<arg|before>>|<vspace|<arg|after>>|<wide-bothlined|<arg|top-border>|<arg|bot-border>|<arg|top-sep>|<arg|bot-sep>|<arg|body>>>>>

  <assign|padded-std-bothlined|<\macro|before|after|body>
    <padded-bothlined|<arg|before>|<arg|after>|<value|overlined-width>|<value|underlined-width>|<value|overlined-sep>|<value|underlined-sep>|<arg|body>>
  </macro>>

  <assign|wide-overlined|<macro|tborder|tsep|body|<wide-bothlined|<arg|tborder>|0pt|<arg|tsep>|0pt|<arg|body>>>>

  <assign|wide-std-overlined|<macro|body|<wide-overlined|<value|overlined-width>|<value|overlined-sep>|<arg|body>>>>

  <assign|wide-std-overlined-titled|<macro|body|title|<wide-bothlined-titled|<value|overlined-width>|0pt|<value|overlined-sep>|0pt|<arg|body>|<arg|title>>>>

  <assign|wide-underlined|<macro|bborder|bsep|body|<wide-bothlined|0pt|<arg|bborder>|0pt|<arg|bsep>|<arg|body>>>>

  <assign|wide-std-underlined|<macro|body|<wide-underlined|<value|underlined-width>|<value|underlined-sep>|<arg|body>>>>

  <assign|wide-std-underlined-titled|<macro|body|title|<wide-bothlined-titled*|0pt|<value|underlined-width>|0pt|<value|underlined-sep>|<arg|body>|<arg|title>>>>

  <assign|overlined|<\macro|body>
    <padded|<wide-std-overlined|<arg|body>>>
  </macro>>

  <assign|underlined|<\macro|body>
    <padded|<wide-std-underlined|<arg|body>>>
  </macro>>

  <assign|bothlined|<\macro|body>
    <padded|<wide-std-bothlined|<arg|body>>>
  </macro>>

  <assign|overlined-titled|<\macro|body|title>
    <padded|<wide-std-overlined-titled|<arg|body>|<arg|title>>>
  </macro>>

  <assign|underlined-titled|<\macro|body|title>
    <padded|<wide-std-underlined-titled|<arg|body>|<arg|title>>>
  </macro>>

  <assign|bothlined-titled|<\macro|body|title>
    <padded|<wide-std-bothlined-titled|<arg|body>|<arg|title>>>
  </macro>>

  <\active*>
    <\src-comment>
      Helper macros for framed block environments (work also for inline
      content).
    </src-comment>
  </active*>

  <assign|framed-color|>

  <assign|framed-width|1ln>

  <assign|framed-hsep|1spc>

  <assign|framed-vsep|1sep>

  <assign|wide-framed|<macro|border-width|hsep|vsep|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lborder|<arg|border-width>>|<cwith|1|1|1|1|cell-rborder|<arg|border-width>>|<cwith|1|1|1|1|cell-tborder|<arg|border-width>>|<cwith|1|1|1|1|cell-bborder|<arg|border-width>>|<cwith|1|1|1|1|cell-lsep|<arg|hsep>>|<cwith|1|1|1|1|cell-rsep|<arg|hsep>>|<cwith|1|1|1|1|cell-tsep|<arg|vsep>>|<cwith|1|1|1|1|cell-bsep|<arg|vsep>>|<cwith|1|1|1|1|cell-background|<value|framed-color>>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|wide-framed-titled|<macro|border-width|hsep|vsep|body|title|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|2|1|1|cell-width|1par>|<cwith|2|2|1|1|cell-hyphen|t>|<cwith|1|2|1|1|cell-lborder|<arg|border-width>>|<cwith|1|2|1|1|cell-rborder|<arg|border-width>>|<cwith|1|2|1|1|cell-tborder|<arg|border-width>>|<cwith|1|2|1|1|cell-bborder|<arg|border-width>>|<cwith|1|2|1|1|cell-lsep|<arg|hsep>>|<cwith|1|2|1|1|cell-rsep|<arg|hsep>>|<cwith|1|2|1|1|cell-tsep|<arg|vsep>>|<cwith|1|2|1|1|cell-bsep|<arg|vsep>>|<cwith|1|2|1|1|cell-background|<value|framed-color>>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|title>
  </cell>>|<row|<\cell>
    <arg|body>
  </cell>>>>>>>>

  <assign|wide-std-framed|<macro|body|<wide-framed|<value|framed-width>|<value|framed-hsep>|<value|framed-vsep>|<arg|body>>>>

  <assign|wide-std-framed-titled|<macro|body|title|<wide-framed-titled|<value|framed-width>|<value|framed-hsep>|<value|framed-vsep>|<arg|body>|<arg|title>>>>

  <assign|wide-framed-colored|<macro|border-color|body-color|border-width|hsep|vsep|body|<with|old-color|<value|color>|color|<arg|border-color>|framed-color|<arg|body-color>|<wide-framed|<arg|border-width>|<arg|hsep>|<arg|vsep>|<with|color|<value|old-color>|<style-with|src-compact|none|<arg|body>>>>>>>

  <assign|wide-std-framed-colored|<macro|border-color|body-color|body|<wide-framed-colored|<arg|border-color>|<arg|body-color>|<value|framed-width>|<value|framed-hsep>|<value|framed-vsep>|<style-with|src-compact|none|<arg|body>>>>>

  <assign|framed|<\macro|body>
    <padded|<wide-std-framed|<arg|body>>>
  </macro>>

  <assign|framed-titled|<\macro|body|title>
    <padded|<compound|wide-std-framed-titled|<arg|body>|<arg|title>>>
  </macro>>

  <\active*>
    <\src-comment>
      Ornamented environments.
    </src-comment>
  </active*>

  <assign|ornamented|<\macro|body>
    <padded|<\ornament>
      <\surround||<right-flush>>
        <arg|body>
      </surround>
    </ornament>>
  </macro>>

  <assign|ornamented-titled|<\macro|body|title>
    <padded|<\ornament>
      <\surround||<right-flush>>
        <arg|body>
      </surround>
    <|ornament>
      <arg|title>
    </ornament>>
  </macro>>

  <drd-props|ornamented-titled|arity|2|accessible|all>

  <assign|ornament-indent|<\macro|left|right|bottom|top|body>
    <\with|old-shape|<value|ornament-shape>|old-color|<value|ornament-color>|old-hpadding|<value|ornament-hpadding>|old-vpadding|<value|ornament-vpadding>|old-border|<value|ornament-border>|ornament-shape|classic|ornament-color|#fff0|ornament-hpadding|<tuple|<arg|left>|<arg|right>>|ornament-vpadding|<tuple|<arg|bottom>|<arg|top>>|ornament-border|0ln>
      <\ornament>
        <\surround||<right-flush>>
          <\with|ornament-shape|<value|old-shape>|ornament-color|<value|old-color>|ornament-hpadding|<value|old-hpadding>|ornament-vpadding|<value|old-vpadding>|ornament-border|<value|old-border>>
            <arg|body>
          </with>
        </surround>
      </ornament>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Decorations based on ornaments.
    </src-comment>
  </active*>

  <assign|ornament-render-title|<macro|body|<arg|body>>>

  <assign|ornament-render-body|<macro|body|<arg|body>>>

  <assign|ornament-title-color|<value|color>>

  <assign|ornament-title-math-color|<value|color>>

  <assign|ornament-title-strong-color|<value|color>>

  <assign|ornament-body-color|<value|color>>

  <assign|ornament-body-math-color|<value|color>>

  <assign|ornament-body-strong-color|<value|color>>

  \;

  <assign|decorated-hook|<macro|body|<with|bg-color|<value|ornament-extra-color>|<ornament-render-title|<with|ornament-extra-color|<value|bg-color>|<with|bg-color|<value|ornament-color>|<ornament-render-body|<with|ornament-color|<value|bg-color>|<arg|body>>>>>>>>>

  <assign|decorated-title|<macro|body|<with|color|<value|ornament-title-color>|math-color|<value|ornament-title-math-color>|strong-color|<value|ornament-title-strong-color>|<ornament-render-title|<arg|body>>>>>

  <assign|decorated-body|<macro|body|<with|color|<value|ornament-body-color>|math-color|<value|ornament-body-math-color>|strong-color|<value|ornament-body-strong-color>|<ornament-render-body|<arg|body>>>>>

  <assign|decorated|<macro|body|<decorated-hook|<ornament|<decorated-body|<arg|body>>>>>>

  <assign|decorated-block|<\macro|body>
    <decorated-hook|<\ornament>
      <\wide-normal>
        <\decorated-body>
          <arg|body>
        </decorated-body>
      </wide-normal>
    </ornament>>
  </macro>>

  <assign|decorated-titled|<macro|name|body|<decorated-hook|<ornament|<decorated-body|<arg|body>>|<decorated-title|<arg|name>>>>>>

  <assign|decorated-titled-block|<\macro|name|body>
    <decorated-hook|<\ornament>
      <\wide-normal>
        <decorated-body|<arg|body>>
      </wide-normal>
    </ornament|<decorated-title|<arg|name>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Indentation.
    </src-comment>
  </active*>

  <assign|indent-left|<\macro|indentation|body>
    <\with|par-left|<plus|<value|par-left>|<arg|indentation>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-right|<\macro|indentation|body>
    <\with|par-right|<plus|<value|par-right>|<arg|indentation>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-both|<\macro|left-indentation|right-indentation|body>
    <\with|par-left|<plus|<value|par-left>|<arg|left-indentation>>|par-right|<plus|<value|par-right>|<arg|right-indentation>>>
      <arg|body>
    </with>
  </macro>>

  <assign|margin-first-other|<macro|head-indent|tail-indent|body|<with|par-left|<plus|<arg|tail-indent>|0em>|par-first|<minus|<arg|head-indent>|<arg|tail-indent>>|<surround|<yes-indent>||<arg|body>>>>>

  <\active*>
    <\src-comment>
      Mini paragraphs.
    </src-comment>
  </active*>

  <assign|mini-paragraph|<\macro|width|body>
    <tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|<arg|width>>|<cwith|1|1|1|1|cell-hmode|exact>|<table|<row|<\cell>
      <arg|body>
    </cell>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Header information.
    </src-comment>
  </active*>

  <assign|page-number|<macro|<quote|<page-the-page>>>>

  <assign|set-this-page-header|<macro|header|<style-with|src-compact|none|<flag|<localize|this
  page header>|brown|header><assign|page-this-header|<arg|header>>>>>

  <assign|set-header|<macro|header|<style-with|src-compact|none|<flag|<localize|header>|brown|header><assign|page-odd-header|<arg|header>><assign|page-even-header|<arg|header>>>>>

  <assign|set-odd-page-header|<macro|header|<style-with|src-compact|none|<flag|<localize|odd
  page header>|brown|header><assign|page-odd-header|<arg|header>>>>>

  <assign|set-even-page-header|<macro|header|<style-with|src-compact|none|<flag|<localize|even
  page header>|brown|header><assign|page-odd-header|<arg|header>>>>>

  <assign|set-this-page-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|this
  page footer>|brown|footer><assign|page-this-footer|<arg|footer>>>>>

  <assign|set-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|footer>|brown|footer><assign|page-odd-footer|<arg|footer>><assign|page-even-footer|<arg|footer>>>>>

  <assign|set-odd-page-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|odd
  page footer>|brown|footer><assign|page-odd-footer|<arg|footer>>>>>

  <assign|set-even-page-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|even
  page footer>|brown|footer><assign|page-odd-footer|<arg|footer>>>>>

  <assign|set-page-number|<macro|nr|<style-with|src-compact|none|<flag|<localize|renumber
  this page>|brown|nr><assign|page-nr|<arg|nr>>>>>

  <assign|set-page-number-macro|<macro|rendering-macro|<style-with|src-compact|none|<flag|<localize|page
  number text>|brown|rendering-macro><assign|page-the-page|<arg|rendering-macro>>>>>

  <assign|blanc-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|>>>>

  <assign|simple-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|<no-indent><htab|5mm><page-number><htab|5mm>>>>>

  <\active*>
    <\src-comment>
      Further table macros.
    </src-comment>
  </active*>

  <assign|description-table|<macro|body|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<twith|table-block|yes>|<twith|table-min-cols|2>|<cwith|1|-1|2|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-hpart|0.001>|<cwith|1|-1|1|1|cell-lsep|0fn>|<cwith|1|-1|-1|-1|cell-rsep|0fn>|<cwith|1|-2|1|-1|cell-bsep|<value|par-par-sep>>|<cwith|2|-1|1|-1|cell-tsep|<value|par-sep>>|<arg|body>>>>

  <assign|prefixed-line|<\macro|prefix|body>
    <\with|par-first|0fn>
      <\description-table>
        <tformat|<twith|table-valign|T>|<twith|table-hmode|min>|<cwith|1|-1|1|-1|cell-lsep|0em>|<cwith|1|-1|1|-1|cell-rsep|0em>|<cwith|1|1|1|-1|cell-tsep|0em>|<cwith|-1|-1|1|-1|cell-bsep|0em>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|<arg|prefix>>|<\cell>
          <arg|body>
        </cell>>>>
      </description-table>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Other macros.
    </src-comment>
  </active*>

  <assign|localize|<macro|text|<translate|<arg|text>|english|<value|language>>>>

  <assign|language-suffix|<macro|<extern|ext-language-suffix>>>

  <assign|map|<macro|fun|args|<extern|ext-map|<arg|fun>|<arg|args>>>>

  <assign|concat-tuple|<macro|args|sep|fin|<extern|ext-concat-tuple|<arg|args>|<arg|sep>|<arg|fin>>>>

  <assign|comma-separated|<xmacro|x|<concat-tuple|<quote-arg|x>|, >>>

  <assign|semicolon-separated|<xmacro|x|<concat-tuple|<quote-arg|x>|; >>>

  <drd-props|comma-separated|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|semicolon-separated|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|font-magnify|<macro|factor|body|<with|font-size|<times|<value|font-size>|<arg|factor>>|<arg|body>>>>

  <assign|hidden-title|<macro|title|>>

  <assign|inline-block|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0fn>|<twith|table-width|1par>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>