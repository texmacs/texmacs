<TeXmacs|1.0.6.3>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-utils|1.0>

    <\src-purpose>
      This package contains useful macros for writing style files.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Helper macros for wide block environments (work also for inline
      content, like section titles).
    </src-comment>
  </active*>

  <assign|hflush|<macro|<htab|0fn|0>>>

  <assign|right-flush|<macro|<htab|0fn|first>>>

  <assign|left-flush|<macro|<htab|0fn|last>>>

  <assign|wide-normal|<macro|body|<surround|<no-indent>|<htab|0fn|first>|<arg|body>>>>

  <assign|wide-centered|<macro|body|<surround|<no-indent><htab|0fn|last>|<htab|0fn|first>|<arg|body>>>>

  <assign|padded-normal|<macro|a|b|body|<surround|<vspace*|<arg|a>><no-indent>|<htab|0fn|first><vspace|<arg|b>>|<arg|body>>>>

  <assign|padded-centered|<macro|a|b|body|<surround|<vspace*|<arg|a>><no-indent><htab|0fn|last>|<htab|0fn|first><vspace|<arg|b>>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Helper macros for underlined/overlined block environments (work also
      for inline content, like page headers).
    </src-comment>
  </active*>

  <assign|wide-bothlined|<macro|top-border|bot-border|top-sep|bot-sep|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-tborder|<arg|top-border>>|<cwith|1|1|1|1|cell-bborder|<arg|bot-border>>|<cwith|1|1|1|1|cell-tsep|<arg|top-sep>>|<cwith|1|1|1|1|cell-bsep|<arg|bot-sep>>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|wide-std-bothlined|<macro|body|<wide-bothlined|1ln|1ln|1sep|1sep|<arg|body>>>>

  <assign|padded-bothlined|<macro|a|b|top-border|bot-border|top-sep|bot-sep|body|<surround|<vspace*|<arg|a>>|<vspace|<arg|b>>|<wide-bothlined|<arg|top-border>|<arg|bot-border>|<arg|top-sep>|<arg|bot-sep>|<arg|body>>>>>

  <assign|padded-std-bothlined|<macro|a|b|body|<padded-bothlined|<arg|a>|<arg|b>|1ln|1ln|1sep|1sep|<arg|body>>>>

  <assign|wide-underlined|<macro|bborder|bsep|body|<wide-bothlined|0pt|<arg|bborder>|0pt|<arg|bsep>|<arg|body>>>>

  <assign|wide-std-underlined|<macro|body|<wide-underlined|1ln|1sep|<arg|body>>>>

  <\active*>
    <\src-comment>
      Helper macros for framed block environments (work also for inline
      content).
    </src-comment>
  </active*>

  <assign|wide-framed-color|>

  <assign|wide-framed|<macro|border-width|hsep|vsep|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lborder|<arg|border-width>>|<cwith|1|1|1|1|cell-rborder|<arg|border-width>>|<cwith|1|1|1|1|cell-tborder|<arg|border-width>>|<cwith|1|1|1|1|cell-bborder|<arg|border-width>>|<cwith|1|1|1|1|cell-lsep|<arg|hsep>>|<cwith|1|1|1|1|cell-rsep|<arg|hsep>>|<cwith|1|1|1|1|cell-tsep|<arg|vsep>>|<cwith|1|1|1|1|cell-bsep|<arg|vsep>>|<cwith|1|1|1|1|cell-background|<value|wide-framed-color>>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|wide-std-framed|<macro|body|<wide-framed|1ln|1spc|1sep|<arg|body>>>>

  <assign|wide-framed-colored|<macro|border-color|body-color|border-width|hsep|vsep|body|<with|old-color|<value|color>|color|<arg|border-color>|wide-framed-color|<arg|body-color>|<wide-framed|<arg|border-width>|<arg|hsep>|<arg|vsep>|<with|color|<value|old-color>|<style-with|src-compact|none|<arg|body>>>>>>>

  <assign|wide-std-framed-colored|<macro|border-color|body-color|body|<wide-framed-colored|<arg|border-color>|<arg|body-color>|1ln|1spc|1sep|<style-with|src-compact|none|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Indentation.
    </src-comment>
  </active*>

  <assign|indent-left|<\macro|l|body>
    <\with|par-left|<plus|<value|par-left>|<arg|l>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-right|<\macro|r|body>
    <\with|par-right|<plus|<value|par-right>|<arg|r>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-both|<\macro|l|r|body>
    <\with|par-left|<plus|<value|par-left>|<arg|l>>|par-right|<plus|<value|par-right>|<arg|r>>>
      <arg|body>
    </with>
  </macro>>

  <assign|margin-first-other|<macro|head-indent|tail-indent|body|<with|par-left|<plus|<arg|tail-indent>|0em>|par-first|<minus|<arg|head-indent>|<arg|tail-indent>>|<surround|<yes-indent>||<arg|body>>>>>

  <\active*>
    <\src-comment>
      Header information.
    </src-comment>
  </active*>

  <assign|set-this-page-header|<macro|s|<style-with|src-compact|none|<flag|<localize|this
  page header>|brown|s><assign|page-this-header|<arg|s>>>>>

  <assign|set-header|<macro|s|<style-with|src-compact|none|<flag|<localize|header>|brown|s><assign|page-odd-header|<arg|s>><assign|page-even-header|<arg|s>>>>>

  <assign|set-odd-page-header|<macro|s|<style-with|src-compact|none|<flag|<localize|odd
  page header>|brown|s><assign|page-odd-header|<arg|s>>>>>

  <assign|set-even-page-header|<macro|s|<style-with|src-compact|none|<flag|<localize|even
  page header>|brown|s><assign|page-odd-header|<arg|s>>>>>

  <assign|set-this-page-footer|<macro|s|<style-with|src-compact|none|<flag|<localize|this
  page footer>|brown|s><assign|page-this-footer|<arg|s>>>>>

  <assign|set-footer|<macro|s|<style-with|src-compact|none|<flag|<localize|footer>|brown|s><assign|page-odd-footer|<arg|s>><assign|page-even-footer|<arg|s>>>>>

  <assign|set-odd-page-footer|<macro|s|<style-with|src-compact|none|<flag|<localize|odd
  page footer>|brown|s><assign|page-odd-footer|<arg|s>>>>>

  <assign|set-even-page-footer|<macro|s|<style-with|src-compact|none|<flag|<localize|even
  page footer>|brown|s><assign|page-odd-footer|<arg|s>>>>>

  <assign|set-page-number|<macro|nr|<style-with|src-compact|none|<flag|<localize|renumber
  this page>|brown|nr><assign|page-nr|<arg|nr>>>>>

  <assign|set-page-number-macro|<macro|rendering-macro|<style-with|src-compact|none|<flag|<localize|page
  number text>|brown|rendering-macro><assign|page-the-page|<arg|rendering-macro>>>>>

  <assign|blanc-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|>>>>

  <assign|simple-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>>>>

  <\active*>
    <\src-comment>
      Other macros.
    </src-comment>
  </active*>

  <assign|localize|<macro|x|<translate|<arg|x>|english|<value|language>>>>

  <assign|map|<macro|f|args|<extern|ext-map|<arg|f>|<arg|args>>>>

  <assign|concat-tuple|<macro|args|sep|fin|<extern|ext-concat-tuple|<arg|args>|<arg|sep>|<arg|fin>>>>

  <assign|font-magnify|<macro|factor|body|<with|font-size|<times|<value|font-size>|<arg|factor>>|<arg|body>>>>

  <assign|wiki-link|<macro|body|name|<style-with|src-compact|none|<action|<arg|body>|<merge|(remote-project-load-by-name
  "|<arg|name>|")>>>>>

  <assign|inline-block|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0fn>|<twith|table-width|1par>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>