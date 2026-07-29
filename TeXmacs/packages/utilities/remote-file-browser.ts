<TeXmacs|2.1.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|remote-file-browser|1.0>

    <\src-purpose>
      Remote File Browser
    </src-purpose>

    <src-copyright|2026|Robin WILS>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|std-shadow>

  <assign|locus-color|dark blue>

  <assign|visited-color|dark blue>

  <\active*>
    <\src-comment>
      Icons
    </src-comment>
  </active*>

  <assign|dir-icon-width|12pt>

  <assign|dir-entry-icon|<macro|name|<image|<find-file|$TEXMACS_PATH/misc/pixmaps/light|$TEXMACS_PATH/misc/pixmaps/modern/24x24/main|$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus|<arg|name>>||12pt||-0.666ex>>>

  <assign|phantom-icon|<macro|<phantom|<dir-entry-icon|tm_cloud_share.svg>>>>

  <\active*>
    <\src-comment>
      Generic row. <src-arg|dir-row-line> lays out the four columns and is
      shared by every line of the browser \V header, entry, empty placeholder
      \V which is what keeps them aligned; they differ only by the cells they
      are given. <src-arg|dir-row> adds the single-row background for entries;
      <src-arg|dir-header> uses its own two-row table so that the caption and
      the column names touch.
    </src-comment>
  </active*>

  <assign|dir-row-bg|<macro|bg|tsep|bsep|body|\
    <tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|\
      <cwith|1|1|1|1|cell-background|<arg|bg>>|\
      <cwith|1|1|1|1|cell-hyphen|t>|\
      <cwith|1|1|1|1|cell-lsep|2spc>|\
      <cwith|1|1|1|1|cell-rsep|2spc>|\
      <cwith|1|1|1|1|cell-tsep|<arg|tsep>>|\
      <cwith|1|1|1|1|cell-bsep|<arg|bsep>>|\
        <table|<row|<cell|<arg|body>>>>>>>

  <assign|dir-row-line|<macro|icon|name|date|actions|<resize|<arg|icon>|||<value|dir-icon-width>|><hspace|12pt><arg|name><htab|5mm><arg|date><hspace|1em><arg|actions>>>

  <assign|dir-row|<macro|bg|tsep|bsep|icon|name|date|actions|<dir-row-bg|<arg|bg>|<arg|tsep>|<arg|bsep>|<dir-row-line|<arg|icon>|<arg|name>|<arg|date>|<arg|actions>>>>>

  <\active*>
    <\src-comment>
      Column headers
    </src-comment>
  </active*>

  <assign|dir-header-caption|<macro|name|<htab|0fn><samp|<with|color|white|locus-color|grey|visited-color|grey|<arg|name>>><htab|0fn>>>

  <assign|dir-header-cell|<macro|label|action|<samp|<small|<with|locus-color|white|<action|<arg|label>|<arg|action>>>>>>>

  <assign|dir-header|<macro|caption|type|name|date|actions|\
    <with|color|white|\
      <tformat|\
        <twith|table-width|1par>|\
        <twith|table-hmode|exact>|\
        <cwith|1|-1|1|1|cell-hyphen|t>|\
        <cwith|1|-1|1|1|cell-lsep|2spc>|\
        <cwith|1|-1|1|1|cell-rsep|2spc>|\
        <cwith|1|1|1|1|cell-background|darker grey>|\
        <cwith|1|1|1|1|cell-tsep|2spc>|\
        <cwith|1|1|1|1|cell-bsep|2spc>|\
        <cwith|2|2|1|1|cell-background|dark grey>|\
        <cwith|2|2|1|1|cell-tsep|1spc>|\
        <cwith|2|2|1|1|cell-bsep|1spc>|\
          <table|\
            <row|<cell|<dir-header-caption|<arg|caption>>>>|\
            <row|<cell|<dir-row-line|<arg|type>|<arg|name>|<arg|date>|<phantom|<arg|actions>>>>>>>>>>

  <\active*>
    <\src-comment>
      Entry macros
    </src-comment>
  </active*>

  <assign|dir-entry-name-max-len|60>

  <assign|dir-entry-name|<macro|name|link|<if|<greater|<length|<arg|name>>|<value|dir-entry-name-max-len>>|<hlink|<range|<arg|name>|0|<minus|<value|dir-entry-name-max-len>|3>>\<ldots\>|<arg|link>>|<hlink|<arg|name>|<arg|link>>>>>

  <assign|dir-entry-row|<macro|bg|icon-name|name|link|date|actions|<dir-row|<arg|bg>|0spc|0spc|<dir-entry-icon|<arg|icon-name>>|<dir-entry-name|<arg|name>|<arg|link>>|<arg|date>|<arg|actions>>>>

  <assign|dir-entry|<\macro|icon-name|name|link|date|actions>
    <with|clickable-color|#00000000|<dynamic-case|mouse-over|<dir-entry-row|#e0e0e0|<arg|icon-name>|<arg|name>|<arg|link>|<arg|date>|<arg|actions>>|any|<dir-entry-row||<arg|icon-name>|<arg|name>|<arg|link>|<arg|date>|<arg|actions>>>>
  </macro>>

  <assign|dir-entry-empty|<macro|<dir-row||0spc|0spc|<phantom-icon>|<with|color|dark grey|font-shape|italic|Nothing to show>|||>>>

  <\active*>
    <\src-comment>
      Container macros
    </src-comment>
  </active*>

  <assign|dir-list|<\macro|body>
    <with|shadow-elevation|0.75|<\drop-shadow>
      <arg|body>
    </drop-shadow>>
  </macro>>

  <assign|dir-content|<\macro|entries>
    <with|ornament-border|0ln|ornament-hpadding|0spc|ornament-vpadding|0.5fn|padding-above|0fn|<\ornamented>
      <arg|entries>
    </ornamented>>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
