<TeXmacs|2.1.4>

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

  <\active*>
    <\src-comment>
      Icons
    </src-comment>
  </active*>

  <assign|dir-entry-icon|<macro|name|<image|<find-file|$TEXMACS_PATH/misc/pixmaps/light|$TEXMACS_PATH/misc/pixmaps/modern/24x24/main|$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus|<arg|name>>||12pt||-0.666ex>>>

  <assign|phantom-icon|<macro|<phantom|<dir-entry-icon|tm_cloud_share.svg>>>>

  <\active*>
    <\src-comment>
      Column headers
    </src-comment>
  </active*>

  <assign|dir-title|<\macro|name>
    <tformat|<cwith|1|1|1|1|cell-background|dark grey>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-lsep|1spc>|<cwith|1|1|1|1|cell-rsep|1spc>|<cwith|1|1|1|1|cell-bsep|0.5spc>|<cwith|1|1|1|1|cell-tsep|0.5spc>|<table|<row|<cell|<samp|<with|color|white|locus-color|preserve|<arg|name>>>>>>>
  </macro>>

  <assign|dir-header|<\macro|name|type-label|type-action|name-label|name-action|date-label|date-action|action-label>
    <tformat|\
      <cwith|1|1|1|1|cell-background|dark grey>|<cwith|2|2|1|1|cell-background|dark grey>|\
      <twith|table-width|1par>|<twith|table-hmode|exact>|\
      <cwith|1|1|1|1|cell-tsep|2.5spc>|<cwith|2|2|1|1|cell-bsep|0.5spc>|\
      <cwith|1|1|1|1|cell-valign|c>|<cwith|1|1|1|1|cell-halign|c>|<cwith|2|2|1|1|cell-valign|c>|<cwith|2|2|1|1|cell-halign|c>|\
      <table|\
        <row|<cell|<samp|<with|color|white|locus-color|preserve|<compact|<arg|name>|<vspace|0.6fn>>>>>>|\
        <row|<cell|<samp|<small|<with|color|white|locus-color|preserve|<wide-underlined|||<concat|<action|<arg|type-label>|<arg|type-action>>|<hspace|12pt><action|<arg|name-label>|<arg|name-action>>|<htab|5mm>|<action|<arg|date-label>|<arg|date-action>><hspace|2em><arg|action-label>>>>>>>>>>
  </macro>>

  <assign|dir-header-old|<\macro|breadcrumbs>
    <with|color|white|locus-color|preserve|<compact|<concat|<phantom-icon><dir-entry-icon|tm_cloud_dir.svg><hspace|12pt><arg|breadcrumbs>|<vspace|0.6fn>>>>
  </macro>>

  <assign|dir-labels|<\macro|type-label|type-action|name-label|name-action|date-label|date-action|action-label>
    <wide-underlined|0.5ln|0.2em|<concat|<action|<arg|type-label>|<arg|type-action>>|<hspace|12pt><action|<arg|name-label>|<arg|name-action>>|<htab|5mm>|<action|<arg|date-label>|<arg|date-action>><hspace|2em><arg|action-label>>>
  </macro>>

  <\active*>
    <\src-comment>
      Entry macros
    </src-comment>
  </active*>

  <assign|dir-entry-name-max-len|60>

  <assign|dir-entry-name|<macro|name|link|<if|<greater|<length|<arg|name>>|<value|dir-entry-name-max-len>>|<hlink|<concat|<range|<arg|name>|0|<minus|<value|dir-entry-name-max-len>|3>>|\<ldots\>>|<arg|link>>|<hlink|<arg|name>|<arg|link>>>>>

  <assign|dir-entry|<\macro|icon-name|name|link|date|actions>
    <concat|<dir-entry-icon|<arg|icon-name>>|<hspace|12pt><dir-entry-name|<arg|name>|<arg|link>>|<htab|5mm>|<arg|date><hspace|1em><arg|actions>>
  </macro>>

  <assign|dir-entry-empty|<macro|<with|color|dark
  grey|font-shape|italic|<concat|<phantom-icon><hspace|12pt>Nothing to
  show>>>>

  <\active*>
    <\src-comment>
      Container macros
    </src-comment>
  </active*>

  <assign|dir-spacer|<macro|<tformat|<cwith|1|1|1|1|cell-height|0.1fn>|<cwith|1|1|1|1|cell-vmode|exact>|<table|<row|<cell|>>>>>>

  <assign|dir-list|<\macro|body>
    <with|shadow-elevation|0.75|<\drop-shadow>
      <arg|body>
    </drop-shadow>>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
