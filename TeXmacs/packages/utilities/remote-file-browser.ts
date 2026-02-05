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

  <assign|dir-entry-icon|<macro|name|tip|<with|icon|<image|<find-file|$TEXMACS_PATH/misc/pixmaps/light|$TEXMACS_PATH/misc/pixmaps/modern/24x24/main|$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus|<arg|name>>||12pt||-0.666ex>|<if|<equal|<arg|tip>|>|<value|icon>|<help-balloon|<value|icon>|<arg|tip>|left|Bottom>>>>>

  <assign|phantom-icon|<macro|<phantom|<dir-entry-icon|tm_cloud_share.svg|>>>>

  <\active*>
    <\src-comment>
      Column headers
    </src-comment>
  </active*>

  <assign|dir-header|<\macro|type-label|type-action|name-label|name-action|date-label|date-action|action-label>
    <wide-underlined|0.5ln|0.2em|<concat|<action|<arg|type-label>|<arg|type-action>>|<hspace|12pt><action|<arg|name-label>|<arg|name-action>>|<htab|5mm>|<action|<arg|date-label>|<arg|date-action>><hspace|1em><phantom-icon>>>
  </macro>>

  <\active*>
    <\src-comment>
      Entry macros
    </src-comment>
  </active*>

  <assign|dir-entry|<\macro|icon-name|name|link|date|share-action>
    <concat|<dir-entry-icon|<arg|icon-name>|>|<hspace|12pt><hlink|<arg|name>|<arg|link>>|<htab|5mm>|<arg|date><hspace|1em><arg|share-action>>
  </macro>>

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
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>
