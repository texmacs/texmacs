<TeXmacs|1.0.7.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|presentation|1.0|presentation|1.0>

    <\src-purpose>
      Presentation style.
    </src-purpose>

    <src-copyright|2007--2008|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|varsession|alt-colors|ornaments|ridged-paper>

  <\active*>
    <\src-comment>
      Global document layout
    </src-comment>
  </active*>

  <assign|font-family|ss>

  <assign|name|<macro|x|<with|font-family|rm|font-shape|small-caps|<arg|x>>>>

  <assign|magnification|1.5>

  <assign|page-screen-left|5mm>

  <assign|page-screen-right|5mm>

  <assign|page-screen-top|5mm>

  <assign|page-screen-bottom|5mm>

  <\active*>
    <\src-comment>
      Customized list environments
    </src-comment>
  </active*>

  <assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-2.5fn|<yes-indent>><resize|<arg|x>|r-2.5fn||r+0.0fn|>>>>

  <assign|render-bibitem|<macro|text|<aligned-item|<transform-bibitem|<arg|text>>>>>

  <\active*>
    <\src-comment>
      Customized session elements
    </src-comment>
  </active*>

  <assign|session|<\macro|lan|ses|body>
    <\with|prog-language|<arg|lan>|prog-session|<arg|ses>>
      <\small>
        <render-session|<arg|body>>
      </small>
    </with>
  </macro>>

  <assign|folded-body|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <\active*>
    <\src-comment>
      Miscellaneous
    </src-comment>
  </active*>

  <assign|img|<macro|body|<with|ornament-color|white|<ornament|<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>