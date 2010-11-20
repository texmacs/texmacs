<TeXmacs|1.0.7.7>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|presentation|1.0|presentation|1.0>

    <\src-purpose>
      Presentation style.
    </src-purpose>

    <src-copyright|2007--2010|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|alt-colors|ornaments|varsession>

  <\active*>
    <\src-comment>
      Global document layout
    </src-comment>
  </active*>

  <assign|page-medium|automatic>

  <assign|page-screen-left|5mm>

  <assign|page-screen-right|5mm>

  <assign|page-screen-top|5mm>

  <assign|page-screen-bottom|5mm>

  <assign|magnification|1.5>

  <assign|font-family|ss>

  <assign|name|<macro|body|<with|font-family|rm|font-shape|small-caps|<arg|body>>>>

  <\active*>
    <\src-comment>
      Customized list environments
    </src-comment>
  </active*>

  <assign|aligned-item|<macro|name|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-2.5fn|<yes-indent>><resize|<arg|name>|<minus|1r|2.5fn>||<plus|1r|0.0fn>|>>>>

  <assign|render-bibitem|<macro|text|<aligned-item|<transform-bibitem|<arg|text>>>>>

  <\active*>
    <\src-comment>
      Customized session elements
    </src-comment>
  </active*>

  <assign|session|<\macro|language|session|body>
    <\with|prog-language|<arg|language>|prog-session|<arg|session>>
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