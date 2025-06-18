<TeXmacs|2.1.2>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|steelblue|1.0|steelblue|1.0>

    <\src-purpose>
      Steel blue theme.
    </src-purpose>

    <src-copyright|2013--2023|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|steelblue-combo>

  <apply-theme|steel-blue>

  \;

  <assign|title-blurred-radius|0.3pt>

  <assign|title-shadow-font-color|#604040>

  <assign|title-sunny-font-color|#604040>

  <assign|title-enhance|<macro|body|<with|color|#d8c0c0|strong-color|#d8c0c0|<add-font-effect|enhanced|<merge|<value|title-blurred-radius>|;|<value|title-shadow-font-color>|;|<value|title-sunny-font-color>>|<arg|body>>>>>

  \;

  <assign|strong-blurred-radius|0.3pt>

  <assign|strong-shadow-font-color|#000>

  <assign|strong-sunny-font-color|#fff>

  <assign|nosteel-strong|<value|strong>>

  <assign|nosteel-item-strong|<value|item-strong>>

  <assign|strong-enhance|<macro|body|<add-font-effect|enhanced|<merge|<value|strong-blurred-radius>|;|<value|strong-shadow-font-color>|;|<value|strong-sunny-font-color>>|<arg|body>>>>

  <assign|strong|<macro|body|<nosteel-strong|<strong-enhance|<arg|body>>>>>

  <assign|item-strong|<macro|body|<nosteel-item-strong|<strong-enhance|<arg|body>>>>>

  \;

  <assign|nosteel-tit|<value|tit>>

  <assign|nosteel-title-right|<value|title-right>>

  <assign|tit|<macro|body|<nosteel-tit|<nosteel-strong|<title-enhance|<arg|body>>>>>>

  <assign|title-sub-bar-contents|<macro|body|>>

  <assign|title-right|<macro|body|<nosteel-strong|<title-enhance|<nosteel-title-right|<arg|body>>>>>>

  \;

  <assign|nosteel-render-theorem|<value|render-theorem>>

  <assign|render-theorem|<\macro|name|body>
    <\nosteel-render-theorem|<title-enhance|<arg|name>>>
      <arg|body>
    </nosteel-render-theorem>
  </macro>>

  \;

  <assign|heading-color|#844>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>