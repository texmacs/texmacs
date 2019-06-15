<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|base-deco|1.0|base-deco|1.0>

    <\src-purpose>
      Common base for decorations.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Main colors
    </src-comment>
  </active*>

  <new-theme|colors|bg-color|color|math-color|strong-color>

  <\active*>
    <\src-comment>
      Ornaments
    </src-comment>
  </active*>

  <new-theme|ornaments|ornament-shape|ornament-border|ornament-hpadding|ornament-vpadding|ornament-color|ornament-sunny-color|ornament-shadow-color|ornament-title-style|ornament-extra-color|ornament-title-color>

  <assign|ornaments-ornament-border|2ln>

  <assign|ornaments-ornament-hpadding|1spc>

  <assign|ornaments-ornament-vpadding|1spc>

  <assign|ornaments-title-color|black>

  <assign|copy-ornament|<macro|t|from|<with|f|<merge|<arg|from>|-ornament>|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|t>|-shape>>|<value|<unquote|<merge|<value|f>|-shape>>>><assign|<unquote|<merge|<arg|t>|-border>>|<value|<unquote|<merge|<value|f>|-border>>>><assign|<unquote|<merge|<arg|t>|-title-style>>|<value|<unquote|<merge|<value|f>|-title-style>>>><assign|<unquote|<merge|<arg|t>|-title-bg-color>>|<value|<unquote|<merge|<value|f>|-extra-color>>>><assign|<unquote|<merge|<arg|t>|-title-color>>|<value|<unquote|<merge|<value|f>|-title-color>>>><assign|<unquote|<merge|<arg|t>|-body-bg-color>>|<value|<unquote|<merge|<value|f>|-color>>>><assign|<unquote|<merge|<arg|t>|-body-color>>|<value|<unquote|<merge|<arg|from>|-color>>>><assign|<unquote|<merge|<arg|t>|-body-math-color>>|<value|<unquote|<merge|<arg|from>|-math-color>>>><assign|<unquote|<merge|<arg|t>|-body-strong-color>>|<value|<unquote|<merge|<arg|from>|-strong-color>>>><assign|<unquote|<merge|<arg|t>|-sunny-color>>|<value|<unquote|<merge|<value|f>|-sunny-color>>>><assign|<unquote|<merge|<arg|t>|-shadow-color>>|<value|<unquote|<merge|<value|f>|-shadow-color>>>>>>>>

  <\active*>
    <\src-comment>
      Decorations
    </src-comment>
  </active*>

  <copy-theme|rich-ornament|colors|ornaments>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>