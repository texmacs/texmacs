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

  <new-theme|ornaments|ornament-shape|ornament-border|ornament-hpadding|ornament-vpadding|ornament-color|ornament-body-color|ornament-body-math-color|ornament-body-strong-color|ornament-sunny-color|ornament-shadow-color|ornament-title-style|ornament-extra-color|ornament-title-color|ornament-title-math-color|ornament-title-strong-color>

  <assign|ornaments-ornament-border|2ln>

  <assign|ornaments-ornament-hpadding|1spc>

  <assign|ornaments-ornament-vpadding|1spc>

  <assign|ornaments-title-color|<value|color>>

  <assign|ornaments-title-math-color|<value|math-color>>

  <assign|ornaments-title-strong-color|<value|strong-color>>

  <assign|ornaments-body-color|<value|color>>

  <assign|ornaments-body-math-color|<value|math-color>>

  <assign|ornaments-body-strong-color|<value|strong-color>>

  <assign|copy-ornament|<macro|t|from|<with|f|<merge|<arg|from>|-ornament>|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|t>|-shape>>|<value|<unquote|<merge|<value|f>|-shape>>>><assign|<unquote|<merge|<arg|t>|-border>>|<value|<unquote|<merge|<value|f>|-border>>>><assign|<unquote|<merge|<arg|t>|-title-style>>|<value|<unquote|<merge|<value|f>|-title-style>>>><assign|<unquote|<merge|<arg|t>|-title-bg-color>>|<value|<unquote|<merge|<value|f>|-extra-color>>>><assign|<unquote|<merge|<arg|t>|-title-color>>|<value|<unquote|<merge|<value|f>|-title-color>>>><assign|<unquote|<merge|<arg|t>|-body-bg-color>>|<value|<unquote|<merge|<value|f>|-color>>>><assign|<unquote|<merge|<arg|t>|-body-color>>|<value|<unquote|<merge|<arg|from>|-color>>>><assign|<unquote|<merge|<arg|t>|-body-math-color>>|<value|<unquote|<merge|<arg|from>|-math-color>>>><assign|<unquote|<merge|<arg|t>|-body-strong-color>>|<value|<unquote|<merge|<arg|from>|-strong-color>>>><assign|<unquote|<merge|<arg|t>|-sunny-color>>|<value|<unquote|<merge|<value|f>|-sunny-color>>>><assign|<unquote|<merge|<arg|t>|-shadow-color>>|<value|<unquote|<merge|<value|f>|-shadow-color>>>>>>>>

  <\active*>
    <\src-comment>
      Create new decorations from themes
    </src-comment>
  </active*>

  <assign|deco-hook|<macro|body|<arg|body>>>

  <assign|deco-title|<macro|body|<with|color|<value|ornament-title-color>|math-color|<value|ornament-title-math-color>|strong-color|<value|ornament-title-strong-color>|<arg|body>>>>

  <assign|deco-body|<macro|body|<with|color|<value|ornament-body-color>|math-color|<value|ornament-body-math-color>|strong-color|<value|ornament-body-strong-color>|<arg|body>>>>

  <assign|deco|<macro|body|<deco-hook|<ornament|<deco-body|<arg|body>>>>>>

  <assign|deco-block|<\macro|body>
    <\deco-hook>
      <\ornament>
        <\wide-normal>
          <\deco-body>
            <arg|body>
          </deco-body>
        </wide-normal>
      </ornament>
    </deco-hook>
  </macro>>

  <assign|deco-titled|<macro|name|body|<deco-hook|<ornament|<deco-body|<arg|body>>|<deco-title|<arg|name>>>>>>

  <assign|deco-titled-block|<\macro|name|body>
    <\deco-hook>
      <\ornament>
        <\wide-normal>
          <\deco-body>
            <arg|body>
          </deco-body>
        </wide-normal>
      </ornament|<deco-title|<arg|name>>>
    </deco-hook>
  </macro>>

  \;

  <assign|new-deco|<macro|deco|<with|w|<merge|with-|<arg|deco>>|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|env>|-text>>|<macro|<localize|<unquote|<arg|name>>>>><assign|<unquote|<arg|deco>>|<macro|body|<compound|<unquote|<value|w>>|<deco|<arg|body>>>>><assign|<unquote|<merge|<arg|deco>|-titled>>|<macro|name|body|<compound|<unquote|<value|w>>|<deco-titled|<arg|name>|<arg|body>>>>><assign|<unquote|<merge|<arg|deco>|-block>>|<\macro|body>
    <compound|<unquote|<value|w>>|<deco-block|<arg|body>>>
  </macro>><assign|<unquote|<merge|<arg|deco>|-titled-block>>|<\macro|name|body>
    <compound|<unquote|<value|w>>|<deco-titled-block|<arg|name>|<arg|body>>>
  </macro>>>>>>>

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