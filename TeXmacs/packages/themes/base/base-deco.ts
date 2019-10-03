<TeXmacs|1.99.11>

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

  <use-package|base-colors|std-shadow>

  <\active*>
    <\src-comment>
      Ornaments as decorations
    </src-comment>
  </active*>

  <assign|ornament-render-title|<macro|body|<arg|body>>>

  <assign|ornament-render-body|<macro|body|<arg|body>>>

  <assign|ornament-render-shadow|<macro|body|<compound|<merge|<value|ornament-shape>|-shadow>|<arg|body>>>>

  <new-theme|deco|ornament-shape|ornament-border|ornament-corner|ornament-hpadding|ornament-vpadding|ornament-color|ornament-sunny-color|ornament-shadow-color|ornament-title-style|ornament-extra-color|ornament-render-title|ornament-render-body|ornament-render-shadow|deco-shadow-effect|deco-title-shadow-effect>

  <\active*>
    <\src-comment>
      Extract color information using ornament-render-title and
      ornament-render-body macros
    </src-comment>
  </active*>

  <assign|complete-deco|<macro|deco|<quasi|<style-with|src-compact|none|<with|ornament-body-color|<value|<unquote|<merge|<arg|deco>|-ornament-color>>>|bg-color|<value|ornament-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-body>>|<assign|<unquote|<merge|<arg|deco>|-ornament-body-bg-color>>|<value|bg-color>><assign|<unquote|<merge|<arg|deco>|-ornament-color>>|<value|bg-color>>>><with|ornament-body-color|<value|<unquote|<merge|<arg|deco>|-ornament-body-color>>>|color|<value|ornament-body-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-body>>|<assign|<unquote|<merge|<arg|deco>|-ornament-body-color>>|<value|color>>>><with|ornament-body-math-color|<value|<unquote|<merge|<arg|deco>|-ornament-body-math-color>>>|color|<value|ornament-body-math-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-body>>|<assign|<unquote|<merge|<arg|deco>|-ornament-body-math-color>>|<value|math-color>>>><with|ornament-body-strong-color|<value|<unquote|<merge|<arg|deco>|-ornament-body-strong-color>>>|color|<value|ornament-body-strong-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-body>>|<assign|<unquote|<merge|<arg|deco>|-ornament-body-strong-color>>|<value|strong-color>>>><with|ornament-title-bg-color|<value|<unquote|<merge|<arg|deco>|-ornament-extra-color>>>|bg-color|<value|ornament-extra-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-title>>|<assign|<unquote|<merge|<arg|deco>|-ornament-title-bg-color>>|<value|bg-color>><assign|<unquote|<merge|<arg|deco>|-ornament-extra-color>>|<value|bg-color>>>><with|ornament-title-color|<value|<unquote|<merge|<arg|deco>|-ornament-title-color>>>|color|<value|ornament-title-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-title>>|<assign|<unquote|<merge|<arg|deco>|-ornament-title-color>>|<value|color>>>><with|ornament-title-math-color|<value|<unquote|<merge|<arg|deco>|-ornament-title-math-color>>>|color|<value|ornament-title-math-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-title>>|<assign|<unquote|<merge|<arg|deco>|-ornament-title-math-color>>|<value|math-color>>>><with|ornament-title-strong-color|<value|<unquote|<merge|<arg|deco>|-ornament-title-strong-color>>>|color|<value|ornament-title-strong-color>|<compound|<unquote|<merge|<arg|deco>|-ornament-render-title>>|<assign|<unquote|<merge|<arg|deco>|-ornament-title-strong-color>>|<value|strong-color>>>>>>>>

  <\active*>
    <\src-comment>
      Create new decorations from themes
    </src-comment>
  </active*>

  <assign|deco-hook|<macro|body|<arg|body>>>

  <assign|deco-hook*|<macro|body|<with|bg-color|<value|ornament-extra-color>|old-color|<value|color>|old-math-color|<value|math-color>|old-strong-color|<value|strong-color>|<ornament-render-title|<with|ornament-extra-color|<value|bg-color>|color|<value|old-color>|math-color|<value|old-math-color>|strong-color|<value|old-strong-color>|<with|bg-color|<value|ornament-color>|<ornament-render-body|<with|ornament-color|<value|bg-color>|<deco-hook|<arg|body>>>>>>>>>>

  <assign|deco-title|<macro|body|<ornament-render-title|<arg|body>>>>

  <assign|deco-body|<macro|body|<ornament-render-body|<arg|body>>>>

  \;

  <assign|deco|<macro|body|<deco-hook*|<ornament|<deco-body|<arg|body>>>>>>

  <assign|deco-block|<\macro|body>
    <\deco-hook*>
      <\ornament>
        <\wide-normal>
          <\deco-body>
            <arg|body>
          </deco-body>
        </wide-normal>
      </ornament>
    </deco-hook*>
  </macro>>

  <assign|deco-titled|<macro|name|body|<deco-hook*|<ornament|<deco-body|<arg|body>>|<deco-title|<arg|name>>>>>>

  <assign|deco-titled-block|<\macro|name|body>
    <\deco-hook*>
      <\ornament>
        <\wide-normal>
          <\deco-body>
            <arg|body>
          </deco-body>
        </wide-normal>
      </ornament|<deco-title|<arg|name>>>
    </deco-hook*>
  </macro>>

  \;

  <assign|new-deco|<macro|deco|<with|w|<merge|with-|<arg|deco>>|<quasi|<style-with|src-compact|none|<assign|<unquote|<arg|deco>>|<macro|body|<compound|<unquote|<value|w>>|<deco|<arg|body>>>>><assign|<unquote|<merge|<arg|deco>|-titled>>|<macro|name|body|<compound|<unquote|<value|w>>|<deco-titled|<arg|name>|<arg|body>>>>><assign|<unquote|<merge|<arg|deco>|-block>>|<\macro|body>
    <compound|<unquote|<value|w>>|<deco-block|<arg|body>>>
  </macro>><assign|<unquote|<merge|<arg|deco>|-titled-block>>|<\macro|name|body>
    <compound|<unquote|<value|w>>|<deco-titled-block|<arg|name>|<arg|body>>>
  </macro>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>