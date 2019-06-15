<TeXmacs|1.99.9>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|poster|1.0>

      <\src-purpose>
        Style for A0 posters.
      </src-purpose>

      <\src-copyright|2018>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|generic|two-columns>

  <\active*>
    <\src-comment>
      Global page style.
    </src-comment>
  </active*>

  <assign|page-type|a0>

  <assign|page-medium|paper>

  <assign|page-screen-margin|false>

  <assign|magnification|3.2>

  <assign|par-line-sep|2fns>

  \;

  <assign|page-odd|3.2cm>

  <assign|page-even|3.2cm>

  <assign|page-right|3.2cm>

  <assign|page-top|3.2cm>

  <assign|page-bot|3.2cm>

  \;

  <assign|page-odd-footer|>

  <assign|page-even-footer|>

  <\active*>
    <\src-comment>
      Common default settings for block.
    </src-comment>
  </active*>

  <assign|block-render-title|<macro|name|<with|font-series|bold|<large|<arg|name>>>>>

  <assign|ornament-title-color|black>

  <assign|ornament-extra-color|white>

  <assign|ornament-color|white>

  <assign|ornament-sunny-color|#e0c0c0>

  <assign|ornament-shadow-color|#f0e0e0>

  <assign|ornament-hpadding|2spc>

  <assign|ornament-vpadding|2spc>

  <\active*>
    <\src-comment>
      Title blocks.
    </src-comment>
  </active*>

  <assign|with-title-font|<macro|name|<with|font-series|bold|<huge|<arg|name>>>>>

  <assign|title-body-bg-color|white>

  <assign|title-body-color|black>

  <assign|title-shape|classic>

  <assign|title-sunny-color|#e0c0c0>

  <assign|title-shadow-color|#f0e0e0>

  \;

  <assign|title-swell|0em>

  <assign|title-border|2ln>

  <assign|title-hpadding|2spc>

  <assign|title-vpadding|4spc>

  <assign|title-vsep|1spc>

  \;

  <assign|deco-hook|<macro|body|<arg|body>>>

  <assign|title-colored|<\macro|body>
    <\with|color|<value|title-body-color>|math-color|<value|title-body-color>|strong-color|<value|title-body-color>>
      <arg|body>
    </with>
  </macro>>

  <assign|title-block|<\macro|body>
    <\with|ornament-color|<value|title-body-bg-color>|ornament-shape|<value|title-shape>|ornament-sunny-color|<value|title-sunny-color>|ornament-shadow-color|<value|title-shadow-color>>
      <\deco-hook>
        <\ornament>
          <\title-colored>
            <\wide-normal>
              <arg|body>
            </wide-normal>
          </title-colored>
        </ornament>
      </deco-hook>
    </with>
  </macro>>

  <assign|poster-title|<\macro|body|ldeco|rdeco>
    <\with|par-columns|1|par-left|<minus|<value|par-left>|<value|title-swell>>|par-right|<minus|<value|par-right>|<value|title-swell>>|deco-hook|<macro|body|<with|ornament-border|<value|title-border>|ornament-hpadding|<value|title-hpadding>|ornament-vpadding|<value|title-vpadding>|<arg|body>>>>
      <\surround||<vspace|<value|title-vsep>>>
        <\title-block>
          <\surround|<resize|<arg|ldeco>|||1l|><htab|5mm>|<htab|5mm><resize|<arg|rdeco>|1r|||>>
            <arg|body>
          </surround>
        </title-block>
      </surround>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Plain blocks.
    </src-comment>
  </active*>

  <assign|plain-render-title|<macro|name|<block-render-title|<arg|name>>>>

  \;

  <assign|plain-block|<\macro|body>
    <\with|par-line-sep|0fns>
      <\wide-normal>
        <arg|body>
      </wide-normal>
    </with>
  </macro>>

  <assign|plain-titled-block-title|<macro|name|<wide-normal|<plain-render-title|<arg|name>>>>>

  <assign|plain-titled-block|<\macro|name|body>
    <\surround||<vspace|<value|par-line-sep>>>
      <\with|par-line-sep|0fns>
        <surround||<no-break-here>|<plain-titled-block-title|<arg|name>>>

        <\wide-normal>
          <arg|body>
        </wide-normal>
      </with>
    </surround>
  </macro>>

  <\active*>
    <\src-comment>
      Framed blocks.
    </src-comment>
  </active*>

  <assign|framed-render-title|<macro|name|<block-render-title|<arg|name>>>>

  <assign|framed-shape|classic>

  <assign|framed-title-style|classic>

  <assign|framed-title-bg-color|dark blue>

  <assign|framed-title-color|pastel yellow>

  <assign|framed-body-bg-color|white>

  <assign|framed-body-color|dark brown>

  <assign|framed-body-math-color|dark red>

  <assign|framed-body-strong-color|dark blue>

  <assign|framed-sunny-color|#e0c0c0>

  <assign|framed-shadow-color|#f0e0e0>

  \;

  <assign|framed-colored|<\macro|body>
    <\with|color|<value|framed-body-color>>
      <arg|body>
    </with>
  </macro>>

  <assign|framed-block|<\macro|body>
    <\with|ornament-color|<value|framed-body-bg-color>|ornament-shape|<value|framed-shape>|ornament-sunny-color|<value|framed-sunny-color>|ornament-shadow-color|<value|framed-shadow-color>>
      <\ornament>
        <\framed-colored>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </framed-colored>
      </ornament>
    </with>
  </macro>>

  <assign|framed-block*|<\macro|name|body>
    <\with|ornament-extra-color|<value|framed-title-bg-color>|ornament-title-color|<value|framed-title-color>|ornament-color|<value|framed-body-bg-color>|ornament-shape|<value|framed-shape>|ornament-title-style|<value|framed-title-style>|ornament-sunny-color|<value|framed-sunny-color>|ornament-shadow-color|<value|framed-shadow-color>>
      <\ornament>
        <\framed-colored>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </framed-colored>
      </ornament|<with|color|<value|framed-title-color>|math-color|<value|framed-title-color>|strong-color|<value|framed-title-color>|<arg|name>>>
    </with>
  </macro>>

  <assign|framed-titled-block|<\macro|name|body>
    <\framed-block*|<framed-render-title|<arg|name>>>
      <arg|body>
    </framed-block*>
  </macro>>

  <\active*>
    <\src-comment>
      Alternate framed blocks.
    </src-comment>
  </active*>

  <assign|alternate-render-title|<macro|name|<block-render-title|<arg|name>>>>

  <assign|alternate-shape|classic>

  <assign|alternate-title-style|classic>

  <assign|alternate-title-bg-color|dark red>

  <assign|alternate-title-color|white>

  <assign|alternate-body-bg-color|#fff8e0>

  <assign|alternate-body-color|black>

  <assign|alternate-body-math-color|dark red>

  <assign|alternate-body-strong-color|dark blue>

  <assign|alternate-sunny-color|#e0c0c0>

  <assign|alternate-shadow-color|#f0e0e0>

  \;

  <assign|alternate-colored|<\macro|body>
    <\with|color|<value|alternate-body-color>>
      <arg|body>
    </with>
  </macro>>

  <assign|alternate-block|<\macro|body>
    <\with|ornament-color|<value|alternate-body-bg-color>|ornament-shape|<value|alternate-shape>|ornament-sunny-color|<value|alternate-sunny-color>|ornament-shadow-color|<value|alternate-shadow-color>>
      <\ornament>
        <\alternate-colored>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </alternate-colored>
      </ornament>
    </with>
  </macro>>

  <assign|alternate-block*|<\macro|name|body>
    <\with|ornament-extra-color|<value|alternate-title-bg-color>|ornament-title-color|<value|alternate-title-color>|ornament-color|<value|alternate-body-bg-color>|ornament-shape|<value|alternate-shape>|ornament-title-style|<value|alternate-title-style>|ornament-sunny-color|<value|alternate-sunny-color>|ornament-shadow-color|<value|alternate-shadow-color>>
      <\ornament>
        <\alternate-colored>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </alternate-colored>
      </ornament|<with|color|<value|alternate-title-color>|math-color|<value|alternate-title-color>|strong-color|<value|alternate-title-color>|<arg|name>>>
    </with>
  </macro>>

  <assign|alternate-titled-block|<\macro|name|body>
    <\alternate-block*|<alternate-render-title|<arg|name>>>
      <arg|body>
    </alternate-block*>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>