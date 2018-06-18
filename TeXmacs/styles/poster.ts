<TeXmacs|1.99.6>

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

  <assign|ornament-hpadding|2spc>

  <assign|ornament-vpadding|2spc>

  <\active*>
    <\src-comment>
      Title blocks.
    </src-comment>
  </active*>

  <assign|title-render-title|<macro|name|<with|font-series|bold|<huge|<arg|name>>>>>

  <assign|title-body-bg-color|white>

  <assign|title-body-color|black>

  <assign|title-shape|classic>

  <assign|title-sunny-color|#e0c0c0>

  <assign|title-shadow-color|#f0e0e0>

  \;

  <assign|title-swell|0em>

  <assign|title-border|1ln>

  <assign|title-hpadding|2spc>

  <assign|title-vpadding|4spc>

  \;

  <assign|poster-title|<\macro|body|ldeco|rdeco>
    <\with|par-columns|1|ornament-color|<value|title-body-bg-color>|color|<value|title-body-color>|ornament-shape|<value|title-shape>|par-left|<minus|<value|par-left>|<value|title-swell>>|par-right|<minus|<value|par-right>|<value|title-swell>>|ornament-border|<value|title-border>|ornament-hpadding|<value|title-hpadding>|ornament-vpadding|<value|title-vpadding>|ornament-sunny-color|<value|title-sunny-color>|ornament-shadow-color|<value|title-shadow-color>>
      <\ornament>
        <\surround|<resize|<arg|ldeco>|||1l|><htab|5mm>|<htab|5mm><resize|<arg|rdeco>|1r|||>>
          <\title-render-title>
            <arg|body>
          </title-render-title>
        </surround>
      </ornament>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Plain blocks.
    </src-comment>
  </active*>

  <assign|plain-render-title|<macro|name|<block-render-title|<arg|name>>>>

  <assign|plain-title-bg-color|none>

  <assign|plain-title-color|black>

  <assign|plain-body-bg-color|none>

  <assign|plain-body-color|black>

  <assign|plain-shape|classic>

  \;

  <assign|plain-block|<\macro|body>
    <\with|color|<value|plain-body-color>>
      <\wide-normal>
        <arg|body>
      </wide-normal>
    </with>
  </macro>>

  <assign|plain-non-framed-title|<macro|name|<with|color|<value|plain-title-color>|<plain-render-title|<arg|name>>>>>

  <assign|plain-framed-title|<macro|name|<with|ornament-color|<value|plain-title-bg-color>|color|<value|plain-title-color>|ornament-shape|<value|plain-shape>|<ornament|<wide-normal|<space|0fn><plain-render-title|<arg|name>>
  >>>>>

  <assign|plain-titled-block|<\macro|name|body>
    <surround||<no-break-here>|<compound|<if|<equal|<value|plain-title-bg-color>|none>|plain-non-framed-title|plain-framed-title>|<arg|name>>>

    <\with|dummy|<value|plain-title-color>|color|<value|plain-body-color>>
      <\wide-normal>
        <arg|body>
      </wide-normal>
    </with>
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

  <assign|framed-sunny-color|#e0c0c0>

  <assign|framed-shadow-color|#f0e0e0>

  \;

  <assign|framed-block|<\macro|body>
    <\with|ornament-color|<value|framed-body-bg-color>|ornament-shape|<value|framed-shape>|ornament-sunny-color|<value|framed-sunny-color>|ornament-shadow-color|<value|framed-shadow-color>>
      <\ornament>
        <\with|color|<value|framed-body-color>>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </with>
      </ornament>
    </with>
  </macro>>

  <assign|framed-titled-block|<\macro|name|body>
    <\with|ornament-extra-color|<value|framed-title-bg-color>|ornament-color|<value|framed-body-bg-color>|ornament-shape|<value|framed-shape>|ornament-title-style|<value|framed-title-style>|ornament-sunny-color|<value|framed-sunny-color>|ornament-shadow-color|<value|framed-shadow-color>>
      <\ornament>
        <\with|color|<value|framed-body-color>>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </with>
      </ornament|<with|color|<value|framed-title-color>|<framed-render-title|<arg|name>>>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Alternate framed blocks.
    </src-comment>
  </active*>

  <assign|alternate-render-title|<macro|name|<block-render-title|<arg|name>>>>

  <assign|alternate-shape|rounded>

  <assign|alternate-title-style|rounded>

  <assign|alternate-title-bg-color|dark red>

  <assign|alternate-title-color|white>

  <assign|alternate-body-bg-color|#fff8e0>

  <assign|alternate-body-color|black>

  <assign|alternate-sunny-color|#e0c0c0>

  <assign|alternate-shadow-color|#f0e0e0>

  \;

  <assign|alternate-block|<\macro|body>
    <\with|ornament-color|<value|alternate-body-bg-color>|ornament-shape|<value|alternate-shape>|ornament-sunny-color|<value|alternate-sunny-color>|ornament-shadow-color|<value|alternate-shadow-color>>
      <\ornament>
        <\with|color|<value|alternate-body-color>>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </with>
      </ornament>
    </with>
  </macro>>

  <assign|alternate-titled-block|<\macro|name|body>
    <\with|ornament-extra-color|<value|alternate-title-bg-color>|ornament-color|<value|alternate-body-bg-color>|ornament-shape|<value|alternate-shape>|ornament-title-style|<value|alternate-title-style>|ornament-sunny-color|<value|alternate-sunny-color>|ornament-shadow-color|<value|alternate-shadow-color>>
      <\ornament>
        <\with|color|<value|alternate-body-color>>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </with>
      </ornament|<with|color|<value|alternate-title-color>|<alternate-render-title|<arg|name>>>>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>