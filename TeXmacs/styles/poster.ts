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
      Default settings for ornaments.
    </src-comment>
  </active*>

  <assign|ornament-hpadding|2spc>

  <assign|ornament-vpadding|2spc>

  <\active*>
    <\src-comment>
      Plain blocks.
    </src-comment>
  </active*>

  <assign|plain-render-title|<macro|body|<with|font-series|bold|<large|<arg|body>>>>>

  <assign|plain-title-bg-color|none>

  <assign|plain-title-color|pastel yellow>

  <assign|plain-body-bg-color|none>

  <assign|plain-body-color|black>

  <assign|plain-shape|classic>

  \;

  <assign|plain-block|<\macro|name>
    <\with|color|<value|plain-body-color>>
      <\wide-normal>
        <arg|body>
      </wide-normal>
    </with>
  </macro>>

  <assign|plain-title|<macro|name|<with|ornament-color|<value|plain-title-bg-color>|color|<value|plain-title-color>|<plain-render-title|<arg|name>>>>>

  <assign|plain-titled-block|<\macro|name|body>
    <compound|<if|<equal|<value|plain-title-bg-color>|none>|plain-render-title|plain-title>|<arg|name>>

    <\with|color|<value|framed-body-color>>
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

  <assign|framed-title-style|<macro|body|<with|font-series|bold|<large|<arg|body>>>>>

  <assign|framed-title-bg-color|dark blue>

  <assign|framed-title-color|pastel yellow>

  <assign|framed-body-bg-color|white>

  <assign|framed-body-color|dark brown>

  <assign|framed-shape|classic>

  \;

  <assign|framed-block|<\macro|body>
    <\with|ornament-color|<value|framed-body-bg-color>|ornament-shape|<value|framed-shape>>
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
    <\with|ornament-extra-color|<value|framed-title-bg-color>|ornament-color|<value|framed-body-bg-color>|ornament-shape|<value|framed-shape>>
      <\ornament>
        <\with|color|<value|framed-body-color>>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </with>
      </ornament|<with|color|<value|framed-title-color>|<framed-title-style|<arg|name>>>>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Alternate framed blocks.
    </src-comment>
  </active*>

  <assign|alternate-title-style|<macro|body|<with|font-family|ss|font-series|bold|<large|<arg|body>>>>>

  <assign|alternate-title-bg-color|dark red>

  <assign|alternate-title-color|white>

  <assign|alternate-body-bg-color|#fff8e0>

  <assign|alternate-body-color|black>

  <assign|alternate-shape|rounded>

  \;

  <assign|alternate-block|<\macro|body>
    <\with|ornament-color|<value|alternate-body-bg-color>|ornament-shape|<value|alternate-shape>>
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
    <\with|ornament-extra-color|<value|alternate-title-bg-color>|ornament-color|<value|alternate-body-bg-color>|ornament-shape|<value|alternate-shape>>
      <\ornament>
        <\with|color|<value|alternate-body-color>>
          <\wide-normal>
            <arg|body>
          </wide-normal>
        </with>
      </ornament|<with|color|<value|alternate-title-color>|<alternate-title-style|<arg|name>>>>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>