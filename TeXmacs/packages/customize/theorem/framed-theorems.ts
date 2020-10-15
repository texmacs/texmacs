<TeXmacs|1.99.13>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|framed-theorems|1.0>

    <\src-purpose>
      A style package for ornamented theorems.
    </src-purpose>

    <src-copyright|2013|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Extra style parameters
    </src-comment>
  </active*>

  <assign|frame-titles|<unequal|<value|ornament-color>|>>

  <drd-props|frame-titles|parameter|boolean>

  <\active*>
    <\src-comment>
      Framed theorems
    </src-comment>
  </active*>

  <assign|theorem-padding-above|<value|large-padding-above>>

  <assign|theorem-padding-below|<value|large-padding-below>>

  <if|<provides|beamer-style>|<assign|theorem-padding-above|0fn><assign|theorem-padding-below|0fn>>

  \;

  <provide|unframed-render-enunciation|<value|render-enunciation>>

  <provide|unframed-render-theorem|<value|render-theorem>>

  <provide|unframed-render-remark|<value|render-remark>>

  <provide|unframed-render-proof|<value|render-proof>>

  \;

  <assign|framed-render-enunciation|<\macro|which|body>
    <\padded-normal|<value|theorem-padding-above>|<value|theorem-padding-below>>
      <\surround|<no-indent>|>
        <\decorated>
          <\unframed-render-enunciation|<arg|which>>
            <arg|body>
          </unframed-render-enunciation>
        </decorated>
      </surround>
    </padded-normal>
  </macro>>

  <assign|framed-render-enunciation*|<\macro|which|body>
    <\padded-normal|<value|theorem-padding-above>|<value|theorem-padding-below>>
      <\with|enunciation-sep|>
        <\surround|<no-indent>|>
          <\decorated-titled|<with|strong-color|<value|color>|math-color|<value|color>|<arg|which>>>
            <\surround|<no-indent>|<right-flush>>
              <arg|body>
            </surround>
          </decorated-titled>
        </surround>
      </with>
    </padded-normal>
  </macro>>

  <assign|render-enunciation|<\macro|which|body>
    <\with|dummy1|<ornament|>|d2|<value|theorem-padding-above>|d3|<value|theorem-padding-below>>
      <compound|<if|<value|frame-titles>|framed-render-enunciation*|framed-render-enunciation>|<arg|which>|<arg|body>>
    </with>
  </macro>>

  \;

  <assign|render-remark|<\macro|which|body>
    <\with|render-enunciation|<value|unframed-render-enunciation>>
      <\unframed-render-remark|<arg|which>>
        <arg|body>
      </unframed-render-remark>
    </with>
  </macro>>

  <assign|render-proof|<\macro|which|body>
    <\with|render-enunciation|<value|unframed-render-enunciation>>
      <\unframed-render-proof|<arg|which>>
        <arg|body>
      </unframed-render-proof>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>