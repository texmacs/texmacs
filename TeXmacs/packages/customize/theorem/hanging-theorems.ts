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
      Extra style parameter
    </src-comment>
  </active*>

  <assign|hang-length|<if|<unequal|<value|ornament-color>|>|0.15em|0em>>

  <drd-props|hang-length|parameter|length>

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

  <assign|render-enunciation|<\macro|which|body>
    <\padded-normal|<value|theorem-padding-above>|<value|theorem-padding-below>>
      <\with|enunciation-sep||enunciation-name|<value|strong>>
        <\surround|<no-indent>|>
          <\decorated>
            <surround|<no-indent><resize|<with|ornament-color|<value|ornament-extra-color>|dummy|<value|hang-length>|<ornament|<ornament-render-title|<decorated-title|<with|strong-color|<value|color>|<resize|<arg|which>||<minus|0ex|0.5hang>||<plus|1t|1hang>>>>>>>|<plus|1hang|<value|ornament-hpadding>|<value|ornament-border>>|<plus|1b|0.5hang>||<minus|<minus|1t|1hang>|<plus|<value|ornament-vpadding>|<value|ornament-border>>>>
            |<right-flush>|<arg|body>>
          </decorated>
        </surround>
      </with>
    </padded-normal>
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