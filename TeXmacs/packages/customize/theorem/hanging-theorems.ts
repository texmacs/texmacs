<TeXmacs|1.99.9>

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

  <assign|enunciation-sep|>

  <assign|enunciation-name|<macro|which|<with|font-series|bold|<arg|which>>>>

  <provide|unframed-render-enunciation|<value|render-enunciation>>

  <assign|render-enunciation|<\macro|which|body>
    <\surround|<no-indent>|>
      <\decorated>
        <surround|<no-indent><resize|<with|ornament-color|<value|ornament-extra-color>|dummy|<value|hang-length>|<ornament|<ornament-render-title|<resize|<arg|which>||<minus|0ex|0.5hang>||<plus|1t|1hang>>>>>|<plus|1hang|<value|ornament-hpadding>|<value|ornament-border>>|<plus|1b|0.5hang>||<minus|<minus|1t|1hang>|<plus|<value|ornament-vpadding>|<value|ornament-border>>>>
        |<right-flush>|<arg|body>>
      </decorated>
    </surround>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>