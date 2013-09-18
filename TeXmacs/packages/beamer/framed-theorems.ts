<TeXmacs|1.0.7.19>

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
      Framed theorems
    </src-comment>
  </active*>

  <assign|enunciation-sep| >

  <assign|enunciation-name|<macro|which|<with|color|<value|ornament-color>|font-series|bold|<arg|which>>>>

  <assign|unframed-render-enunciation|<value|render-enunciation>>

  <assign|render-enunciation|<\macro|which|body>
    <\ornament>
      <\surround||<right-flush>>
        <arg|body>
      </surround>
    </ornament|<arg|which>>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>