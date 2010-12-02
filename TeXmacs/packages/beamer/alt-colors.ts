<TeXmacs|1.0.7.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|alt-colors|1.0>

    <\src-purpose>
      An example style package for fancy colors on slides.
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Mathematics in dark red.
    </src-comment>
  </active*>

  <assign|math-color|dark red>

  <assign|greyed-math-color|#d0b0b0>

  <assign|uncolored-math|<value|math>>

  <assign|uncolored-equation*|<value|equation*>>

  <assign|uncolored-equations-base|<value|equations-base>>

  <assign|math|<macro|x|<with|color|<value|math-color>|<uncolored-math|<arg|x>>>>>

  <assign|equation*|<\macro|x>
    <\with|color|<value|math-color>>
      <\uncolored-equation*>
        <arg|x>
      </uncolored-equation*>
    </with>
  </macro>>

  <assign|equations-base|<\macro|x>
    <\with|color|<value|math-color>>
      <\uncolored-equations-base>
        <arg|x>
      </uncolored-equations-base>
    </with>
  </macro>>

  <assign|greyed|<macro|x|<with|color|grey|math-color|<value|greyed-math-color>|<arg|x>>>>

  <\active*>
    <\src-comment>
      Theorems, lists and strong text in dark blue.
    </src-comment>
  </active*>

  <assign|strong-color|dark blue>

  <assign|uncolored-enunciation-name|<value|enunciation-name>>

  <assign|uncolored-render-list|<value|render-list>>

  <assign|uncolored-strong|<value|strong>>

  <assign|enunciation-name|<macro|x|<with|color|<value|strong-color>|<uncolored-enunciation-name|<arg|x>>>>>

  <assign|render-list|<\macro|body>
    <\uncolored-render-list>
      <\with|uncolored-current-item|<value|current-item>|current-item|<macro|name|<with|color|<value|strong-color>|<uncolored-current-item|<arg|name>>>>>
        <arg|body>
      </with>
    </uncolored-render-list>
  </macro>>

  <assign|strong|<macro|x|<with|color|<value|strong-color>|<uncolored-strong|<arg|x>>>>>

  <\active*>
    <\src-comment>
      Blinking
    </src-comment>
  </active*>

  <assign|blink|<macro|x|<anim-repeat|<anim-compose|<anim-constant|<arg|x>|1sec>|<anim-constant||0.5sec>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>