<TeXmacs|1.0.6.12>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|alt-colors|1.0>

    <\src-purpose>
      An example style package for fancy colors on slides.
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
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
      Theorems and strong text in dark blue.
    </src-comment>
  </active*>

  <assign|strong-color|dark blue>

  <assign|uncolored-theorem-name|<value|theorem-name>>

  <assign|uncolored-exercise-name|<value|exercise-name>>

  <assign|uncolored-strong|<value|strong>>

  <assign|theorem-name|<macro|x|<with|color|<value|strong-color>|<uncolored-theorem-name|<arg|x>>>>>

  <assign|exercise-name|<macro|x|<with|color|<value|strong-color>|<uncolored-exercise-name|<arg|x>>>>>

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