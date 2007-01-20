<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|alt-colors|1.0>

    <\src-purpose>
      An example style package for fancy colors on slides.
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Greyed portions of text.
    </src-comment>
  </active*>

  <assign|is-greyed|false>

  <assign|greyed|<macro|x|<with|is-greyed|true|color|<if|<equal|<value|mode>|math>|#d0b0b0|grey>|<arg|x>>>>

  <\active*>
    <\src-comment>
      Mathematics in dark red.
    </src-comment>
  </active*>

  <assign|uncolored-math|<value|math>>

  <assign|uncolored-equation*|<value|equation*>>

  <assign|uncolored-equations-base|<value|equations-base>>

  <assign|math|<macro|x|<with|color|<if|<value|is-greyed>|#d0b0b0|dark
  red>|<uncolored-math|<arg|x>>>>>

  <assign|equation*|<\macro|x>
    <\with|color|<if|<value|is-greyed>|#d0b0b0|dark red>>
      <\uncolored-equation*>
        <arg|x>
      </uncolored-equation*>
    </with>
  </macro>>

  <assign|equations-base|<\macro|x>
    <\with|color|<if|<value|is-greyed>|#d0b0b0|dark red>>
      <\uncolored-equations-base>
        <arg|x>
      </uncolored-equations-base>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Theorems and strong text in dark blue.
    </src-comment>
  </active*>

  <assign|uncolored-theorem-name|<value|theorem-name>>

  <assign|uncolored-exercise-name|<value|exercise-name>>

  <assign|uncolored-strong|<value|strong>>

  <assign|theorem-name|<macro|x|<with|color|dark
  blue|<uncolored-theorem-name|<arg|x>>>>>

  <assign|exercise-name|<macro|x|<with|color|dark
  blue|<uncolored-exercise-name|<arg|x>>>>>

  <assign|strong|<macro|x|<with|color|dark blue|<uncolored-strong|<arg|x>>>>>

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