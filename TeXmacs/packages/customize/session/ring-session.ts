<TeXmacs|1.99.9>

<style|<tuple|source|english>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|ring-session|1.0|session|1.0>

    <\src-purpose>
      New style package for fancy notebooks
    </src-purpose>

    <src-copyright|2013|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|session>

  <\active*>
    <\src-comment>
      Style parameters.
    </src-comment>
  </active*>

  <assign|session-ring|<pattern|$TEXMACS_PATH/misc/images/ring-binder-2.png|100%||#fff0>>

  <assign|session-ring-width|0.75fn>

  <assign|session-color|#fdfdfa>

  <assign|session-border|2ln>

  <assign|session-border-color|<tuple|pastel grey|grey>>

  <assign|session-left-indent|0.5fn>

  <assign|session-right-indent|0.5fn>

  <assign|session-sep|2ln>

  \;

  <assign|input-color|pastel yellow>

  <assign|input-border|2ln>

  <assign|input-border-color|<tuple|pastel grey|grey>>

  <assign|input-vpadding|0.4fn>

  <assign|output-vpadding|0.6fn>

  \;

  <assign|error-color|pastel red>

  <assign|error-border|1ln>

  <assign|error-border-color|<tuple|dark red>>

  <assign|error-vpadding|0.6fn>

  <assign|generic-error-color|dark red>

  \;

  <assign|fold-title-color|#dcd0b8>

  <assign|fold-bar-color|#ede8dc>

  <assign|fold-bar-border|1ln>

  <assign|fold-bar-border-color|<tuple|pastel grey|grey>>

  <\active*>
    <\src-comment>
      Entire sessions
    </src-comment>
  </active*>

  <assign|generic-session|<\macro|name|body>
    <\padded>
      <\with|session-par-sep|<value|par-sep>|session-par-ver-sep|<value|par-ver-sep>|par-sep|<value|session-sep>|par-ver-sep|<value|session-sep>|par-first|0fn|par-par-sep|0fn|par-line-sep|0fn|old-shape|<value|ornament-shape>|old-color|<value|ornament-color>|old-hpadding|<value|ornament-hpadding>|old-vpadding|<value|ornament-vpadding>|old-border|<value|ornament-border>|old-sunny-color|<value|ornament-sunny-color>|ornament-shape|ring|ornament-color|<value|session-ring>|ornament-hpadding|<over|<value|session-ring-width>|2>|ornament-vpadding|0spc>
        <\ornament>
          <with|ornament-shape|classic|ornament-color|<value|session-color>|ornament-border|<value|session-border>|ornament-sunny-color|<value|session-border-color>|ornament-hpadding|0spc|<\ornament>
            <\surround||<right-flush>>
              <\with|ornament-shape|<value|old-shape>|ornament-color|<value|old-color>|ornament-border|<value|old-border>|ornament-sunny-color|<value|old-sunny-color>|ornament-hpadding|<value|old-hpadding>|ornament-vpadding|<value|old-vpadding>>
                <arg|body>
              </with>
            </surround>
          </ornament>>
        </ornament>
      </with>
    </padded>
  </macro>>

  <assign|subsession|<\macro|body>
    <\ornament-indent|<value|session-ring-width>|<value|session-right-indent>|<value|output-vpadding>|<value|output-vpadding>>
      <\with|old-shape|<value|ornament-shape>|old-color|<value|ornament-color>|old-hpadding|<value|ornament-hpadding>|old-vpadding|<value|ornament-vpadding>|old-border|<value|ornament-border>|old-sunny-color|<value|ornament-sunny-color>|ornament-shape|ring|ornament-color|<value|session-ring>|ornament-hpadding|<over|<value|session-ring-width>|2>|ornament-vpadding|0spc>
        <\ornament>
          <with|ornament-shape|classic|ornament-color|<value|session-color>|ornament-border|<value|session-border>|ornament-sunny-color|<value|session-border-color>|ornament-hpadding|0spc|<\ornament>
            <\surround||<right-flush>>
              <\with|ornament-shape|<value|old-shape>|ornament-color|<value|old-color>|ornament-border|<value|old-border>|ornament-hpadding|<value|old-hpadding>|ornament-sunny-color|<value|old-sunny-color>|ornament-vpadding|<value|old-vpadding>>
                <arg|body>
              </with>
            </surround>
          </ornament>>
        </ornament>
      </with>
    </ornament-indent>
  </macro>>

  <\active*>
    <\src-comment>
      Fields for input, output, text and errors.
    </src-comment>
  </active*>

  <assign|generic-input|<\macro|prompt|body>
    <\with|ornament-shape|classic|ornament-color|<value|input-color>|ornament-hpadding|<tuple|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>>|ornament-vpadding|<value|input-vpadding>|ornament-border|<value|input-border>|ornament-sunny-color|<value|input-border-color>>
      <\ornament>
        <surround||<right-flush>|<tabbed*|<tformat|<table|<row|<cell|<id-function|<with|color|<value|generic-prompt-color>|<arg|prompt>>>>|<\cell>
          <\with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
            <arg|body>
          </with>
        </cell>>>>>>
      </ornament>
    </with>
  </macro>>

  <assign|generic-output*|<macro|body|<with|par-mode|justify|par-flexibility|2.0|par-hyphen|normal|math-display|true|math-frac-limit|<value|session-frac-limit>|math-table-limit|<value|session-table-limit>|<ornament-indent|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>|<value|output-vpadding>|<value|output-vpadding>|<with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>|<arg|body>>>>>>

  <assign|generic-output|<macro|body|<generic-output*|<arg|body>>>>

  <assign|generic-textput|<macro|body|<ornament-indent|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>|<value|output-vpadding>|<value|output-vpadding>|<with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>|<arg|body>>>>>

  <assign|generic-errput|<\macro|body>
    <\with|ornament-shape|classic|ornament-color|<value|error-color>|ornament-vpadding|<value|error-vpadding>|ornament-border|<value|error-border>|ornament-sunny-color|<value|error-border-color>>
      <\ornament>
        <\surround||<right-flush>>
          <\with|color|<value|generic-error-color>|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
            <arg|body>
          </with>
        </surround>
      </ornament>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Folding
    </src-comment>
  </active*>

  <assign|folded-subsession|<\macro|x|y>
    <\with|ornament-shape|classic|ornament-color|<value|fold-title-color>|ornament-hpadding|<tuple|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>>|ornament-vpadding|<value|input-vpadding>|ornament-border|<value|input-border>|ornament-sunny-color|<value|input-border-color>>
      <\ornament>
        <\surround||<right-flush><action|<math|\<Downarrow\>>|mouse-unfold|<arg|x>>>
          <\with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
            <arg|x>
          </with>
        </surround>
      </ornament>
    </with>
  </macro>>

  <assign|unfolded-subsession|<\macro|x|y>
    <\with|ornament-shape|classic|ornament-color|<value|fold-title-color>|ornament-hpadding|<tuple|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>>|ornament-vpadding|<value|input-vpadding>|ornament-border|<value|input-border>|ornament-sunny-color|<value|input-border-color>>
      <\ornament>
        <\surround||<right-flush><action|<math|\<Uparrow\>>|mouse-fold|<arg|x>>>
          <\with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
            <arg|x>
          </with>
        </surround>
      </ornament>

      <\subsession>
        <arg|y>
      </subsession>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>