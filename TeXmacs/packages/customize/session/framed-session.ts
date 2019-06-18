<TeXmacs|1.99.9>

<style|<tuple|source|english>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|framed-session|1.0|session|1.0>

    <\src-purpose>
      European-style numbering.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

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

  <assign|session-band-width|0.75fn>

  <assign|session-border|1ln>

  <assign|session-border-color|<tuple|pastel grey|grey>>

  <assign|session-left-indent|0.5fn>

  <assign|session-right-indent|0.5fn>

  <assign|session-sep|1ln>

  \;

  <assign|input-color|pastel yellow>

  <assign|input-border|1ln>

  <assign|input-border-color|<tuple|pastel grey|grey>>

  <assign|input-vpadding|0.3333fn>

  <assign|output-vpadding|0.5fn>

  \;

  <assign|error-color|pastel red>

  <assign|error-border|1ln>

  <assign|error-border-color|<tuple|dark red>>

  <assign|error-vpadding|0.5fn>

  <assign|generic-error-color|dark red>

  \;

  <assign|fold-title-color|#dcd0b8>

  <assign|fold-bar-color|#ede8dc>

  <assign|fold-bar-border|1ln>

  <assign|fold-bar-border-color|<tuple|pastel grey|grey>>

  <\active*>
    <\src-comment>
      New sessions
    </src-comment>
  </active*>

  <assign|with-fold-bar-deco|<macro|body|<with|ornament-color|<value|fold-bar-color>|ornament-border|<value|fold-bar-border>|ornament-border-color|<value|fold-bar-border-color>|<arg|body>>>>

  <assign|generic-session|<\macro|name|body>
    <\padded>
      <\with|session-par-sep|<value|par-sep>|session-par-ver-sep|<value|par-ver-sep>|par-sep|<value|session-sep>|par-ver-sep|<value|session-sep>|par-first|0fn|par-par-sep|0fn|par-line-sep|0fn>
        <arg|body>
      </with>
    </padded>
  </macro>>

  <assign|subsession|<\macro|body>
    <\with|old-shape|<value|ornament-shape>|old-color|<value|ornament-color>|old-hpadding|<value|ornament-hpadding>|old-vpadding|<value|ornament-vpadding>|old-border|<value|ornament-border>|old-sunny-color|<value|ornament-sunny-color>>
      <\with-fold-bar-deco>
        <\with|ornament-shape|band|ornament-hpadding|<over|<value|session-band-width>|2>|ornament-vpadding|0spc>
          <\ornament>
            <\surround||<right-flush>>
              <\with|ornament-shape|<value|old-shape>|ornament-color|<value|old-color>|ornament-border|<value|old-border>|ornament-hpadding|<value|old-hpadding>|ornament-sunny-color|<value|old-sunny-color>|ornament-vpadding|<value|old-vpadding>>
                <arg|body>
              </with>
            </surround>
          </ornament>
        </with>
      </with-fold-bar-deco>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Fields for input, output, text and errors.
    </src-comment>
  </active*>

  <assign|with-input-deco|<macro|body|<with|ornament-shape|classic|ornament-color|<value|input-color>|ornament-vpadding|<value|input-vpadding>|ornament-border|<value|input-border>|ornament-sunny-color|<value|input-border-color>|<arg|body>>>>

  <assign|generic-input|<\macro|prompt|body>
    <\with-input-deco>
      <\with|ornament-hpadding|<tuple|<value|session-left-indent>|<value|session-right-indent>>>
        <\ornament>
          <\ornament-render-body>
            <surround||<right-flush>|<tabbed*|<tformat|<table|<row|<cell|<id-function|<with|color|<value|generic-prompt-color>|<arg|prompt>>>>|<\cell>
              <\with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
                <arg|body>
              </with>
            </cell>>>>>>
          </ornament-render-body>
        </ornament>
      </with>
    </with-input-deco>
  </macro>>

  <assign|generic-output*|<macro|body|<with|par-mode|justify|par-flexibility|2.0|par-hyphen|normal|math-display|true|math-frac-limit|<value|session-frac-limit>|math-table-limit|<value|session-table-limit>|<ornament-indent|<value|session-left-indent>|<value|session-right-indent>|<value|output-vpadding>|<value|output-vpadding>|<with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>|<arg|body>>>>>>

  <assign|generic-output|<macro|body|<generic-output*|<arg|body>>>>

  <assign|generic-textput|<macro|body|<ornament-indent|<value|session-left-indent>|<value|session-right-indent>|<value|output-vpadding>|<value|output-vpadding>|<with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>|<arg|body>>>>>

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

  <assign|with-fold-title-deco|<macro|body|<with|ornament-shape|classic|ornament-color|<value|fold-title-color>|ornament-vpadding|<value|input-vpadding>|ornament-border|<value|input-border>|ornament-sunny-color|<value|input-border-color>|<arg|body>>>>

  <assign|folded-subsession|<\macro|x|y>
    <\with-fold-title-deco>
      <\with|ornament-hpadding|<tuple|0fn|<value|session-right-indent>>>
        <\ornament>
          <\ornament-render-body>
            <\surround|<action|<resize|<space|0.4spc><math|\<Downarrow\>>|||<value|session-band-width>|>|mouse-unfold|<arg|x>><space|<value|session-left-indent>>|<right-flush>>
              <\with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
                <arg|x>
              </with>
            </surround>
          </ornament-render-body>
        </ornament>
      </with>
    </with-fold-title-deco>
  </macro>>

  <assign|unfolded-subsession|<\macro|x|y>
    <\with-fold-title-deco>
      <\with|ornament-shape|classic|ornament-hpadding|<tuple|0fn|<value|session-right-indent>>>
        <\ornament>
          <\ornament-render-body>
            <\surround|<action|<resize|<space|0.4spc><math|\<Uparrow\>>|||<value|session-band-width>|>|mouse-fold|<arg|x>><space|<value|session-left-indent>>|<right-flush>>
              <\with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
                <arg|x>
              </with>
            </surround>
          </ornament-render-body>
        </ornament>

        <\subsession>
          <arg|y>
        </subsession>
      </with>
    </with-fold-title-deco>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>