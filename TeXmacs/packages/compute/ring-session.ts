<TeXmacs|1.0.7.21>

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

  <assign|session-ring-width|1fn>

  <assign|session-color|#fdfdfa>

  <assign|session-border|1ln,0ln,1ln,0ln>

  <assign|session-left-indent|0.5fn>

  <assign|session-right-indent|0.5fn>

  <assign|session-sep|0.1fn>

  \;

  <assign|input-color|pastel yellow>

  <assign|input-border|0ln,1ln,0ln,1ln>

  <assign|input-border-color|<tuple|dark grey>>

  <assign|output-vpadding|0.5fn>

  \;

  <assign|error-color|pastel red>

  <assign|error-border|1ln>

  <assign|error-border-color|<tuple|dark red>>

  <assign|generic-error-color|dark red>

  \;

  <assign|fold-bar-color|pastel brown>

  <assign|fold-title-color|pastel orange>

  <\active*>
    <\src-comment>
      Entire sessions
    </src-comment>
  </active*>

  <assign|generic-session|<\macro|name|body>
    <\padded>
      <\with|session-par-sep|<value|par-sep>|session-par-ver-sep|<value|par-ver-sep>|par-sep|<value|session-sep>|par-ver-sep|<value|session-sep>|par-first|0fn|par-par-sep|0fn|par-line-sep|0fn|old-shape|<value|ornament-shape>|old-color|<value|ornament-color>|old-hpadding|<value|ornament-hpadding>|old-vpadding|<value|ornament-vpadding>|old-border|<value|ornament-border>|ornament-shape|ring|ornament-color|<value|session-ring>|ornament-hpadding|<over|<value|session-ring-width>|2>|ornament-vpadding|0spc>
        <\ornament>
          <with|ornament-shape|classic|ornament-color|<value|session-color>|ornament-border|<value|session-border>|ornament-hpadding|0spc|<\ornament>
            <\surround||<right-flush>>
              <\with|ornament-shape|<value|old-shape>|ornament-color|<value|old-color>|ornament-border|<value|old-border>|ornament-hpadding|<value|old-hpadding>|ornament-vpadding|<value|old-vpadding>>
                <arg|body>
              </with>
            </surround>
          </ornament>>
        </ornament>
      </with>
    </padded>
  </macro>>

  <\active*>
    <\src-comment>
      Utility macro
    </src-comment>
  </active*>

  <assign|ornament-indent|<\macro|left|right|bottom|top|body>
    <\with|old-shape|<value|ornament-shape>|old-color|<value|ornament-color>|old-hpadding|<value|ornament-hpadding>|old-vpadding|<value|ornament-vpadding>|old-border|<value|ornament-border>|ornament-shape|classic|ornament-color|#fff0|ornament-hpadding|<tuple|<arg|left>|<arg|right>>|ornament-vpadding|<tuple|<arg|bottom>|<arg|top>>|ornament-border|0ln>
      <\ornament>
        <\surround||<right-flush>>
          <\with|ornament-shape|<value|old-shape>|ornament-color|<value|old-color>|ornament-hpadding|<value|old-hpadding>|ornament-vpadding|<value|old-vpadding>|ornament-border|<value|old-border>>
            <arg|body>
          </with>
        </surround>
      </ornament>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Fields for input, output, text and errors.
    </src-comment>
  </active*>

  <assign|generic-input|<\macro|prompt|body>
    <\with|ornament-shape|classic|ornament-color|<value|input-color>|ornament-hpadding|<tuple|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>>|ornament-border|<value|input-border>|ornament-sunny-color|<value|input-border-color>>
      <\ornament>
        <\surround|<id-function|<with|color|<value|generic-prompt-color>|<arg|prompt>>>|<right-flush>>
          <\with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>>
            <arg|body>
          </with>
        </surround>
      </ornament>
    </with>
  </macro>>

  <assign|generic-output*|<macro|body|<with|par-mode|left|math-display|true|<ornament-indent|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>|<value|output-vpadding>|<value|output-vpadding>|<with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>|<arg|body>>>>>>

  <assign|generic-output|<macro|body|<generic-output*|<arg|body>>>>

  <assign|generic-textput|<macro|body|<ornament-indent|<plus|<value|session-ring-width>|<value|session-left-indent>>|<value|session-right-indent>|<value|output-vpadding>|<value|output-vpadding>|<with|par-sep|<value|session-par-sep>|par-ver-sep|<value|session-par-ver-sep>|<arg|body>>>>>

  <assign|generic-errput|<\macro|body>
    <\with|ornament-shape|classic|ornament-color|<value|error-color>|ornament-border|<value|error-border>|ornament-sunny-color|<value|error-border-color>>
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

  <assign|folded|<macro|x|y|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|1|2|2|cell-hpart|1>|<cwith|1|1|2|2|cell-background|<value|fold-title-color>>|<cwith|1|1|1|1|cell-background|<value|fold-bar-color>>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-bsep|0.25fn>|<cwith|1|1|2|2|cell-tsep|0.25fn>|<cwith|1|1|2|2|cell-lsep|0.5fn>|<cwith|1|1|2|2|cell-rsep|0.5fn>|<table|<row|<cell|<action||(mouse-unfold)|<arg|x>>>|<\cell>
    <arg|x>
  </cell>>>>>>>>

  <assign|unfolded|<macro|x|y|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|-1|2|2|cell-hpart|1>|<cwith|1|1|2|2|cell-lborder|0.5ln>|<cwith|1|1|2|2|cell-rborder|0.5ln>|<cwith|1|1|2|2|cell-bborder|0.5ln>|<cwith|1|1|2|2|cell-tborder|0.5ln>|<cwith|1|-1|1|1|cell-lborder|0.5ln>|<cwith|1|-1|1|1|cell-rborder|0.5ln>|<cwith|1|-1|1|1|cell-bborder|0.5ln>|<cwith|1|-1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0ln>|<cwith|2|2|1|1|cell-tborder|0ln>|<cwith|2|2|2|2|cell-lsep|0fn>|<cwith|2|2|2|2|cell-rsep|0fn>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-hyphen|t>|<cwith|1|1|2|2|cell-bsep|0.25fn>|<cwith|1|1|2|2|cell-tsep|0.25fn>|<cwith|2|2|2|2|cell-tsep|0.5fn>|<cwith|1|-1|1|1|cell-background|<value|fold-bar-color>>|<cwith|1|1|2|2|cell-background|<value|fold-title-color>>|<cwith|2|2|2|2|cell-bsep|0fn>|<cwith|1|1|2|2|cell-lsep|0.5fn>|<cwith|1|1|2|2|cell-rsep|0.5fn>|<table|<row|<cell|<action||(mouse-fold)|<arg|x>>>|<\cell>
    <arg|x>
  </cell>>|<row|<cell|<action||(mouse-fold)|<arg|x>>>|<\cell>
    <arg|y>
  </cell>>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>