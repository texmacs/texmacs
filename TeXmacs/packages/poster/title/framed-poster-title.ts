<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|framed-poster-title|1.0|poster-title|1.0>

    <\src-purpose>
      Default poster title in a frame.
    </src-purpose>

    <src-copyright|2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|title-swell|0em>

  <assign|title-border|1ln>

  <assign|title-hpadding|2spc>

  <assign|title-vpadding|2spc>

  \;

  <assign|poster-title|<\macro|body|l|r>
    <\with|par-columns|1|ornament-color|<value|title-body-bg-color>|color|<value|title-body-color>|ornament-shape|<value|title-shape>|par-left|<minus|<value|par-left>|<value|title-swell>>|par-right|<minus|<value|par-right>|<value|title-swell>>|ornament-border|<value|title-border>|ornament-hpadding|<value|title-hpadding>|ornament-vpadding|<value|title-vpadding>>
      <\ornament>
        <\surround|<arg|l><htab|5mm>|<htab|5mm><arg|r>>
          <\title-render-title>
            <arg|body>
          </title-render-title>
        </surround>
      </ornament>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>