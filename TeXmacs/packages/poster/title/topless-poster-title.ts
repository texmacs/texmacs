<TeXmacs|1.99.13>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|topless-poster-title|1.0|poster-title|1.0>

    <\src-purpose>
      Suppress top, left and right margins for a framed poster title.
    </src-purpose>

    <src-copyright|2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|page-top|0cm>

  <assign|title-vpadding|2em>

  \;

  <assign|poster-title|<\macro|body|ldeco|rdeco>
    <\no-margins>
      <\with|par-columns|1|deco-hook|<macro|body|<with|ornament-border|0em|ornament-hpadding|<value|title-hpadding>|ornament-vpadding|<value|title-vpadding>|<arg|body>>>>
        <\surround||<vspace|<value|title-vsep>>>
          <\title-block>
            <\surround|<resize|<arg|ldeco>|||1l|><htab|5mm>|<htab|5mm><resize|<arg|rdeco>|1r|||>>
              <arg|body>
            </surround>
          </title-block>
        </surround>
      </with>
    </no-margins>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>