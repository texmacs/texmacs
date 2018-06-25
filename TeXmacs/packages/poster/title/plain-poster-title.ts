<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|plain-poster-title|1.0|poster-title|1.0>

    <\src-purpose>
      Plain frameless poster title.
    </src-purpose>

    <src-copyright|2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|poster-title|<\macro|body|ldeco|rdeco>
    <\with|par-columns|1|color|<value|title-body-color>>
      <\surround|<resize|<arg|ldeco>|||1l|><htab|5mm>|<htab|5mm><resize|<arg|rdeco>|1r|||>>
        <arg|body>
      </surround>
    </with>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>