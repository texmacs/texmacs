<TeXmacs|1.99.2>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|database-bib|1.0>

      <\src-purpose>
        Style for editing bibliographic databases.
      </src-purpose>

      <\src-copyright|2015>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|database>

  <\active*>
    <\src-comment>
      Markup for bibliography fields.
    </src-comment>
  </active*>

  <assign|name-sep|<macro|<with|color|dark green|font-series|bold|
  <localize|and> >>>

  <assign|name-von|<macro|von|<with|font-shape|small-caps|color|dark
  grey|<arg|von>>>>

  <assign|name-jr|<macro|jr|<with|font-shape|small-caps|color|dark
  grey|<arg|jr>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>