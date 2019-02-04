<TeXmacs|1.99.8>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|exam|1.0>

      <\src-purpose>
        The exam style.
      </src-purpose>

      <\src-copyright|2002--2004>
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

  <use-package|std|env|title-generic|header-exam|section-generic|padded-paragraphs|std-edu>

  <\active*>
    <\src-comment>
      Override rendering of exercise and solution environments
    </src-comment>
  </active*>

  <assign|short-item|<macro|name|<style-with|src-compact|none|<with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<arg|name>|<minus|1r|<minus|<item-hsep>|0.5fn>>||<plus|1r|0.5fn>|>>>>

  <assign|exercise-short-sep|<macro|)>>

  <assign|exercise|<\macro|body>
    <\padded*>
      <\with|par-left|<plus|<value|par-left>|<item-hsep>>|enumerate-level|1|solution-inner|true>
        <\surround|<next-exercise><short-item|<the-exercise><exercise-short-sep>>|>
          <arg|body>
        </surround>
      </with>
    </padded*>
  </macro>>

  <assign|solution-indent|1tab>

  <assign|solution-inner|false>

  <assign|solution|<\macro|body>
    <\padded*>
      <\with|par-left|<plus|<value|par-left>|<if|<value|solution-inner>|0tab|<item-hsep>>|<value|solution-indent>>|enumerate-level|1>
        <\surround|<short-item|\<triangleright\>>|<right-flush>>
          <arg|body>
        </surround>
      </with>
    </padded*>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>