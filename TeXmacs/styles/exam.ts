<TeXmacs|1.0.4>

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
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you do not have a copy of the license, then write to
        the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
        Boston, MA 02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env|header-exam|section-generic>

  \;

  <assign|render-exercise|<\macro|which|body>
    <\padded-normal|1fn|1fn>
      <surround|<exercise-name|<arg|which><exercise-sep>>||<arg|body>>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>