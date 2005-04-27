<TeXmacs|1.0.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|generic|1.0>

      <\src-purpose>
        The generic style.
      </src-purpose>

      <\src-copyright|1998--2004>
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

  <use-package|std|env|title-generic|header-generic|section-generic>

  \;

  <assign|par-first|0fn>

  <assign|par-par-sep|0.6666fn>

  \;

  <assign|code|<\macro|body>
    <\padded-normal|0.5fn|1fn>
      <with|font-family|tt|language|verbatim|par-par-sep|0fn|<arg|body>>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>