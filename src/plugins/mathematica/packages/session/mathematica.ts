<TeXmacs|1.0.5.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|mathematica|1.0>

    <\src-purpose>
      Markup for Mathematica sessions.
    </src-purpose>

    <src-copyright|2005|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|mathematica-output|<macro|body|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<generic-output*|<arg|body>>>>>

  <assign|Mvariable|<macro|name|<arg|name>>>

  <assign|Mfunction|<macro|name|<arg|name>>>

  <assign|Muserfunction|<macro|name|<arg|name>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>