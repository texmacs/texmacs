<TeXmacs|1.0.6.14>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|mathemagix|1.0>

    <\src-purpose>
      Markup for Mathemagix sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|mmx-prompt|<macro|nr|<with|color|red|<arg|nr>] >>>

  <assign|mathemagix-output|<macro|body|<surround|<vspace*|0.5fn>|<vspace|0fn>|<generic-output|<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>