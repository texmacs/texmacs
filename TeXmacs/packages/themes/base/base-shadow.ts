<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|base-shadow|1.0|base-shadow|1.0>

    <\src-purpose>
      Common base for shadows.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Defaults
    </src-comment>
  </active*>

  <assign|shadow-picture|tmfs://artwork/pictures/frames/carved-wood-frame.jpg>

  <assign|shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-drop-shadow.png>

  <assign|shadow-effect|0>

  <assign|shadow-align|outer>

  <assign|shadow-center|true>

  <assign|shadow-lcrop|0.03>

  <assign|shadow-rcrop|0.97>

  <assign|shadow-bcrop|0.03>

  <assign|shadow-tcrop|0.97>

  <assign|shadow-lborder|0.5em>

  <assign|shadow-rborder|0.5em>

  <assign|shadow-bborder|0.5em>

  <assign|shadow-tborder|0.5em>

  <\active*>
    <\src-comment>
      Defaults
    </src-comment>
  </active*>

  <assign|art-shadow|<macro|body|<art-box|<arg|body>|<tuple|frame|<value|shadow-picture>|effect|<value|shadow-effect>|align|<value|shadow-align>|center|<value|shadow-center>|lcrop|<value|shadow-lcrop>|rcrop|<value|shadow-rcrop>|bcrop|<value|shadow-bcrop>|tcrop|<value|shadow-tcrop>|lwidth|<value|shadow-lborder>|rwidth|<value|shadow-rborder>|bheight|<value|shadow-bborder>|theight|<value|shadow-tborder>>|<tuple|text|normal>>>>

  <\active*>
    <\src-comment>
      Frame theme parameters
    </src-comment>
  </active*>

  <new-theme|shadow|shadow-picture|shadow-effect|shadow-align|shadow-center|shadow-lcrop|shadow-rcrop|shadow-bcrop|shadow-tcrop|shadow-lborder|shadow-rborder|shadow-bborder|shadow-tborder>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>