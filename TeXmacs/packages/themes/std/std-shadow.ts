<TeXmacs|1.99.11>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|std-shadow|1.0|std-shadow|1.0>

    <\src-purpose>
      Standard artistic shadows
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|base-shadow>

  <\active*>
    <\src-comment>
      South east drop shadows
    </src-comment>
  </active*>

  <copy-theme|south-east|shadow>

  <assign|south-east-shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-drop-shadow.png>

  <assign|south-east-shadow-picture|tmfs://artwork/pictures/shadows/drop-shadow.png>

  <assign|south-east-shadow-align|south east>

  <assign|south-east-shadow|<macro|body|<with-south-east|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      South east rounded shadows
    </src-comment>
  </active*>

  <copy-theme|south-east-round|shadow>

  <assign|south-east-round-shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-round-shadow.png>

  <assign|south-east-round-shadow-picture|tmfs://artwork/pictures/shadows/round-shadow.png>

  <assign|south-east-round-shadow-align|south east>

  <assign|south-east-round-shadow-lcrop|0.05>

  <assign|south-east-round-shadow-rcrop|0.95>

  <assign|south-east-round-shadow-bcrop|0.05>

  <assign|south-east-round-shadow-tcrop|0.95>

  <assign|south-east-round-shadow-lborder|0.75em>

  <assign|south-east-round-shadow-rborder|0.75em>

  <assign|south-east-round-shadow-bborder|0.75em>

  <assign|south-east-round-shadow-tborder|0.75em>

  <assign|south-east-round-shadow|<macro|body|<with-south-east-round|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      South east bend shadows
    </src-comment>
  </active*>

  <copy-theme|south-east-in|shadow>

  <assign|south-east-in-shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-bend-in-shadow.png>

  <assign|south-east-in-shadow-picture|tmfs://artwork/pictures/shadows/bend-in-shadow.png>

  <assign|south-east-in-shadow-align|south east>

  <assign|south-east-in-shadow-format|..x ..x xxx>

  <assign|south-east-in-shadow-lcrop|0.05>

  <assign|south-east-in-shadow-rcrop|0.95>

  <assign|south-east-in-shadow-bcrop|0.05>

  <assign|south-east-in-shadow-tcrop|0.95>

  <assign|south-east-in-shadow|<macro|body|<with-south-east-in|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Diagonal bend in shadows
    </src-comment>
  </active*>

  <copy-theme|diag-in|shadow>

  <assign|diag-in-shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-bend-diag-in-shadow.png>

  <assign|diag-in-shadow-picture|tmfs://artwork/pictures/shadows/bend-diag-in-shadow.png>

  <assign|diag-in-shadow-lcrop|0.04>

  <assign|diag-in-shadow-rcrop|0.96>

  <assign|diag-in-shadow-bcrop|0.04>

  <assign|diag-in-shadow-tcrop|0.96>

  <assign|diag-in-shadow-lborder|0.4em>

  <assign|diag-in-shadow-tborder|0.4em>

  <assign|diag-in-shadow|<macro|body|<with-diag-in|<art-shadow|<arg|body>>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>