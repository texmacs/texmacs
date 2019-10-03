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
      Standard drop shadows
    </src-comment>
  </active*>

  <copy-theme|drop-shadow|shadow>

  <assign|drop-shadow-shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-drop-shadow.png>

  <assign|drop-shadow-shadow-picture|tmfs://artwork/pictures/shadows/drop-shadow.png>

  <assign|drop-shadow-shadow-align|south east>

  <assign|drop-shadow|<macro|body|<with-drop-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Bend-in shadows
    </src-comment>
  </active*>

  <copy-theme|bend-in-shadow|shadow>

  <assign|bend-in-shadow-shadow-picture|$TEXMACS_PATH/misc/pictures/shadows/thumbnail-bend-in-shadow.png>

  <assign|bend-in-shadow-shadow-picture|tmfs://artwork/pictures/shadows/bend-in-shadow.png>

  <assign|bend-in-shadow-shadow-align|south east>

  <assign|bend-in-shadow-shadow-format|..x ..x xxx>

  <assign|bend-in-shadow-shadow-lcrop|0.05>

  <assign|bend-in-shadow-shadow-rcrop|0.95>

  <assign|bend-in-shadow-shadow-bcrop|0.05>

  <assign|bend-in-shadow-shadow-tcrop|0.95>

  <assign|bend-in-shadow|<macro|body|<with-bend-in-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Diagonal bend-in shadows
    </src-comment>
  </active*>

  <copy-theme|diagonal-bend-in-shadow|shadow>

  <assign|diagonal-bend-in-shadow-shadow-picture|tmfs://artwork/pictures/shadows/bend-diag-in-shadow.png>

  <assign|diagonal-bend-in-shadow-shadow-lcrop|0.04>

  <assign|diagonal-bend-in-shadow-shadow-rcrop|0.96>

  <assign|diagonal-bend-in-shadow-shadow-bcrop|0.04>

  <assign|diagonal-bend-in-shadow-shadow-tcrop|0.96>

  <assign|diagonal-bend-in-shadow-shadow-lborder|0.4em>

  <assign|diagonal-bend-in-shadow-shadow-tborder|0.4em>

  <assign|diagonal-bend-in-shadow|<macro|body|<with-diagonal-bend-in-shadow|<art-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Classic shadows
    </src-comment>
  </active*>

  <copy-theme|classic-shadow|shadow>

  <assign|classic-shadow-shadow-picture|tmfs://artwork/pictures/shadows/drop-shadow.png>

  <assign|classic-shadow|<macro|body|<with-classic-shadow|<deco-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Rounded shadows
    </src-comment>
  </active*>

  <copy-theme|rounded-shadow|shadow>

  <assign|rounded-shadow-shadow-picture|tmfs://artwork/pictures/shadows/round-shadow.png>

  <assign|rounded-shadow|<macro|body|<with-rounded-shadow|<deco-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Angular shadows
    </src-comment>
  </active*>

  <copy-theme|angular-shadow|shadow>

  <assign|angular-shadow-shadow-picture|tmfs://artwork/pictures/shadows/angular-shadow.png>

  <assign|angular-shadow|<macro|body|<with-angular-shadow|<deco-shadow|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Angular shadows
    </src-comment>
  </active*>

  <copy-theme|cartoon-shadow|shadow>

  <assign|cartoon-shadow-shadow-picture|tmfs://artwork/pictures/shadows/cartoon-shadow.png>

  <assign|cartoon-shadow|<macro|body|<with-cartoon-shadow|<deco-shadow|<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>