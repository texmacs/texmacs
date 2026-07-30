<TeXmacs|2.1.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|home-page|1.0>

    <\src-purpose>
      Home page for TeXmacs cloud
    </src-purpose>

    <src-copyright|2026|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|std-shadow|tmdoc-gui>

  <use-module|(client client-markup)>

  <assign|locus-color|dark blue>

  <assign|visited-color|dark blue>

  <\active*>
    <\src-comment>
      Icons
    </src-comment>
  </active*>

  <assign|remote-big-icon|<macro|name|<image|<find-file|$TEXMACS_PATH/misc/pixmaps/light|$TEXMACS_PATH/misc/pixmaps/modern/24x24/main|$TEXMACS_PATH/misc/pixmaps/modern/16x16/focus|<arg|name>>|0.2par|||>>>

  <\active*>
    <\src-comment>
      User information
    </src-comment>
  </active*>

  <assign|remote-picture|<macro|<extern|ext-remote-picture>>>

  <assign|remote-name|<macro|<extern|ext-remote-name>>>

  <assign|remote-email|<macro|<extern|ext-remote-email>>>

  <\active*>
    <\src-comment>
      Welcome message
    </src-comment>
  </active*>

  <assign|remote-welcome|<\macro>
    <\greyed>
      <strong|Welcome to the <TeXmacs> cloud!>

      <\em>
        This is your personal home page. You may edit it freely and save it
        as an ordinary <TeXmacs> document. You may always restore the
        standard welcome page using <with|font-shape|right|<menu|Remote|Restore
        home page>>.

        Quick links to frequently used documents are available below, but you
        may insert hyperlinks to more of your favorite documents. For further
        information about the <TeXmacs> cloud, <hlink|click
        here|tmfs://help/article/tm/doc/main/remote/man-collaborative.en.tm>.
      </em>
    </greyed>
  </macro>>

  <\active*>
    <\src-comment>
      Menus
    </src-comment>
  </active*>

  <assign|remote-menu-table|<macro|body|<tformat|<cwith|2|2|1|1|cell-halign|c>|<cwith|2|2|1|1|cell-background|dark
  grey>|<cwith|2|2|1|1|color|white>|<cwith|2|2|1|1|cell-halign|l>|<cwith|1|-1|1|-1|font-family|ss>|<arg|body>>>>

  <assign|remote-menu|<macro|name|icon-name|title|<half-bend-in-down|<extern|ext-remote-menu|<quote-arg|name>|<quote-arg|icon-name>|<quote-arg|title>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>