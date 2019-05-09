<TeXmacs|1.99.9>

<style|<tuple|source|std|english>>

<\body>
  <active*|<\src-title>
    <src-package|tmdoc-web2|1.0>

    <\src-purpose>
      New design for <TeXmacs> web site.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  \;

  <assign|html-site-version|2>

  <assign|html-css|../css/tmweb2.css>

  <\active*>
    <\src-comment>
      Hyperlink menus
    </src-comment>
  </active*>

  <assign|tmweb-link-item|<macro|body|<arg|body>>>

  <assign|tmweb-link-menu|<xmacro|args|<html-tag|nav|<map-args|tmweb-link-item|concat|args>>>>

  <assign|tmweb-link-section|<xmacro|args|<html-tag|section|<html-tag|h1|<arg|args|0>><html-tag|nav|<map-args|tmweb-link-item|concat|args|1>>>>>

  <\active*>
    <\src-comment>
      Hard-coded hyperlinks. Argh, but well...
    </src-comment>
  </active*>

  <assign|tmweb-top-links|<macro|<style-with|src-compact|none|<tmweb-link-menu|<hlink|About|../home/welcome.en.tm>|<hlink|Download|../download/download.en.tm>|<hlink|Learn|../help/learn.en.tm>|<hlink|Contribute|../contribute/contribute.en.tm>>>>>

  <assign|tmweb-home-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Welcome|welcome>|<tmweb-link|Videos|videos>|<tmweb-link|Screen
  shots|screenshots>|<tmweb-link|News|news>|<tmweb-link|Mailing
  lists|ml>|<tmweb-link|Jobs|jobs>>>>>

  <assign|tmweb-download-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|General|download>|<tmweb-link|Sources|sources>|<tmweb-link|Linux|unix>|<tmweb-link|MacOS|macosx>|<tmweb-link|Windows|windows>|<tmweb-link|Knoppix|knoppix>>>>>

  <assign|tmweb-help-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|General|help>|<tmweb-link|FAQ|faq>|<tmweb-link|Tutorials|tutorial>|<tmweb-link|Manual|manual>|<tmweb-link|Articles|articles>|<tmweb-link|Videos|../home/videos>>>>>

  <assign|tmweb-contribute-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Contributing|contribute>|<tmweb-link|Team|team>|<tmweb-link|Documentation|documentation>|<tmweb-link|Translations|translations>|<tmweb-link|Extensions|plugins>|<tmweb-link|Donations|donations>>>>>

  <assign|tmweb-plugin-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|All|plugins>|<tmweb-link|Mathematics|cas>|<tmweb-link|Numerics|numerics>|<tmweb-link|Statistics|statistics>|<tmweb-link|Physics|physics>|<tmweb-link|Graphics|graphics>|<tmweb-link|Tools|tools>>>>>

  <assign|tmweb-about-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Authors|authors>|<tmweb-link|Donators|donators>|<tmweb-link|Philosophy|philosophy>|<tmweb-link|Changes|changes>|<tmweb-link|To
  do|todo>|<tmweb-link|Plans|plans>|<tmweb-link|Roadmap|roadmap>|<tmweb-link|Artwork|artwork>|<tmweb-link|License|license>>>>>

  <assign|tmweb-contact-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Feedback|contact>|<tmweb-link|Bugs|bugs>|<tmweb-link|Suggestions|wishes>|<tmweb-link|Patches|patches>>>>>

  <\active*>
    <\src-comment>
      Browsing the manual and the tutorial.
    </src-comment>
  </active*>

  <assign|tmweb-manual-links|<macro|previous|next|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Manual|../help/manual>|<tmweb-link|Top|web-manual>|<tmweb-link|Previous|<arg|previous>>|<tmweb-link|Next|<arg|next>>>>>>

  <assign|tmweb-tutorial-links|<macro|previous|next|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Tutorial|../help/tutorial>|<tmweb-link|Top|web-tutorial>|<tmweb-link|Previous|<arg|previous>>|<tmweb-link|Next|<arg|next>>>>>>

  <\active*>
    <\src-comment>
      Main navigation bar and the license for the <TeXmacs> web pages.
    </src-comment>
  </active*>

  <assign|tmweb-main-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Home|../home/welcome>|<specific|texmacs|<tmweb-link|Download|../download/download>><specific|html|<merge|\<less\>script
  language="javascript"\<gtr\>document.write (downloadButton
  ("|<translate|Download|english|<language>>|",
  "|<if|<or|<equal|Download|<tmweb-main>>|<equal|Download|<tmweb-sub>>>|#008000|brown>|"));\<less\>/script\<gtr\>>>|<tmweb-link|Help|../help/help>|<tmweb-link|Contribute|../contribute/contribute>|<tmweb-link|Plug-ins|../plugins/plugins>|<tmweb-link|About|../about/authors>|<tmweb-link|Contact|../contact/contact>|<hlink|<with|color|brown|<localize|Search>>|http://www.texmacs.org/search>>>>>

  <assign|tmweb-title|<macro|title|bar|<tmdoc-title**|<tmweb-main-links>|<arg|title>|<arg|bar>>>>

  <assign|tmweb-tmimage|<macro|<image|../images/TeXmacs.png|80px|80px||-0.25h>>>

  <assign|tmweb-title|<macro|title|bar|<html-div-class|tmweb-pad-below|<html-div-class|tmweb-header|<html-class|tmweb-title-image|<tmweb-tmimage>><space|1em><html-class|tmweb-title|<arg|title>><space|1em><html-class|tmweb-top-menu|<tmweb-top-links>>>>>>

  <assign|tmweb-license|<\macro>
    <\tmdoc-license>
      <active*|<\with|color|dark grey|font-size|0.84>
        This webpage is part of <hlink|GNU <TeXmacs>|http://www.texmacs.org>
        and the larger <hlink|GNU project|http://www.gnu.org>. Verbatim
        copying and distribution of it is permitted in any medium, provided
        this notice is preserved. For more information or questions, please
        contact <hlink|Joris van der Hoeven|http://www.texmacs.org/Web/Mail.html>.

        <hlink|Free Software Foundation|http://www.fsf.org/fsf/fsf.html>,
        Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111, USA
      </with>>
    </tmdoc-license>
  </macro>>

  <assign|tmweb-license|<\macro>
    <\html-div-class|tmweb-pad-above>
      <\html-div-class|tmweb-footer>
        <\small>
          This webpage is part of <hlink|GNU
          <TeXmacs>|http://www.texmacs.org> and the larger <hlink|GNU
          project|http://www.gnu.org>. Verbatim copying and distribution of
          it is permitted in any medium, provided this notice is preserved.
          For more information or questions, please contact <hlink|Joris van
          der Hoeven|http://www.texmacs.org/Web/Mail.html>.

          <hlink|Free Software Foundation|http://www.fsf.org/fsf/fsf.html>,
          Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111, USA
        </small>
      </html-div-class>
    </html-div-class>
  </macro>>

  <\active*>
    <\src-comment>
      Miscellaneous macros.
    </src-comment>
  </active*>

  <assign|tmweb-image|<macro|name|<image|<merge|http://www.texmacs.org/Samples/|<arg|name>>|0.666667w|||>>>

  <assign|tmweb-email|<macro|name|domain|<active*|<with|font-family|tt|color|dark
  magenta|\<less\>>><with|font-family|tt|color|dark
  blue|<arg|name>><active*|<with|font-family|tt|color|dark
  magenta|@>><with|font-family|tt|color|dark
  blue|<arg|domain>><active*|<with|font-family|tt|color|dark
  magenta|\<gtr\>>>>>

  \;

  <assign|texmacs-stable-targz|<macro|1.0.2>>

  <assign|texmacs-stable-rpm|<macro|1.0.2-1>>

  <assign|texmacs-version|<macro|1.0.2.1>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>