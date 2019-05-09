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

  <assign|html-css|../css/tmweb2.css>

  <\active*>
    <\src-comment>
      Bars with hyperlinks.
    </src-comment>
  </active*>

  <assign|tmweb-current|<macro|main|sub|<assign|tmweb-main|<arg|main>><assign|tmweb-sub|<arg|sub>>>>

  <assign|tmweb-suffix|<macro|<style-with|src-compact|none|<case|<equal|<language>|german>|.de.tm|<equal|<language>|french>|.fr.tm|<equal|<language>|italian>|.it.tm|<equal|<language>|portuguese>|.pt.tm|<equal|<language>|spanish>|.es.tm|.en.tm>>>>

  <assign|tmweb-link|<macro|what|to|<style-with|src-compact|none|<hlink|<style-with|src-compact|none|<expand-as|<arg|what>|<style-with|src-compact|none|<with|color|<if|<or|<equal|<arg|what>|<tmweb-main>>|<equal|<arg|what>|<tmweb-sub>>>|dark
  green|brown>|<translate|<arg|what>|english|<language>>>>>>|<style-with|src-compact|none|<if|<equal|<find-file|<merge|<arg|to>|<tmweb-suffix>>>|false>|<merge|<arg|to>|.en.tm>|<merge|<arg|to>|<tmweb-suffix>>>>>>>>

  <assign|tmweb-list-extra|<macro|x| \| <arg|x>>>

  <assign|tmweb-list|<xmacro|x| <arg|x|0><map-args|tmweb-list-extra|concat|x|1>
  >>

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

  <assign|tmweb-tmimage|<macro|<image|../images/TeXmacs.png|80px|80px||>>>

  <assign|tmweb-title|<macro|title|bar|<html-div|tmweb-pad-below|<html-div|tmweb-header|<tabular|<tformat|<cwith|1|1|1|-1|cell-valign|c>|<table|<row|<cell|<tmweb-tmimage>>|<cell|<space|1em>>|<cell|<html-div|tmweb-title|<arg|title>>>|<cell|<space|1em>>|<cell|<arg|bar>>>>>>>>>>

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
    <\html-div|tmweb-pad-above>
      <\html-div|tmweb-footer>
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
      </html-div>
    </html-div>
  </macro>>

  <\active*>
    <\src-comment>
      Hard-coded hyperlinks. Argh, but well...
    </src-comment>
  </active*>

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