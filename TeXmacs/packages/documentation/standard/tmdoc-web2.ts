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
      Miscellaneous helper macros.
    </src-comment>
  </active*>

  <assign|obsolete|<macro|body|<html-class|tmweb-obsolete|<arg|body>>>>

  <assign|tmweb-image|<macro|name|<image|<merge|http://www.texmacs.org/Samples/|<arg|name>>|0.666667w|||>>>

  <assign|tmweb-email|<macro|name|domain|<active*|<with|font-family|tt|color|dark
  magenta|\<less\>>><with|font-family|tt|color|dark
  blue|<arg|name>><active*|<with|font-family|tt|color|dark
  magenta|@>><with|font-family|tt|color|dark
  blue|<arg|domain>><active*|<with|font-family|tt|color|dark
  magenta|\<gtr\>>>>>

  <\active*>
    <\src-comment>
      Hyperlink menus
    </src-comment>
  </active*>

  <assign|tmweb-link-item|<macro|body|<arg|body>>>

  <assign|tmweb-link-menu|<xmacro|args|<html-tag|nav|<map-args|tmweb-link-item|concat|args>>>>

  <assign|tmweb-link-section|<xmacro|args|<html-tag|section|<section*|<arg|args|0>><html-tag|nav|<map-args|tmweb-link-item|concat|args|1>>>>>

  <\active*>
    <\src-comment>
      Hard-coded hyperlinks. Argh, but well...
    </src-comment>
  </active*>

  <assign|tmweb-top-links|<macro|<style-with|src-compact|none|<tmweb-link-menu|<hlink|About|../home/welcome.en.tm>|<hlink|Download|../download/download.en.tm>|<hlink|Learn|../help/learn.en.tm>|<hlink|Contribute|../contribute/contribute.en.tm>>>>>

  <assign|tmweb-home-links|<macro|<style-with|src-compact|none|<tmweb-link-section|TeXmacs|<hlink|Welcome|../home/welcome.en.tm>|<obsolete|<hlink|Philosophy|../about/philosophy.en.tm>>|<hlink|Gallery|../home/screenshots.en.tm>|<obsolete|<hlink|Artwork|../about/artwork.en.tm>>|<hlink|News|../home/news.en.tm>|<obsolete|<hlink|Changes|../about/changes.en.tm>>|<obsolete|<hlink|Jobs|../home/jobs.en.tm>>|<hlink|Plans|../about/plans.en.tm>|<obsolete|<hlink|To
  do|../about/todo.en.tm>>|<obsolete|<hlink|Roadmap|../about/roadmap.en.tm>>|<hlink|Thanks|../home/thanks.en.tm>|<obsolete|<hlink|Authors|../about/authors.en.tm>>|<obsolete|<hlink|Donators|../about/donators.en.tm>>>>>>

  <assign|tmweb-download-links|<macro|<style-with|src-compact|none|<tmweb-link-section|Download|<hlink|Sources|../download/sources.en.tm>|<hlink|GNU
  Linux|../download/unix.en.tm>|<hlink|MacOS|../download/macosx.en.tm>|<hlink|Windows|../download/windows.en.tm>|<hlink|Other|../download/other.en.tm>|<hlink|License|../about/license.en.tm>>>>>

  <assign|tmweb-learn-links|<macro|<style-with|src-compact|none|<tmweb-link-section|Learn|<hlink|Videos|../home/videos.en.tm>|<hlink|Tutorials|../help/tutorial.en.tm>|<hlink|Books|../help/book.en.tm>|<hlink|FAQ|../help/faq.en.tm>|<hlink|Mailing
  lists|../home/ml.en.tm>>>>>

  <assign|tmweb-contribute-links|<macro|<style-with|src-compact|none|<tmweb-link-section|Contribute|<obsolete|<hlink|Contribute|../contribute/contribute.en.tm>>|<obsolete|<hlink|Team|../contribute/team.en.tm>>|<hlink|Donate|../contribute/donations.en.tm>|<hlink|Develop|../contribute/develop.en.tm>|<hlink|Document|../contribute/documentation.en.tm>|<hlink|Translate|../contribute/translations.en.tm>|<hlink|Plug-ins|../contribute/plugins.en.tm>>>>>

  <assign|tmweb-contact-links|<macro|<style-with|src-compact|none|<tmweb-link-section|Contact|<hlink|Feedback|../contact/contact.en.tm>|<hlink|Bugs|../contact/bugs.en.tm>|<hlink|Suggestions|../contact/wishes.en.tm>|<hlink|Patches|../contact/patches.en.tm>|<hlink|Mailing
  lists|../home/ml.en.tm>>>>>

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

  <assign|tmweb-license-text|<\macro>
    <\active*>
      <section*|About this website>

      This webpage is part of <hlink|GNU <TeXmacs>|http://www.texmacs.org>
      and the larger <hlink|GNU project|http://www.gnu.org>. Verbatim copying
      and distribution of it is permitted in any medium, provided this notice
      is preserved.

      <hlink|Free Software Foundation|http://www.fsf.org/fsf/fsf.html>, Inc.,
      51 Franklin Street, Fifth Floor, Boston, MA 02111, USA
    </active*>
  </macro>>

  <assign|tmweb-license|<\macro>
    <\html-div-class|tmweb-pad-above>
      <\html-div-class|tmweb-footer>
        <html-div-class|tmweb-license|<tmweb-license-text>><tmweb-home-links><tmweb-download-links><tmweb-learn-links><tmweb-contribute-links><tmweb-contact-links>
      </html-div-class>
    </html-div-class>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>