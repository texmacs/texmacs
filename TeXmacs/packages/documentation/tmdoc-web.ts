<TeXmacs|1.0.2.6>

<\body>
  <assign|tmdoc-web-package|1.0>

  \;

  <assign|tmweb-current|<func|main|sub|<assign|tmweb-main|<apply|main>><assign|tmweb-sub|<apply|sub>>>>

  <assign|tmweb-suffix|<func|<case|<equal|<apply|language>|german>|.de.tm|<equal|<apply|language>|french>|.fr.tm|<equal|<apply|language>|italian>|.it.tm|<equal|<apply|language>|portuguese>|.pt.tm|<equal|<apply|language>|spanish>|.es.tm|.en.tm>>>

  <assign|tmweb-link|<func|what|to|<hlink|<with|color|<if|<or|<equal|<apply|what>|<apply|tmweb-main>>|<equal|<apply|what>|<apply|tmweb-sub>>>|dark
  green|brown>|<translate|<apply|what>|english|<apply|language>>>|<if|<equal|<find_file|<merge|<apply|to>|<apply|tmweb-suffix>>>|false>|<merge|<apply|to>|.en.tm>|<merge|<apply|to>|<apply|tmweb-suffix>>>>>>

  <assign|tmweb-list-sub|<func|what|<look_up|<value|what>|0><if|<is_tuple|<look_up|<value|what>|1>>|
  \| <apply|tmweb-list-sub|<look_up|<value|what>|1>>>>>

  <assign|tmweb-list|<func|what*|<if|<is_tuple|<value|what>>|
  <apply|tmweb-list-sub|<value|what>> |-->>>

  \;

  <assign|tmweb-main-links|<func|<apply|tmweb-list|<apply|tmweb-link|Home|../home/welcome>|<apply|tmweb-link|Download|../download/download>|<apply|tmweb-link|Help|../help/help>|<apply|tmweb-link|Contribute|../contribute/contribute>|<apply|tmweb-link|Plug-ins|../plugins/plugins>|<apply|tmweb-link|About|../about/authors>|<apply|tmweb-link|Contact|../contact/contact>|<hlink|<with|color|brown|Search>|http://www.texmacs.org/search>>>>

  <assign|tmweb-title|<macro|title|bar|<tmdoc-title**|<apply|tmweb-main-links>|<arg|title>|<arg|bar>>>>

  <assign|tmweb-license|<\macro>
    <\tmdoc-license>
      <\with|color|dark grey|font size|0.84>
        This webpage is part of <hlink|GNU <TeXmacs>|http://www.texmacs.org>
        and the larger <hlink|GNU project|http://www.gnu.org>. Verbatim
        copying and distribution of it is permitted in any medium, provided
        this notice is preserved. For more information or questions, please
        contact <hlink|Joris van der Hoeven|http://www.texmacs.org/Web/Mail.html>.

        <hlink|Free Software Foundation|http://www.fsf.org/fsf/fsf.html>,
        Inc., 59 Temple Place - Suite 330, Boston, MA 02111, USA
      </with>
    </tmdoc-license>
  </macro>>

  \;

  <assign|tmweb-home-links|<func|<apply|tmweb-list|<apply|tmweb-link|Welcome|welcome>|<apply|tmweb-link|Screen
  shots|screenshots>|<apply|tmweb-link|News|news>|<apply|tmweb-link|Mailing
  lists|ml>>>>

  <assign|tmweb-download-links|<func|<apply|tmweb-list|<apply|tmweb-link|General|download>|<apply|tmweb-link|Sources|sources>|<apply|tmweb-link|Binaries|unix>|<apply|tmweb-link|RPM|rpm>|<apply|tmweb-link|Knoppix|knoppix>|<apply|tmweb-link|CVS|cvs>|<apply|tmweb-link|Fonts|fonts>|<apply|tmweb-link|Requirements|requirements>>>>

  <assign|tmweb-help-links|<func|<apply|tmweb-list|<apply|tmweb-link|General|help>|<apply|tmweb-link|FAQ|faq>|<apply|tmweb-link|Tutorial|tutorial>|<apply|tmweb-link|Manual|manual>|<apply|tmweb-link|Articles|articles>>>>

  <assign|tmweb-contribute-links|<func|<apply|tmweb-list|<apply|tmweb-link|Helping|contribute>|<apply|tmweb-link|Documentation|documentation>|<apply|tmweb-link|Translations|translations>|<apply|tmweb-link|Extensions|plugins>|<apply|tmweb-link|Donations|donations>>>>

  <assign|tmweb-plugin-links|<func|<apply|tmweb-list|<apply|tmweb-link|All|plugins>|<apply|tmweb-link|Mathematics|cas>|<apply|tmweb-link|Numerics|numerics>|<apply|tmweb-link|Statistics|statistics>|<apply|tmweb-link|Physics|physics>|<apply|tmweb-link|Graphics|graphics>|<apply|tmweb-link|Education|education>|<apply|tmweb-link|Tools|tools>>>>

  <assign|tmweb-about-links|<func|<apply|tmweb-list|<apply|tmweb-link|Authors|authors>|<apply|tmweb-link|Philosophy|philosophy>|<apply|tmweb-link|Changes|changes>|<apply|tmweb-link|Plans|plans>|<apply|tmweb-link|License|license>>>>

  <assign|tmweb-contact-links|<func|<apply|tmweb-list|<apply|tmweb-link|Feedback|contact>|<apply|tmweb-link|Bugs|bugs>|<apply|tmweb-link|Suggestions|wishes>|<apply|tmweb-link|Patches|patches>>>>

  \;

  <assign|tmweb-manual-links|<func|previous|next|<apply|tmweb-list|<apply|tmweb-link|Manual|../help/manual>|<apply|tmweb-link|Top|web-manual>|<apply|tmweb-link|Previous|<apply|previous>>|<apply|tmweb-link|Next|<apply|next>>>>>

  <assign|tmweb-tutorial-links|<func|previous|next|<apply|tmweb-list|<apply|tmweb-link|Tutorial|../help/tutorial>|<apply|tmweb-link|Top|web-tutorial>|<apply|tmweb-link|Previous|<apply|previous>>|<apply|tmweb-link|Next|<apply|next>>>>>

  \;

  <assign|tmweb-image|<func|name|<postscript|<merge|http://www.texmacs.org/Samples/|<apply|name>>|*2/3|||||>>>

  <assign|tmweb-email|<macro|name|domain|<with|font family|tt|color|dark
  magenta|\<less\>><with|font family|tt|color|dark blue|<arg|name>><with|font
  family|tt|color|dark magenta|@><with|font family|tt|color|dark
  blue|<arg|domain>><with|font family|tt|color|dark magenta|\<gtr\>>>>

  \;

  <assign|texmacs-stable-targz|<macro|1.0.2>>

  <assign|texmacs-stable-rpm|<macro|1.0.2-1>>

  <assign|texmacs-version|<macro|1.0.2.1>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>