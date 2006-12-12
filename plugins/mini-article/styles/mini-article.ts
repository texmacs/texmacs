<TeXmacs|1.0.0.7>

<style|letter>

<\body>
  <assign|odd page footer|>

  <assign|even page footer|>

  <assign|column separation|14mm>

  \;

  <assign|thetitle|>

  <assign|theauthor|>

  <assign|theaddress|>

  <assign|title*|<func|name|<assign|thetitle|<value|name>>>>

  <assign|author*|<func|name|<assign|theauthor|<value|name>>>>

  <assign|title|<macro|body|<apply|title*|<value|body>><with|math font
  series|bold|font series|bold|font size|1.68|<arg|body>>>>

  <assign|author|<macro|body|<apply|author*|<value|body>><vspace*|1fn><with|f\
  ont shape|small-caps|<translate|by|english|<apply|language>> <arg|body>>>>

  <assign|address|<macro|body|<with|interparagraph
  space|0fn|<surround|<vspace*|0.5fn>||<arg|body>>>>>

  <assign|address-block|<macro|x|<tformat|<cwith|1|-1|1|1|cell
  lsep|1.5fn>|<cwith|1|-1|-1|-1|cell rsep|1.5fn>|<twith|table
  valign|T>|<arg|x>>>>

  <assign|title-date|<macro|body|<vspace*|1.5fn><with|font
  shape|italic|<arg|body>>>>

  <assign|make-title|<macro|body|<surround||<vspace|2fn>|<with|paragraph
  mode|center|<arg|body>>>>>

  \;

  <assign|space after itemize|0.5fn>

  <assign|item space|0fn>

  <assign|itemize|<macro|body|<surround|<format|no page break
  before>|<vspace|<apply|space after itemize>><format|no indentation
  after>|<with|left margin|<plus|<value|left
  margin>|1.8333fn>|item|<macro|<vspace*|<apply|item space>><with|first
  indentation|-1.5fn|<format|enable first indentation>><resize|<apply|thetag>
  |||r]1.5fn|>>|<arg|body>>>>>

  <assign|theitem|<func|<apply|itemnr>.>>

  <assign|thesubitem|<func|<apply|itemnr>.>>

  <assign|enumerate|<macro|body|<surround|<format|no page break
  before>|<vspace|0.5fn><format|no indentation after>|<with|itemnr|0|left
  margin|<plus|<value|left margin>|1.8333fn>|item|<macro|<vspace*|0.5fn><assi\
  gn|itemnr|<plus|<value|itemnr>|1>><assign|thelabel|<apply|theitem>><with|fi\
  rst indentation|-3fn|<format|enable first
  indentation>><resize|<apply|theitem>|r-2.5fn||r+0.5fn|>>|<arg|body>>>>>

  <assign|subenumerate|<macro|body|<with|thebaseitem|<apply|theitem>|theitem|\
  <func|<apply|thebaseitem><apply|thesubitem>>|<enumerate|<arg|body>>>>>

  <assign|item* separation| >

  <assign|item* space|0.5fn>

  <assign|item*|<macro|theitem|<vspace*|<apply|item* space>><with|first
  indentation|-1.5fn|<format|enable first indentation>><resize|<with|math
  font series|bold|font series|bold|<arg|theitem>><apply|item*
  separation>|||r]1.5fn|>>>

  \;

  <assign|newitemize|<func|name|tag|<assign|<apply|name>|<hold|<macro|body|<w\
  ith|thetag|<release|<value|tag>>|<itemize|<arg|body>>>>>>>>

  <assign|newenumerate|<func|name|num|<assign|<apply|name>|<hold|<macro|body|\
  <with|theitem|<release|<value|num>>|<enumerate|<arg|body>>>>>>>>

  <assign|newsubenumerate|<func|name|num|<assign|<apply|name>|<hold|<macro|bo\
  dy|<with|theitem|<release|<value|num>>|<subenumerate|<arg|body>>>>>>>>

  \;

  <apply|newitemize|description|<with|mode|math|\<bullet\>>>

  <apply|newitemize|itemize-minus|<with|mode|math|->>

  <apply|newitemize|itemize-dot|<with|mode|math|\<bullet\>>>

  <apply|newitemize|itemize-arrow|<with|mode|math|\<rightarrow\>>>

  \;

  <apply|newenumerate|enumerate-numeric|<func|<apply|itemnr>.>>

  <apply|newenumerate|enumerate-roman|<func|<number|<apply|itemnr>|roman>.>>

  <apply|newenumerate|enumerate-Roman|<func|<number|<apply|itemnr>|Roman>.>>

  <apply|newenumerate|enumerate-alpha|<func|<number|<apply|itemnr>|alpha><wit\
  h|font shape|right|)>>>

  <apply|newenumerate|enumerate-Alpha|<func|<number|<apply|itemnr>|Alpha><wit\
  h|font shape|right|)>>>

  \;

  \;

  <assign|space before section|1.5fn>

  <assign|space after section|0.5fn>

  <assign|section|<macro|name|<vspace*|<apply|space before
  section>><apply|toc-normal-1|<arg|name>><with|math font series|bold|font
  family|ss|font series|bold|font size|1.41|<arg|name>><vspace|<apply|space
  after section>><format|no page break after>>>

  <assign|space before subsection|1fn>

  <assign|space after subsection|0.3333fn>

  <assign|subsection|<macro|name|<vspace*|<value|space before
  subsection>><apply|toc-normal-2|<arg|name>><with|math font series|bold|font
  family|ss|font series|bold|font size|1.19|<arg|name>><vspace|<apply|space
  after subsection>><format|no page break after>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|page right margin|5mm>
    <associate|reduction page left margin|25mm>
    <associate|page width|1118720unit>
    <associate|language|english>
    <associate|odd page margin|5mm>
    <associate|paragraph width|237mm>
    <associate|page medium|automatic>
    <associate|shrinking factor|7>
    <associate|interparagraph space|0>
    <associate|page top margin|5mm>
    <associate|reduction page right margin|25mm>
    <associate|reduction page bottom margin|15mm>
    <associate|page type|a4>
    <associate|even page margin|5mm>
    <associate|page height|942080unit>
    <associate|page bottom margin|5mm>
    <associate|reduction page top margin|15mm>
    <associate|page orientation|landscape>
    <\associate|~arch-tag>
      arch-tag: 70082676-a701-4ffc-8c5f-6cda1d1eea9b
    </associate>
  </collection>
</initial>
