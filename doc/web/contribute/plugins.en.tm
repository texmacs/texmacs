<TeXmacs|1.0.2.3>

<style|tmweb>

<\body>
  <apply|tmweb-current|Contribute|Extensions><expand|tmweb-title|Writing
  plug-ins for <TeXmacs>|<apply|tmweb-contribute-links>>

  <TeXmacs> can easily be extended with new functionality using the mechanism
  of plug-ins. This mechanism may serve multiple purposes:

  <\itemize>
    <item>Interfacing <TeXmacs> with existing software.

    <item>Extending <TeXmacs> with new style files.

    <item>Customizing the keyboard and menus for particular purposes.

    <item>Extend <TeXmacs> with new functionality using the
    <name|Guile>/<name|Scheme> extension language.

    <item>Extend <TeXmacs> with new typesetting primitives written in
    <name|Scheme>.
  </itemize>

  You might wish to take a look at the <apply|hyper-link|list of existing
  plug-ins for <TeXmacs>|../plugins/plugins.en.tm>. If you wish to write a
  new plug-in, then we recommend you to carefully read the documentation in
  the <apply|menu|Help|Manual|TeXmacs plugins> and
  <apply|menu|Help|Interfacing> menus.

  <apply|tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <expand|tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|made-by-TeXmacs>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Title>|<with|font family|<quote|ss>|TeXmacs
      notice>>|<pageref|idx-2>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Ordinary
      users><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Programmers><value|toc-dots><pageref|toc-2><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>