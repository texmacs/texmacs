<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Contribute|Helping><tmweb-title|Contribute to the
  development of <TeXmacs>|<apply|tmweb-contribute-links>>

  <section|Ordinary users>

  One of the best ways to contribute to GNU <apply|TeXmacs> is by using it a
  lot, talk about it to friends and collegues, and report bugs or other
  unnatural behaviour to us. Please mention the fact that you wrote articles
  using <TeXmacs> when submitting them. You can do this by putting the
  <markup|made-by-TeXmacs> tag somewhere inside your title using
  <apply|menu|Text|Title|TeXmacs notice>.

  Besides these general (but very important) ways to contribute, you may also
  <apply|hyper-link|donate|donations.en.tm> money to us, or actively help
  with the development of <TeXmacs>. You do not necessarily need to be a
  programmer for this, since anyone may help with the
  <apply|hyper-link|internationalization|translations.en.tm> and the
  <apply|hyper-link|documentation|documentation.en.tm> of <TeXmacs>. If you
  have some artistic talents, then you might also design icons for us or nice
  pictures for upcoming <TeXmacs> T-shirts.

  <section|Programmers>

  Programmers who do not want to go into the <TeXmacs> internals may consider
  writing <apply|hyper-link|plug-ins|plugins.en.tm> for <TeXmacs>. These may
  either be interfaces with existing extern software, or enhancements to
  <TeXmacs>, like style files or new functionality written in the
  <name|Guile>/<name|Scheme> extension language.

  More experienced programmers may also help developing the <TeXmacs> core
  system (which is written in <name|C++> and <name|Scheme>). Our
  <apply|hyper-link|plans for the future|../about/plans.en.tm> may give you
  an impression about where we want to go. Of course your own ideas are
  welcome! <TeXmacs> is developed using <apply|hyper-link|CVS|../download/cvs.en.tm>,
  but we prefer contributions in the form of
  <apply|hyper-link|patches|../contact/patches.en.tm>.

  If you are interested, then you may also wish to suscribe to our
  <apply|hyper-link|developers mailing list|../home/ml.en.tm#tmdev>.

  <apply|tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|1|?>>
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