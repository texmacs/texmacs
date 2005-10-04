<TeXmacs|1.0.5.9>

<style|tmweb>

<\body>
  <tmweb-current|Download|Sources><tmweb-title|Suggestions for packaging
  <TeXmacs>|<tmweb-download-links>>

  In the case that you wish to package <TeXmacs> for some <name|Linux>,
  <name|Unix> or <name|Knoppix> distribution, it may be useful to be aware of
  a few points.

  <\itemize>
    <item>The development releases of <TeXmacs> carry four numbers, like
    <verbatim|1.0.4.6> or <verbatim|1.0.5.7>. The stable releases either two
    or three, like <verbatim|1.0>, <verbatim|1.0.4> or <verbatim|1.1>. Stable
    releases are rather frequent (twice or thrice a year), so we recommend to
    use them for all major distributions.

    <item><TeXmacs> is shipped with a small set of Type 1 fonts. For
    auto-installing distributions, we recommend to create a separate font
    package which contains these fonts, as well as several <hlink|extra Type
    1 fonts|fonts.en.tm#fonts-tarball>. You may then create a dependency of
    the main <TeXmacs> package on this separate font package.

    It is particularly important to do this in the case of <name|Knoppix>
    distributions, because all fonts created by <name|Metafont> are lost
    whenever you turn of the computer. Potential users will then spend a lot
    of time on font generation...

    <item>Please send us an email if you maintain a <TeXmacs> package for
    some distribution, so that we can maintain a list with distributions
    which support <TeXmacs>.
  </itemize>

  <tmdoc-copyright|1999--2003|Joris van der Hoeven>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>