<TeXmacs|1.0.2.6>

<style|tmweb>

<\body>
  <apply|tmweb-current|Download|Binaries><tmweb-title|Installing <TeXmacs> on
  MacOS-X using <name|Fink>|<apply|tmweb-download-links>>

  You may use <TeXmacs> on the MacOS-X platform using
  <hlink|<name|Fink>|http://fink.sourceforge.net/>. The <name|Fink> project
  wants to bring the full world of Unix Open Source software to Darwin and
  MacOS-X. In order to install <TeXmacs>, you should therefore first install
  <name|Fink> and next <TeXmacs>.

  <section|Install <name|Fink> via its binary installer>

  Download the <name|Fink> installer from the <name|Fink> <hlink|download
  page|http://fink.sourceforge.net/download/index.php> and run it following
  the instructions (steps 1 and) on that page. Next add the line

  <\code>
    \ \ \ \ source /sw/bin/init.csh
  </code>

  to the file <with|font family|tt|.cshrc> or <with|font family|tt|.tcshrc>
  in your home directory, depending on which shell you use. If you don't know
  which shell you use, then add the line to both files. If the files do not
  exist, then create them.

  Ignore the instructions to use dselect, use FinkCommander instead as
  described in the following.

  <section|Install FinkCommander>

  You can run <name|Fink> from the command line, but for newcomers it is
  recommended to use the graphical interface provided by FinkCommander.
  Download the disk image from the FinkCommander <hlink|download
  page|http://finkcommander.sourceforge.net/> and install it as explained on
  that page.

  <section|Check the prerequisites>

  You now need to decide whether you want to install the precompiled binary
  of <TeXmacs> or whether you want to compile it from source. If you want to
  have the very latest version of <TeXmacs>, then you should compile from
  source. In this case, continue at step 6. The next thing to check is
  whether you already have <with|font family|tt|teTeX> and/or <with|font
  family|tt|X11> installed. If you don't, then continue at step 4.

  If you have Gerben Wierda's <with|font family|tt|teTeX> installed, you need
  to tell <name|Fink> about it by installing the very small <with|font
  family|tt|system-tetex> package. Start FinkCommander, select the <with|font
  family|tt|system-tetex> package from the package list, and choose the
  <with|font series|bold|Binary-\<gtr\>Install> menu option.

  If you have <with|font family|tt|X11> installed, either Apple's <with|font
  family|tt|X11> or <with|font family|tt|XDarwin>, you need to install the
  <with|font family|tt|system-xfree86> placeholder package. In case of
  Apple's <with|font family|tt|X11>, make sure you have both packages,
  <with|font family|tt|X11User> and <with|font family|tt|X11SDK>, installed.
  Then start FinkCommander, select <with|font family|tt|system-xfree86> from
  the package list, and install it by selecting <with|font
  series|bold|Binary-\<gtr\>Install>.

  <section|Install the <TeXmacs> binary package>

  In order to install the binary version of <TeXmacs>, start FinkCommander,
  select <with|font family|tt|texmacs> in the package list, and choosing the
  <with|font series|bold|Binary-\<gtr\>Install> menu option. <name|Fink> will
  now proceed to download and install a number of packages needed for
  <TeXmacs>, including, if you didn't have them yet, <with|font
  family|tt|tetex> and <with|font family|tt|xfree86>. Finally it will install
  <with|font family|tt|texmacs>. All this should run automatically.

  <section|Running <TeXmacs>>

  In order to run <TeXmacs>, launch a terminal, start the X-server and type

  <\code>
    \ \ \ \ texmacs &
  </code>

  Please <apply|hyper-link|let us know|../contact/contact.en.tm> if you like
  or dislike the program. It is very useful for us to have a rough idea about
  the number of permanent users and for what applications you are interested
  in <TeXmacs>. Furthermore, we are glad to receive your suggestions and
  problems, no matter how silly they may seem to you. You may also
  <apply|hyper-link|suscribe|../home/ml.en.tm> to the
  <verbatim|texmacs-users> or <verbatim|texmacs-info> mailing lists. If you
  really like the program, then please consider
  <apply|hyper-link|donating|../contribute/donations.en.tm> money or services
  to us. Of course, you may also <apply|hyper-link|contribute|../contribute/contribute.en.tm>
  yourself.

  <section|Installing <TeXmacs> from source>

  If you want to compile <TeXmacs> from source, you first need to make sure
  that you have the Developer Tools installed. If you don't, you need to
  <hlink|create a (free) ADC membership|http://connect.apple.com> and
  download and install the Developer Tools.

  Next you need to activate the unstable tree in your <with|font
  family|tt|fink> directory. You can do this from the FinkCommander
  Preferences. Then run <with|font family|tt|selfupdate-cvs> from the Source
  menu.

  In case you have <with|font family|tt|teTeX> and/or <with|font
  family|tt|X11> installed from somewhere else than fink, you first need to
  install the <with|font family|tt|system-tetex> and/or the <with|font
  family|tt|system-xfree86> packages. You can do this from FinkCommander.

  In order to install <TeXmacs>, run FinkCommander, select <with|font
  family|tt|texmacs> from the file list, and run <with|font
  series|bold|Source-\<gtr\>Install>. <name|Fink> will proceed to download
  the sources of <TeXmacs> and its supporting packages and compile and
  install them. You may run <TeXmacs> as explained in step 5.

  <apply|tmdoc-copyright|1999--2003|Martin Costabel|Joris van der Hoeven>

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
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Install <with|font
      shape|<quote|small-caps>|Fink> via its binary
      installer><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Install
      FinkCommander><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Check the
      prerequisites><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Install the
      T<rsub|<space|-0.4spc><move|<resize|<with|index
      level|<quote|0>|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>
      binary package><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Running
      T<rsub|<space|-0.4spc><move|<resize|<with|index
      level|<quote|0>|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Installing
      T<rsub|<space|-0.4spc><move|<resize|<with|index
      level|<quote|0>|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>
      from source><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>