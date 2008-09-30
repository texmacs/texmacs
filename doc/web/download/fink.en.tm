<TeXmacs|1.0.6.15>

<style|tmweb>

<\body>
  <tmweb-current|Download|Binaries><tmweb-title|Installing <TeXmacs> on
  MacOS-X|<tmweb-download-links>>

  You may use <TeXmacs> on the MacOS-X platform using
  <hlink|<name|Fink>|http://fink.sourceforge.net/>. The <name|Fink> project
  wants to bring the full world of Unix Open Source software to Darwin and
  MacOS-X. In order to install <TeXmacs>, you should therefore first install
  <name|Fink> and next <TeXmacs>.

  A possible alternative for <name|Fink> is
  <hlink|Macports|http://www.macports.org>, which also provides a
  <hlink|<TeXmacs> package|http://www.macports.org/ports.php?by=name&substr=texmacs>.

  <section|Install <name|Fink> via its binary installer>

  Download the <name|Fink> installer from the <name|Fink> <hlink|download
  page|http://fink.sourceforge.net/download/index.php> and run it following
  the instructions (steps 1 and further) on that page.

  <section|Install FinkCommander>

  You can run <name|Fink> from the command line, but for newcomers it is
  recommended to use the graphical interface provided by FinkCommander.
  FinkCommander is included in the <name|Fink> installer as explained on the
  <name|Fink> download page.

  <section|Check the prerequisites>

  You now need to decide whether you want to install the precompiled binary
  of <TeXmacs> or whether you want to compile it from source. If you want to
  have the very latest version of <TeXmacs>, then you should compile from
  source. In this case, continue at step 6. The next thing to check is
  whether you already have <with|font-family|tt|teTeX> and/or
  <with|font-family|tt|X11> installed. If you don't, then continue at step 4.

  If you have Gerben Wierda's <with|font-family|tt|teTeX> installed, you need
  to tell <name|Fink> about it by installing the very small
  <with|font-family|tt|system-tetex> package. Start FinkCommander, select the
  <with|font-family|tt|system-tetex> package from the package list, and
  choose the <with|font-series|bold|Binary-\<gtr\>Install> menu option.

  Make sure that you have Apple's <with|font-family|tt|X11> installed. For
  MacOS X 10.2 you will have to install <with|font-family|tt|X11> from Fink.
  Useful information about <with|font-family|tt|X11> in combination with
  <name|Fink> is provided on the the <name|Fink> download page.

  <section|Install the <TeXmacs> binary package>

  In order to install the binary version of <TeXmacs>, start FinkCommander,
  select <with|font-family|tt|texmacs> in the package list, and choosing the
  <with|font-series|bold|Binary-\<gtr\>Install> menu option. <name|Fink> will
  now proceed to download and install a number of packages needed for
  <TeXmacs>, including, if you didn't have them yet,
  <with|font-family|tt|tetex> and <with|font-family|tt|xfree86>. Finally it
  will install <with|font-family|tt|texmacs>. All this should run
  automatically.

  <section|Running <TeXmacs>>

  In order to run <TeXmacs>, launch <with|font-family|tt|Terminal.app> from
  <samp|/Applications/Utilities> and type

  <\code>
    \ \ \ \ open-x11 texmacs &
  </code>

  Please <hlink|let us know|../contact/contact.en.tm> if you like or dislike
  the program. It is very useful for us to have a rough idea about the number
  of permanent users and for what applications you are interested in
  <TeXmacs>. Furthermore, we are glad to receive your suggestions and
  problems, no matter how silly they may seem to you. You may also
  <hlink|suscribe|../home/ml.en.tm> to the <verbatim|texmacs-users> or
  <verbatim|texmacs-info> mailing lists. If you really like the program, then
  please consider <hlink|donating|../contribute/donations.en.tm> money or
  services to us. Of course, you may also
  <hlink|contribute|../contribute/contribute.en.tm> yourself.

  <section|Installing <TeXmacs> from source>

  If you want to compile <TeXmacs> from source, you first need to make sure
  that you have the Developer Tools installed. They are included on the Mac
  OS X install CD/DVD. If you cannot use these, you need to <hlink|create a
  (free) ADC membership|http://connect.apple.com> and download and install
  the Developer Tools.

  Next you need to activate the unstable tree in your
  <with|font-family|tt|fink> directory. You can do this from the
  FinkCommander Preferences. Then run <with|font-family|tt|selfupdate-cvs>
  from the Source menu.

  In case you have <with|font-family|tt|teTeX> and/or
  <with|font-family|tt|X11> installed from somewhere else than fink, you
  first need to install the <with|font-family|tt|system-tetex> and/or the
  <with|font-family|tt|system-xfree86> packages. You can do this from
  FinkCommander.

  In order to install <TeXmacs>, run FinkCommander, select
  <with|font-family|tt|texmacs> from the file list, and run
  <with|font-series|bold|Source-\<gtr\>Install>. <name|Fink> will proceed to
  download the sources of <TeXmacs> and its supporting packages and compile
  and install them. You may run <TeXmacs> as explained in step 5.

  <tmdoc-copyright|1999--2006|Martin Costabel|Joris van der Hoeven, Ingolf
  Schäfer>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>