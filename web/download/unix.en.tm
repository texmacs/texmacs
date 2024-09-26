<TeXmacs|2.1.4>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|Installing <TeXmacs> on
  GNU/<name|Linux> systems|<tmweb-download-links>>

  <section|Installation method>

  Depending on your GNU/<name|Linux> distribution, you may choose between the
  following installation methods:

  <\enumerate>
    <item>Please <hlink|check|linux.en.tm> whether your distribution already
    supports <TeXmacs>, in which case you may directly install <TeXmacs>
    using the standard tools of your system.

    <item>Please <hlink|check|linux-packages.en.tm> whether your distribution
    is in the list of standard GNU/<name|Linux> distributions for which we
    provide ready-to-install packages.

    <item>Otherwise, you may install a generic binary package for <TeXmacs>,
    as explained below.
  </enumerate>

  If you are interested in packaging <TeXmacs> for a new GNU/<name|Linux> or
  <name|Unix> distribution, then please take a look at our
  <hlink|suggestions|packaging.en.tm>.

  <section|Download the package>

  Depending on the version of your GNU/<name|Linux> system, download one of
  the following static binary distributions of GNU <TeXmacs>:

  <\enumerate>
    <item><hlink|<TeXmacs> 2.1.2 package for 32 bit GNU/<name|Linux>
    distributions|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/2.1.2-B.tar.gz>>.

    <item><hlink|<TeXmacs> latest package for 64 bit GNU/<name|Linux>
    distributions|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|-C.tar.gz>>.

    <item><hlink|<TeXmacs> latest AppImage for 64 bit GNU/<name|Linux>
    distributions|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|-C.x86_64.AppImage>>.

    <item>If the above packages do not work on your computer, then you may
    try our <hlink|alternate <TeXmacs> package|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|-A.tar.gz>>,
    which should be suitable for older systems.
  </enumerate>

  Notice that we only provide static binary packages for Intel or AMD based
  PC's.

  <section|How to use the packages>

  <subsection|Unpack the package>

  In a shell session, <verbatim|cd> into the directory where you wish to
  install <TeXmacs> and type

  <\shell-code>
    gunzip -c TeXmacs-<with|color|brown|[version]>-<with|color|brown|[your
    system]>.tar.gz \| tar xvf -
  </shell-code>

  All files will be unpacked into the directory
  <with|font-family|tt|TeXmacs-<with|color|brown|[version]>-<with|color|brown|[your
  system]>> (or <with|font-family|tt|TeXmacs-<with|color|brown|[version]>>,
  for some older versions). Let <with|font-family|tt|<with|color|brown|[installation
  directory]>> be the full path of this directory.

  <subsection|Set the environment variables>

  Depending on your shell, either type

  <\shell-code>
    export TEXMACS_PATH=<with|color|brown|[installation directory]>

    export PATH=$TEXMACS_PATH/bin:$PATH
  </shell-code>

  or

  <\shell-code>
    setenv TEXMACS_PATH <with|color|brown|[installation directory]>

    setenv PATH $TEXMACS_PATH/bin:$PATH
  </shell-code>

  where <with|font-family|tt|<with|color|brown|[installation directory]>> is
  as in step 2. We recommend to put these lines in your personal startup
  script, such as <with|font-family|tt|.bash_profile>.

  <subsection|Happy <TeXmacs>-ing!>

  You should now be able to run the program:

  <\code>
    \ \ \ \ texmacs &
  </code>

  <section|How to use the AppImage>

  <subsection|Make the AppImage executable>

  In a shell session, <verbatim|cd> into the directory where you downloaded
  the AppImage and type

  <\shell-code>
    chmod +x TeXmacs-<with|color|brown|[version]>.<with|color|brown|[your
    system]>.AppImage
  </shell-code>

  <subsection|Enjoy <TeXmacs>-ing!>

  You should now be able to run the program:

  <\shell-code>
    ./TeXmacs-<with|color|brown|[version]>.<with|color|brown|[your
    system]>.AppImage
  </shell-code>

  If you like the program, then please consider
  <hlink|donating|../contribute/donations.en.tm> money or services to us. Of
  course, you may also <hlink|contribute|../contribute/contribute.en.tm>
  yourself. In case of problems, please <hlink|subscribe|../home/ml.en.tm> to
  the <verbatim|texmacs-dev> or <verbatim|texmacs-users> mailing lists and
  ask your questions there. You may also directly
  <hlink|contact|../contact/contact.en.tm> us, but you might need to be more
  patient.

  <tmdoc-copyright|1999\U2019|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>