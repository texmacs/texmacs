<TeXmacs|2.1.2>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|Installing <TeXmacs> for
  <name|Linux>|<tmweb-download-links>>

  \;

  <center|<center|<image|../images/Download-TeXmacs.png|600px|||>>>

  <tabular|<tformat|<cwith|1|4|1|1|cell-hyphen|n>|<cwith|1|-1|2|2|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-width|>|<cwith|1|-1|1|1|cell-hmode|auto>|<cwith|1|-1|2|2|cell-lsep|1spc>|<cwith|1|-1|2|2|cell-bsep|1em>|<cwith|1|-1|2|2|cell-tsep|1em>|<cwith|2|2|1|1|cell-hyphen|n>|<cwith|2|2|2|2|cell-hyphen|t>|<cwith|2|2|1|1|cell-width|>|<cwith|2|2|1|1|cell-hmode|auto>|<cwith|2|2|2|2|cell-lsep|1spc>|<cwith|2|2|2|2|cell-bsep|1em>|<cwith|2|2|2|2|cell-tsep|1em>|<table|<row|<cell|<item-pic|../images/install-method.png>>|<\cell>
    <strong|Selection of the installation method>

    Depending on your GNU/<name|Linux> distribution, you may choose between
    the following installation methods:

    <\itemize>
      <item>Some distributions (such as <hlink|Gentoo|http://www.gentoo.org/>)
      actively support <TeXmacs>, in which case you may directly install
      <TeXmacs> using the standard tools of your system.

      <item>We provide ready-to-install packages for
      <hlink|CentOS|linux-packages.en.tm#centos>,
      <hlink|Debian|linux-packages.en.tm#debian>,
      <hlink|Fedora|linux-packages.en.tm#fedora>, <hlink|Scientific
      Linux|linux-packages.en.tm#scientificlinux>, <hlink|Open
      Suse|linux-packages.en.tm#suse>, and
      <hlink|Ubuntu|linux-packages.en.tm#ubuntu>.

      <item>In the cases of <hlink|Debian|linux-repos.en.tm#debian> and
      <hlink|Ubuntu|linux-repos.en.tm#ubuntu>, we also propose a way to add a
      <TeXmacs> repository to your package manager; this makes it easy to
      detect and install new versions.

      <item>We finally provide generic binary packages for <TeXmacs>, which
      can be installed as explained below.
    </itemize>
  </cell>>|<row|<cell|<item-pic|../images/small-download.png>>|<\cell>
    <strong|Downloading <TeXmacs>>

    Depending on the version of your GNU/<name|Linux> system, download one of
    the following static binary distributions of GNU <TeXmacs>:

    <\itemize>
      <item><hlink|<TeXmacs> package for 32 bit GNU/<name|Linux>
      distributions|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|-B.tar.gz>>.

      <item><hlink|<TeXmacs> package for 64 bit GNU/<name|Linux>
      distributions|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|-C.tar.gz>>.

      <item>If the above packages do not work on your computer, then you may
      try our <hlink|alternate <TeXmacs> package|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|-A.tar.gz>>,
      which should be suitable for older systems.
    </itemize>

    Notice that we only provide static binary packages for Intel or AMD based
    PC's.
  </cell>>|<row|<cell|<item-pic|../images/TeXmacs.png>>|<\cell>
    <strong|Unpacking <TeXmacs>>

    In a shell session, <verbatim|cd> into the directory where you wish to
    install <TeXmacs> and type

    <\shell-code>
      gunzip -c TeXmacs-<with|color|brown|[version]>-<with|color|brown|[your
      system]>.tar.gz \| tar xvf -
    </shell-code>

    All files will be unpacked into the directory
    <with|font-family|tt|TeXmacs-<with|color|brown|[version]>-<with|color|brown|[your
    system]>/TeXmacs>. Let <with|font-family|tt|<with|color|brown|[installation
    directory]>> be the full path of this directory. As a sanity check, you
    may verify that this <with|font-family|tt|<with|color|brown|[installation
    directory]>> contains a <verbatim|bin> subdirectory with the <TeXmacs>
    binary <verbatim|texmacs>.

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

    We recommend to put these lines in your personal startup script, such as
    <with|font-family|tt|.bash_profile>.

    You may now run the program using

    <\shell-code>
      texmacs &
    </shell-code>
  </cell>>|<row|<cell|<item-pic|../images/Book_icon_1.png>>|<\cell>
    <strong|Learning <TeXmacs>>

    <\itemize>
      <item>Get started by watching our introductory
      <hlink|videos|../home/videos.en.tm>.

      <item>Or by reading one of the <TeXmacs>
      <hlink|tutorials|../help/tutorial.en.tm>.

      <item>For more information, please consult the <hlink|user
      manual|../help/book.en.tm>.
    </itemize>
  </cell>>|<row|<cell|<item-pic|../images/FAQ_icon.svg.png>>|<\cell>
    <strong|Any questions?>

    <\itemize>
      <item><hlink|Frequently asked questions|../help/faq.en.tm>.

      <item>Ask questions on the <hlink|<verbatim|texmacs-users> mailing
      list|../home/ml.en.tm#tmusers>.
    </itemize>
  </cell>>|<row|<cell|<item-pic|../images/Crystal_Project_money.png>>|<\cell>
    <strong|Donate>

    <\itemize>
      <item>If you like <TeXmacs>, then please consider
      <hlink|donating|../contribute/donations.en.tm> money or services to us.
    </itemize>
  </cell>>>>>

  <tmdoc-copyright|1999\U2019|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>