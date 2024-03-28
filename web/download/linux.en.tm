<TeXmacs|2.1.4>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|Installing <TeXmacs> for
  <name|Linux>|<tmweb-download-links>>

  \;

  <center|<center|<image|../images/Download-TeXmacs.png|600px|||>>>

  <tabular|<tformat|<cwith|1|4|1|1|cell-hyphen|n>|<cwith|1|-1|2|2|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|1|cell-width|>|<cwith|1|-1|1|1|cell-hmode|auto>|<cwith|1|-1|2|2|cell-lsep|1spc>|<cwith|1|-1|2|2|cell-bsep|1em>|<cwith|1|-1|2|2|cell-tsep|1em>|<cwith|3|3|1|1|cell-hyphen|n>|<cwith|3|3|2|2|cell-hyphen|t>|<cwith|3|3|1|1|cell-width|>|<cwith|3|3|1|1|cell-hmode|auto>|<cwith|3|3|2|2|cell-lsep|1spc>|<cwith|3|3|2|2|cell-bsep|1em>|<cwith|3|3|2|2|cell-tsep|1em>|<table|<row|<cell|<item-pic|../images/install-method.png>>|<\cell>
    <strong|Packages for specific GNU/<name|Linux>
    distributions><vspace|0.5fn>

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
    </itemize>
  </cell>>|<row|<cell|<item-pic|../images/appimage.png>>|<\cell>
    <strong|An AppImage for <TeXmacs>><vspace|0.5fn>

    The recommended generic way to install <TeXmacs> on GNU/<name|Linux>
    distributions with no dedicated <TeXmacs> packages is to use an
    <hlink|<name|AppImage>|https://appimage.org/>. For this, you should first
    download the <hlink|<TeXmacs> <name|AppImage> for 64 bit GNU/<name|Linux>
    distributions|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|.x86_64.AppImage>>.<vspace|0.5fn>

    After downloading, simply move the <name|AppImage> to your desktop or any
    other convenient location and double click on it to launch
    <TeXmacs>.<vspace|0.5fn>

    On some older versions of GNU/<name|Linux>, you may need to give your
    system the permissions to execute the <name|AppImage>. In that case, open
    a terminal and type the following command in the directory that contains
    the <name|AppImage>:

    <\shell-code>
      chmod a+x <merge|<TeXmacs-version-release|devel>|.x86_64.AppImage>
    </shell-code>

    After that, double clicking on <name|AppImage> should launch <TeXmacs>.
  </cell>>|<row|<cell|<item-pic|../images/small-download.png>>|<\cell>
    <strong|Static binary packages for <TeXmacs>><vspace|0.5fn>

    An alternative generic way to install <TeXmacs> on GNU/<name|Linux>
    system is to use our static binary package. For this, you should first
    download the <hlink|<TeXmacs> package for 64 bit GNU/<name|Linux>
    distributions|<merge|https://www.texmacs.org/Download/ftp/tmftp/generic/|<TeXmacs-version-release|devel>|-C.tar.gz>>.
    Note that we only provide static binary packages for Intel or AMD based
    PC's.<vspace|0.5fn>

    For some older GNU/<name|Linux> distributions, you may try our
    <hlink|package for 32 bit GNU/<name|Linux>
    distributions|https://www.texmacs.org/Download/ftp/tmftp/generic/TeXmacs-2.1.1-B.tar.gz>
    for the older version 2.1.1 of <TeXmacs>. For very old \ GNU/<name|Linux>
    distributions you may finally try our <hlink|alternate <TeXmacs>
    package|https://www.texmacs.org/Download/ftp/tmftp/generic/TeXmacs-2.1.1-A.tar.gz>.<vspace|0.5fn>

    <strong|Unpacking the static binary package and running
    <TeXmacs>><vspace|0.5fn>

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