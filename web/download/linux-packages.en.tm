<TeXmacs|2.1.4>

<style|<tuple|tmweb2|old-dots|old-lengths>>

<\body>
  <tmweb-current|Download|Linux><tmweb-title|Binary GNU/<name|Linux> packages
  for <TeXmacs>|<tmweb-download-links>>

  We provide our own binary <TeXmacs> packages for the following
  GNU/<name|Linux> distributions:

  <\itemize>
    <item><hlink|CentOS|#centos>.

    <item><hlink|Debian|#debian>.

    <item><hlink|Fedora|#fedora>.

    <item><hlink|Scientific Linux|#scientificlinux>.

    <item><hlink|Open Suse|#suse>.

    <item><hlink|Ubuntu|#ubuntu>.
  </itemize>

  Further distributions may be added progressively
  <hlink|here|ftp://ftp.texmacs.org/TeXmacs/tmftp/Linux/>.

  <strong|Important>: In some cases you need to manually install the
  additional guile package. You can download it
  <hlink|here|ftp://ftp.texmacs.org/TeXmacs/tmftp/Linux/>.

  <section*|CentOS><label|centos>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    CentOS and your processor:

    <\itemize>
      <item><hlink|<TeXmacs> package for CentOS-7 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_7/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>

      <item><hlink|<TeXmacs> package for CentOS-8 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_8/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>
    </itemize>

    We also have the following older versions of <TeXmacs> for older versions
    of CentOS:

    <\itemize>
      <item><hlink|<TeXmacs> package for CentOS-6 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_6/|TeXmacs-2.1.1.i686.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for CentOS-6 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/CentOS_6/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)
    </itemize>

    <item>Install the package using

    <\shell-code>
      yum install <merge|<TeXmacs-version-release|devel>|-*.rpm>
    </shell-code>
  </enumerate>

  <section*|Debian><label|debian>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><hlink|<TeXmacs> package for Debian-9 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_9.0/|<merge|<TeXmacs-version-release|devel>|.i386.deb>>>

      <item><hlink|<TeXmacs> package for Debian-9 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_9.0/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>

      <item><hlink|<TeXmacs> package for Debian-10 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_10/|<merge|<TeXmacs-version-release|devel>|.i386.deb>>>

      <item><hlink|<TeXmacs> package for Debian-10 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_10/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>

      <item><hlink|<TeXmacs> package for Debian-11 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_11/|<merge|<TeXmacs-version-release|devel>|.i386.deb>>>

      <item><hlink|<TeXmacs> package for Debian-11 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_11/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>

      <item><hlink|<TeXmacs> package for Debian-12 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_12/|<merge|<TeXmacs-version-release|devel>|.i386.deb>>>

      <item><hlink|<TeXmacs> package for Debian-12 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_12/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>
    </itemize>

    We also have the following older versions of <TeXmacs> for older versions
    of Debian:

    <\itemize>
      <item><hlink|<TeXmacs> package for Debian-7 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_7.0/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Debian-7 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_7.0/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Debian-8 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_8.0/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Debian-8 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Debian_8.0/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)
    </itemize>

    <item>As <verbatim|root>, install the package using

    <\shell-code>
      sudo apt-get install <merge|<TeXmacs-version-release|devel>|-*.deb>
    </shell-code>

    If you get complaints about missing dependencies, then run

    <\shell-code>
      sudo apt --fix-broken install
    </shell-code>
  </enumerate>

  <section*|Fedora><label|fedora>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><hlink|<TeXmacs> package for Fedora-36 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_36/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>

      <item><hlink|<TeXmacs> package for Fedora-37 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_37/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>

      <item><hlink|<TeXmacs> package for Fedora-38 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_38/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>

      <item><hlink|<TeXmacs> package for Fedora-39 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_39/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>
    </itemize>

    We also have the following older versions of <TeXmacs> for older versions
    of Fedora:

    <\itemize>
      <item><hlink|<TeXmacs> package for Fedora-27 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_27/|TeXmacs-2.1.1.i686.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-27 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_27/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-28 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_28/|TeXmacs-2.1.1.i686.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-28 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_28/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-29 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_29/|TeXmacs-2.1.1.i686.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-29 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_29/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-30 with a 32 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_30/|TeXmacs-2.1.1.i686.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-30 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_30/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-31 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_31/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-32 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_32/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Fedora-33 with a 64 bits Intel/AMD
      processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/Fedora_33/|TeXmacs-2.1.2.x86_64.rpm>>
      (version 2.1.2)
    </itemize>

    <item>Install the package using

    <\shell-code>
      dnf install <merge|<TeXmacs-version-release|devel>|-*.rpm>
    </shell-code>
  </enumerate>

  <section*|Scientific Linux><label|scientificlinux>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><hlink|<TeXmacs> package for Scientific Linux-7 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ScientificLinux_7/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>
    </itemize>

    We also have the following older versions of <TeXmacs> for older versions
    of Scientific Linux:

    <\itemize>
      <item><hlink|<TeXmacs> package for Scientific Linux-6 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ScientificLinux_6/|TeXmacs-2.1.1.i386.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Scientific Linux-6 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ScientificLinux_6/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)
    </itemize>

    <item>Install the package using

    <\shell-code>
      yum install <merge|<TeXmacs-version-release|devel>|-*.rpm>
    </shell-code>
  </enumerate>

  <section*|Open Suse><label|suse>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Debian and your processor:

    <\itemize>
      <item><hlink|<TeXmacs> package for Open Suse Leap-15.4 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_15.4/|<merge|<TeXmacs-version-release|devel>|.x86_64.rpm>>>
    </itemize>

    We also have the following older versions of <TeXmacs> for older versions
    of Fedora:

    <\itemize>
      <item><hlink|<TeXmacs> package for Open Suse Leap-15.1 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_15.1/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Open Suse Leap-15.2 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_15.2/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Open Suse Leap-42.2 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_42.2/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Open Suse Leap-42.3 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Leap_42.3/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Open Suse Tumbleweed with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Tumbleweed/|TeXmacs-2.1.1.i586.rpm>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for Open Suse Tumbleweed with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/openSUSE_Tumbleweed/|TeXmacs-2.1.1.x86_64.rpm>>
      (version 2.1.1)
    </itemize>

    <item>Install the package using

    <\shell-code>
      dpkg -i <merge|<TeXmacs-version-release|devel>|-*.deb>
    </shell-code>
  </enumerate>

  <section*|Ubuntu><label|ubuntu>

  In order to install <TeXmacs>, please follow the steps below:

  <\enumerate>
    <item>Download the latest version that corresponds to your version of
    Ubuntu and your processor:

    <\itemize>
      <item><hlink|<TeXmacs> package for xUbuntu_18.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_18.04/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>

      <item><hlink|<TeXmacs> package for xUbuntu_20.04 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_20.04/|<merge|<TeXmacs-version-release|devel>|.i386.deb>>>

      <item><hlink|<TeXmacs> package for xUbuntu_20.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_20.04/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>

      <item><hlink|<TeXmacs> package for xUbuntu_22.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ubuntu2210/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>

      <item><hlink|<TeXmacs> package for xUbuntu_23.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ubuntu2304/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>

      <item><hlink|<TeXmacs> package for xUbuntu_23.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ubuntu2310/|<merge|<TeXmacs-version-release|devel>|.amd64.deb>>>
    </itemize>

    We also have the following older versions of <TeXmacs> for older versions
    of Ubuntu:

    <\itemize>
      <item><hlink|<TeXmacs> package for xUbuntu_12.04 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_12.04/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_12.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_12.04/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_14.04 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_14.04/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_14.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_14.04/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_16.04 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_16.04/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_16.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_16.04/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_16.10 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_16.10/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_16.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_16.10/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_17.04 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_17.04/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_17.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_17.04/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_17.10 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_17.10/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_17.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_17.10/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_18.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_18.10/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_19.04 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_19.04/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_19.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_19.04/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_19.10 with a 32 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_19.10/|TeXmacs-2.1.1.i386.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_19.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/xUbuntu_19.10/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_20.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ubuntu2010/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_21.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ubuntu2104/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_21.10 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ubuntu2110/|TeXmacs-2.1.1.amd64.deb>>
      (version 2.1.1)

      <item><hlink|<TeXmacs> package for xUbuntu_22.04 with a 64 bits
      Intel/AMD processor|<merge|http://www.texmacs.org/Download/ftp/tmftp/Linux/ubuntu2204/|TeXmacs-2.1.2.amd64.deb>>
      (version 2.1.2)
    </itemize>

    <item>As <verbatim|root>, install the package using

    <\shell-code>
      sudo apt-get install <merge|<TeXmacs-version-release|devel>|-*.deb>
    </shell-code>

    If you get complaints about missing dependencies, then run

    <\shell-code>
      sudo apt --fix-broken install
    </shell-code>
  </enumerate>

  <tmdoc-copyright|1999\U2018|Denis Raux|Joris van der Hoeven>

  <tmweb-license>
</body>

<initial|<\collection>
</collection>>