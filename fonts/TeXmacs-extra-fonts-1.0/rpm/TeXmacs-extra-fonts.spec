Summary: Fonts for the TeXmacs editor
Name: TeXmacs-extra-fonts
Version: 1.0
Release: 1
Url: http://www.texmacs.org
Source: ftp://ftp.texmacs.org/pub/TeXmacs/TeXmacs-extra-fonts-%{version}.tar.gz
License: GNU GPL 2.0
Packager: Joris van der Hoeven <vdhoeven@texmacs.org>
Distribution: GNU/Linux
Vendor: Jo the ripper software
Group: Applications/Editors
Requires: tetex tetex-fonts
Buildarch: noarch
BuildRoot: %{_tmppath}/%{name}-root

%description

This package provides extra fonts for the GNU TeXmacs text editor.
These fonts complement those which are already in the editor or
in the Bluesky fonts which are shipped with teTeX.

%prep
%setup -q

%build
make

%install
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/ec
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/la
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/math
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/tc
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/ec
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/la
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/math
mkdir -p $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/tc
cp TeXmacs/fonts/tfm/ec/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/ec
cp TeXmacs/fonts/tfm/la/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/la
cp TeXmacs/fonts/tfm/math/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/math
cp TeXmacs/fonts/tfm/tc/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/tfm/tc
cp TeXmacs/fonts/type1/ec/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/ec
cp TeXmacs/fonts/type1/la/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/la
cp TeXmacs/fonts/type1/math/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/math
cp TeXmacs/fonts/type1/tc/* $RPM_BUILD_ROOT/usr/share/TeXmacs/fonts/type1/tc

%files
%defattr(-,root,root)
/usr/share/TeXmacs/fonts/tfm/ec/*.tfm
/usr/share/TeXmacs/fonts/tfm/la/*.tfm
/usr/share/TeXmacs/fonts/tfm/math/*.tfm
/usr/share/TeXmacs/fonts/tfm/tc/*.tfm
/usr/share/TeXmacs/fonts/type1/ec/*.pfb
/usr/share/TeXmacs/fonts/type1/la/*.pfb
/usr/share/TeXmacs/fonts/type1/math/*.pfb
/usr/share/TeXmacs/fonts/type1/tc/*.pfb

%clean
rm -rf $RPM_BUILD_ROOT
