Summary: A structured wysiwyg scientific text editor
Name: TeXmacs
Version: 1.0.2.11
Release: 1
Url: http://www.texmacs.org
Source: ftp://ftp.texmacs.org/pub/TeXmacs/TeXmacs-%{version}-src.tar.gz
License: GNU GPL 2.0
Packager: Joris van der Hoeven <vdhoeven@texmacs.org>
Distribution: GNU/Linux
Vendor: Jo the ripper software
Group: Applications/Editors
Requires: tetex
Buildrequires: guile-devel
BuildRoot: %{_tmppath}/%{name}-%{version}-root

%description

GNU TeXmacs is a free scientific text editor, which was both inspired
by TeX and GNU Emacs. The editor allows you to write structured documents
via a wysiwyg (what-you-see-is-what-you-get) and user friendly interface.
New styles may be created by the user. The program implements high-quality
typesetting algorithms and TeX fonts, which help you to produce
professionally looking documents.

The high typesetting quality still goes through for automatically
generated formulas, which makes TeXmacs suitable as an interface
for computer algebra systems. TeXmacs also supports the Guile/Scheme
extension language, so that you may customize the interface and
write your own extensions to the editor.

In the future, TeXmacs is planned to evoluate towards
a complete scientific office suite, with spreadsheet capacities,
a technical drawing editor and a presentation mode.

%prep
%setup -q -n TeXmacs-%{version}-src

%build
%configure
make STATIC_TEXMACS

%install
make DESTDIR=$RPM_BUILD_ROOT install
export GUILE_DATA_PATH=`guile-config info pkgdatadir`
export GUILE_LOAD_PATH=`find $GUILE_DATA_PATH -type d | grep ice-9`
cp -r -f $GUILE_LOAD_PATH $RPM_BUILD_ROOT%{_datadir}/TeXmacs/progs
chmod -f 644 $RPM_BUILD_ROOT%{_datadir}/TeXmacs/progs/ice-9/*
chmod -f 755 $RPM_BUILD_ROOT%{_datadir}/TeXmacs/progs/ice-9
mkdir -p $RPM_BUILD_ROOT/etc/X11/applnk/Applications
mkdir -p $RPM_BUILD_ROOT/usr/share/application-registry
mkdir -p $RPM_BUILD_ROOT/usr/share/mime-info
mkdir -p $RPM_BUILD_ROOT/usr/share/pixmaps
cp -f $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.desktop $RPM_BUILD_ROOT/etc/X11/applnk/Applications
cp -f $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.applications $RPM_BUILD_ROOT/usr/share/application-registry
cp $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.keys $RPM_BUILD_ROOT/usr/share/mime-info
cp $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.mime $RPM_BUILD_ROOT/usr/share/mime-info
cp $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/pixmaps/TeXmacs.xpm $RPM_BUILD_ROOT/usr/share/pixmaps

%files
%{_bindir}/fig2ps
%{_bindir}/texmacs
%{_includedir}/TeXmacs.h
%doc %{_mandir}/*/*
%{_libexecdir}/TeXmacs
%{_datadir}/TeXmacs
/etc/X11/applnk/Applications/texmacs.desktop
/usr/share/application-registry/texmacs.applications
/usr/share/mime-info/texmacs.keys
/usr/share/mime-info/texmacs.mime
/usr/share/pixmaps/TeXmacs.xpm

%clean
rm -rf $RPM_BUILD_ROOT

%changelog

* Mon Sep 22 2003   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.1.24 Release tag set by the configure script

* Sat Jun 28 2003   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.1.16 Moved "TeXmacs-%{version}" in root directory to "TeXmacs"

* Tue Jul 22 2002   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.0.10 Migrated architecture dependent options to configure.in

* Thu Feb 07 2002   Joris van der Hoeven <vdhoeven@texmacs.org>
- 0.3.5.14 Added -f for chmod

* Thu Feb 07 2002   Joris van der Hoeven <vdhoeven@texmacs.org>
- 0.3.5.11 Extra compilation options reduce size of executable

* Sun Nov 26 2001   Joris van der Hoeven <vdhoeven@texmacs.org>
- 0.3.5.7 TeXmacs mime types & pixmap

* Thu Jul 5 2001   Joris van der Hoeven & Christophe Merlet
- 0.3.4.3 Further corrections

* Sun Jun 3 2001   Bo Forslund  <bo.forslund@abc.se>
- 0.3.4.3 Some tiny adjustments in the spec
