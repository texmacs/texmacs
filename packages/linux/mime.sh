#! /bin/bash
# Install desktop ans mime files
# Denis RAUX LIX 2019
#
shopt -s nullglob
test -z "$1" && exit 10
mode=$1;shift

if test -w /usr/share
then  export svgdst=/usr/share/icons/hicolor/scalable
			export XDG_UTILS_INSTALL_MODE=system
else 	export svgdst=$HOME/.local/share/icons/hicolor/scalable
			export XDG_UTILS_INSTALL_MODE=user
fi
export XDG_UTILS_DEBUG_LEVEL=9

case $mode in
install)
	test -n "$1" && cd $1 || exit 11
	xdg-desktop-menu install --novendor texmacs.desktop
	xdg-mime install --novendor texmacs.xml
	;;
uninstall)
	xdg-desktop-menu uninstall texmacs.desktop
	xdg-mime uninstall texmacs.xml
	;;
*) exit 1;;
esac
