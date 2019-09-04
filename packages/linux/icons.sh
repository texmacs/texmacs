#! /bin/bash
# Install icons file in the relevant directory according user's permissions
# Denis RAUX LIX 2019
#
shopt -s nullglob
test -z "$1" && exit 10
mode=$1;shift

test -n "$1" && cd $1 || exit 11

type xdg-icon-resource 1>/dev/null || exit 12
iapp=$2
idoc=$3

function cpicon {
	if test -n "$1" -a -n "$2"
	then	mkdir -p $svgdst/$2 && cp $1.svg $svgdst/$2
				for i in $1-*.png
				do	sz=${i##*-};sz=${sz%.png}
				xdg-icon-resource install --noupdate --novendor --theme hicolor --context $2 --size $sz $i $1
				done
				echo Icon $1 installed in $2
	fi
}

function rmicon {
	if test -n "$1" -a -n "$2"
	then	rm $svgdst/$2/$1.svg
				for i in $1-*.png
				do	sz=${i##*-};sz=${sz%.png}
				xdg-icon-resource uninstall --noupdate --theme hicolor --context $2 --size $sz $1
				done
	fi
}

if test -w /usr/share -o "$XDG_UTILS_INSTALL_MODE" == "system"
then  export XDG_DATA_DIRS="$DESTDIR/usr/share/"
			export svgdst=$DESTDIR/usr/local/share/icons/hicolor/scalable
			export XDG_UTILS_INSTALL_MODE=system
else 	export svgdst=$HOME/.local/share/icons/hicolor/scalable
			export XDG_UTILS_INSTALL_MODE=user
fi
export XDG_UTILS_DEBUG_LEVEL=0

case $mode in
install)
	cpicon $iapp apps
 	cpicon $idoc mimetypes
 	;;
uninstall)
	rmicon $iapp apps
	rmicon $idoc mimetypes
	;;
*) exit 1;;
esac
xdg-icon-resource forceupdate --theme hicolor
