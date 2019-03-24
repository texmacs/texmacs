#!/bin/bash

if [ -L ${BASH_SOURCE-$0} ]; then
  FWDIR=$(dirname $(readlink "${BASH_SOURCE-$0}"))
else
  FWDIR=$(dirname "${BASH_SOURCE-$0}")
fi

APP_HOME="$(cd "${FWDIR}/.."; pwd)"

ln -s $APP_HOME/packages/debian $APP_HOME/debian
rm $APP_HOME/configure
rm $APP_HOME/configure.in
rm $APP_HOME/config.sub
rm $APP_HOME/config.guess
rm $APP_HOME/Makefile.in
cp $APP_HOME/debian/control.in $APP_HOME/debian/control
sed -e "s/@DEVEL_VERSION@/1.99.9/" -e "s/@DEVEL_RELEASE@/1/" \
  $APP_HOME/debian/changelog.in \
  > $APP_HOME/debian/changelog

cd $APP_HOME
dpkg-buildpackage -us -uc -b

$APP_HOME/debian/rules clean
rm $APP_HOME/debian/changelog
rm $APP_HOME/debian/control
unlink $APP_HOME/debian
