# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# Qt
PKG             := qt
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 4.7.4
$(PKG)_CHECKSUM := af9016aa924a577f7b06ffd28c9773b56d74c939
$(PKG)_SUBDIR   := $(PKG)-everywhere-opensource-src-$($(PKG)_VERSION)
$(PKG)_FILE     := $(PKG)-everywhere-opensource-src-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://qt.nokia.com/
$(PKG)_URL      := http://get.qt.nokia.com/qt/source/$($(PKG)_FILE)
$(PKG)_DEPS     := 
#gcc libodbc++ postgresql freetds openssl libgcrypt zlib libpng jpeg libmng tiff sqlite libiconv

define $(PKG)_UPDATE
    curl -s -L  'http://qt.gitorious.org/qt/qt/commits' | \
    grep '<li><a href="/qt/qt/commit/' | \
    $(SED) -n 's,.*<a[^>]*>v\([0-9][^<-]*\)<.*,\1,p' | \
    tail -1
endef

define $(PKG)_BUILD
    cd '$(1)' && ./configure \
        -opensource \
        -confirm-license \
        -fast \
        -force-pkg-config \
        -release \
        -no-exceptions \
        -shared \
        -prefix '$(PREFIX)'  \
        -prefix-install \
        -no-script \
        -no-opengl \
        -no-webkit \
        -no-glib \
        -no-gstreamer \
        -no-phonon \
        -no-phonon-backend \
        -no-dbus \
        -no-qt3support \
        -no-javascript-jit \
        -no-scripttools \
        -no-declarative \
        -no-xmlpatterns \
        -no-stl \
        -no-accessibility \
        -no-reduce-exports \
        -no-rpath \
        -make libs \
        -nomake demos \
        -nomake docs \
        -nomake examples \
        -nomake tools \
        -system-zlib \
        -openssl \
        -arch "x86_64 x86" \
		-sdk /Developer/SDKs/MacOSX10.5.sdk \
        -v

    $(MAKE) -C '$(1)' -j '$(JOBS)'
    rm -rf '$(PREFIX)/mkspecs'
    $(MAKE) -C '$(1)' -j 1 install
endef
