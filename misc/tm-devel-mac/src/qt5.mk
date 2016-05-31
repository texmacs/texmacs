# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# Qt
PKG             := qt5
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 5.6.0
$(PKG)_CHECKSUM := 4f111a4d6bb90eaed024b857b1bd3d0731ace8a2
$(PKG)_SUBDIR   := qt-everywhere-opensource-src-$($(PKG)_VERSION)
$(PKG)_FILE     := qt-everywhere-opensource-src-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://qt.nokia.com/
$(PKG)_URL      := http://download.qt.io/official_releases/qt/5.6/5.6.0/single/$($(PKG)_FILE)
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
        -prefix '$(PREFIX)'  \
        -release \
        -opensource \
        -confirm-license \
        -no-c++11 \
        -platform macx-clang-32 \
        -shared \
        -nomake examples \
        -nomake tools \
        -make libs \
        -no-rpath \
        -v


#          -accessibility \
#          -no-sql-db2 -no-sql-ibase -no-sql-mysql -no-sql-oci \
#  		-no-sql-odbc -no-sql-psql -no-sql-sqlite -no-sql-sqlite2 \
#  		-no-sql-tds \
#  		-no-qml-debug \
#          -force-pkg-config \
#          -system-zlib \
#  		-system-freetype \
#          -openssl \
#          -qt-libpng \
#          -qt-libjpeg \
#          -no-glib \
#  		-no-compile-examples \
#          -no-rpath \
#          -no-opengl \
#          -no-gstreamer \
#          -no-dbus \
#          -no-reduce-exports \
        
#        -arch '$(BUILD_ARCH)' \
#		-sdk macosx10.6  \

    $(MAKE) -C '$(1)' -j '$(JOBS)'
    rm -rf '$(PREFIX)/mkspecs'
    $(MAKE) -C '$(1)' -j 1 install
endef
