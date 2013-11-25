# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# Qt
PKG             := qt
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 4.8.5
$(PKG)_CHECKSUM := 745f9ebf091696c0d5403ce691dc28c039d77b9e
$(PKG)_SUBDIR   := $(PKG)-everywhere-opensource-src-$($(PKG)_VERSION)
$(PKG)_FILE     := $(PKG)-everywhere-opensource-src-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://qt.nokia.com/
$(PKG)_URL      := http://download.qt-project.org/official_releases/qt/4.8/4.8.5/$($(PKG)_FILE)
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
        -qt-libpng \
        -qt-libmng \
        -openssl \
        -no-sql-db2 -no-sql-ibase -no-sql-mysql -no-sql-oci \
		-no-sql-odbc -no-sql-psql -no-sql-sqlite -no-sql-sqlite2 \
		-no-sql-sqlite_symbian -no-sql-tds \
        -arch '$(BUILD_ARCH)' \
        -v
        
#		-sdk '$(MACOS_SDK)'  \
#        -qt-libtiff \
#        -qt-libjpeg \

    $(MAKE) -C '$(1)' -j '$(JOBS)'
    rm -rf '$(PREFIX)/mkspecs'
    $(MAKE) -C '$(1)' -j 1 install
endef
