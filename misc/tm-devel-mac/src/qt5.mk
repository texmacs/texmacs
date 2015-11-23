# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# Qt
PKG             := qt5
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 5.5.1
$(PKG)_CHECKSUM := 3d7e7805d849bcf6cf88788bc83aeb334e1cd875
$(PKG)_SUBDIR   := qt-everywhere-opensource-src-$($(PKG)_VERSION)
$(PKG)_FILE     := qt-everywhere-opensource-src-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://qt.nokia.com/
$(PKG)_URL      := http://download.qt.io/archive/qt/5.5/5.5.1/single/$($(PKG)_FILE)
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
        -arch '$(BUILD_ARCH)' \
      -shared \
        -v
        
#		-sdk macosx10.6  \
#         -accessibility \
#         -no-sql-db2 -no-sql-ibase -no-sql-mysql -no-sql-oci \
# 		-no-sql-odbc -no-sql-psql -no-sql-sqlite -no-sql-sqlite2 \
# 		-no-sql-tds \
# 		-no-qml-debug \
#         -force-pkg-config \
#         -system-zlib \
# 		-system-freetype \
#         -openssl \
#         -qt-libpng \
#         -qt-libjpeg \
#         -no-glib \
#         -nomake examples \
#         -nomake tools \
#         -make libs \
# 		-no-compile-examples \
#         -no-rpath \
#         -no-opengl \
#         -no-gstreamer \
#         -no-dbus \
#         -no-reduce-exports \
#         -v 

    $(MAKE) -C '$(1)' -j '$(JOBS)'
    rm -rf '$(PREFIX)/mkspecs'
    $(MAKE) -C '$(1)' -j 1 install
endef
