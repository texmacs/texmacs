# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# gettext
PKG             := pkg-config
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 0.28
$(PKG)_CHECKSUM := 71853779b12f958777bffcb8ca6d849b4d3bed46
$(PKG)_SUBDIR   := pkg-config-$($(PKG)_VERSION)
$(PKG)_FILE     := pkg-config-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://www.freedesktop.org/wiki/Software/pkg-config
$(PKG)_URL      := http://pkgconfig.freedesktop.org/releases/$($(PKG)_FILE)
$(PKG)_DEPS     := 
#gcc libiconv

define $(PKG)_UPDATE
    curl -s -L 'http://www.gnu.org/software/gettext/' | \
    grep 'gettext-' | \
    $(SED) -n 's,.*gettext-\([0-9][^>]*\)\.tar.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
    cd '$(1)' && ./configure \
        --prefix='$(PREFIX)' \
        --with-internal-glib
    $(MAKE) -C '$(1)' -j 1 install
endef


