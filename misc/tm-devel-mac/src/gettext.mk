# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# gettext
PKG             := gettext
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 0.18.1.1
$(PKG)_CHECKSUM := 5009deb02f67fc3c59c8ce6b82408d1d35d4e38f
$(PKG)_SUBDIR   := gettext-$($(PKG)_VERSION)
$(PKG)_FILE     := gettext-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://www.gnu.org/software/gettext/
$(PKG)_URL      := ftp://ftp.gnu.org/pub/gnu/gettext/$($(PKG)_FILE)
$(PKG)_DEPS     := 
#gcc libiconv

define $(PKG)_UPDATE
    wget -q -O- 'http://www.gnu.org/software/gettext/' | \
    grep 'gettext-' | \
    $(SED) -n 's,.*gettext-\([0-9][^>]*\)\.tar.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
    cd '$(1)/gettext-runtime' && ./configure \
        --host='$(TARGET)' \
        --disable-shared \
        --prefix='$(PREFIX)/$(TARGET)' \
        --enable-threads=win32 \
        --without-libexpat-prefix \
        --without-libxml2-prefix \
        CONFIG_SHELL=$(SHELL)
    $(MAKE) -C '$(1)/gettext-runtime/intl' -j '$(JOBS)' install
endef


define $(PKG)_BUILD
   $(foreach BUILD_ARCH,$(BUILD_ARCHS),
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH)))
endef


define $(PKG)_BUILD_ARCH
    [ -d '$(1)/../$(3)' ] || mkdir -p '$(1)/../$(3)'
    cd '$(1)/../$(3)' && '$(1)'/gettext-runtime/configure\
        --host=$(TARGET_$(3)) \
        NM='/usr/bin/nm -p' \
        CC="gcc-4.2 -arch $(3) -mmacosx-version-min=10.4 "\
        CXX="g++-4.2 -arch $(3) -mmacosx-version-min=10.4 "\
        CPP="cpp-4.2"\
        CXXCPP="cpp-4.2" \
        CPPFLAGS=" -I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        CFLAGS=" -I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        CXXFLAGS="-I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        LDFLAGS="-L$(PREFIX)/$(3)/lib -L$(PREFIX)/lib" \
        --prefix='$(PREFIX)/$(3)' \
        --disable-shared \
        --enable-threads \
        --without-libexpat-prefix \
        --without-libxml2-prefix \
        CONFIG_SHELL=$(SHELL)
    $(MAKE) -C '$(1)/../$(3)/intl' -j '$(JOBS)' install
endef
