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
    curl -s -L 'http://www.gnu.org/software/gettext/' | \
    grep 'gettext-' | \
    $(SED) -n 's,.*gettext-\([0-9][^>]*\)\.tar.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
   $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef


define $(PKG)_BUILD_ARCH
    cd '$(1)' && '$(1)'/gettext-runtime/configure\
        NM='$(DEVTOOLS)/usr/bin/nm -p' \
        --host='$(TARGET_$(3))' \
        --prefix='$(PREFIX)' \
        --disable-shared \
        --enable-threads \
        --without-libexpat-prefix \
        --without-libxml2-prefix \
	    CC='gcc $(BASE_FLAGS)' CXX='g++ $(BASE_FLAGS)' \
        CPP='gcc -E $(BASE_FLAGS)' CXXCPP='g++ -E $(BASE_FLAGS)' \
        CFLAGS='$(BASE_CFLAGS)' LDFLAGS='$(BASE_LDFLAGS)' \
        CONFIG_SHELL=$(SHELL)
    $(MAKE) -C '$(1)/intl' -j '$(JOBS)' 
    $(MAKE) -C '$(1)/intl' -j 1 install
endef
