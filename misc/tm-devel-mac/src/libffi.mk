# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# libffi
PKG             := libffi
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 3.0.9
$(PKG)_CHECKSUM := 56e41f87780e09d06d279690e53d4ea2c371ea88
$(PKG)_SUBDIR   := $(PKG)-$($(PKG)_VERSION)
$(PKG)_FILE     := $(PKG)-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://sourceware.org/$(PKG)/
$(PKG)_URL      := ftp://sourceware.org/pub/$(PKG)/$($(PKG)_FILE)
$(PKG)_DEPS     := 
#gcc

define $(PKG)_UPDATE
    curl -s -L  'http://github.com/atgreen/libffi/downloads' | \
    grep '<a href="/atgreen/libffi/tarball/' | \
    $(SED) -n 's,.*href="/atgreen/libffi/tarball/v\([0-9][^"]*\)".*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
   $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef

define $(PKG)_BUILD_ARCH
    cd '$(1)' && ./configure \
        --host='$(TARGET_$(3))' \
        --prefix='$(PREFIX)' \
        --disable-shared
    $(MAKE) -C '$(1)' -j '$(JOBS)'
    $(MAKE) -C '$(1)' -j 1 install
endef
