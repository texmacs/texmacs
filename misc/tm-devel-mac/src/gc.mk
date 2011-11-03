# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# gc
PKG             := gc
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 7.1
$(PKG)_CHECKSUM := e84cba5d18f4ea5ed4e5fd3f1dc6a46bc190ff6f
$(PKG)_SUBDIR   := $(PKG)-$($(PKG)_VERSION)
$(PKG)_FILE     := $(PKG)-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://www.hpl.hp.com/personal/Hans_Boehm/$(PKG)/
$(PKG)_URL      := http://www.hpl.hp.com/personal/Hans_Boehm/$(PKG)/$(PKG)_source/$($(PKG)_FILE)
$(PKG)_DEPS     := 
#gcc

define $(PKG)_UPDATE
    curl -s -L 'http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/?C=M;O=D' | \
    grep '<a href="gc-' | \
    $(SED) -n 's,.*<a href="gc-\([0-9][^"]*\)\.tar.*,\1,p' | \
    grep -v 'alpha' | \
    head -1
endef

define $(PKG)_BUILD
   $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef

define $(PKG)_BUILD_ARCH
    cd '$(1)' && ./configure \
        --host='$(TARGET_$(3))' \
        --prefix='$(PREFIX)' \
        --disable-shared \
        --enable-cplusplus
#        --enable-threads=win32 \
    $(MAKE) -C '$(1)' -j '$(JOBS)'
    $(MAKE) -C '$(1)' -j 1 install
endef
