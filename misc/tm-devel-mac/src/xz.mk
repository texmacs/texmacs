# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

PKG             := xz
$(PKG)_IGNORE   := 
$(PKG)_VERSION   := 5.0.4
$(PKG)_CHECKSUM := 3e976d7715fde43422572c70f927bfdae56a94c3
$(PKG)_SUBDIR   := $(PKG)-$($(PKG)_VERSION)
$(PKG)_FILE     := $(PKG)-$($(PKG)_VERSION).tar.gz
$(PKG)_URL      := http://tukaani.org/xz/$($(PKG)_FILE)
$(PKG)_DEPS     := 

define $(PKG)_UPDATE
    curl -s -L  'http://tukaani.org/xz/' | \
    $(SED) -n 's,.*xz-\([0-9][^>]*\)\.tar.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef


define $(PKG)_BUILD_ARCH
   cd '$(1)' && ./configure \
        $(CONFIGURE_HOST) \
        --build="`config.guess`" \
        --prefix='$(PREFIX)' \
        --disable-threads \
        --disable-nls \
        --disable-shared \
	    CC='gcc $(BASE_FLAGS)' CXX='g++ $(BASE_FLAGS)' \
        CPP='gcc -E $(BASE_FLAGS)' CXXCPP='g++ -E $(BASE_FLAGS)' \
        CFLAGS='$(BASE_CFLAGS)' LDFLAGS='$(BASE_LDFLAGS)' 
    $(MAKE) -C '$(1)'/src/liblzma -j '$(JOBS)' install
endef

