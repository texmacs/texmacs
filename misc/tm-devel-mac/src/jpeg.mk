# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

PKG             := jpeg
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 8d
$(PKG)_CHECKSUM := f080b2fffc7581f7d19b968092ba9ebc234556ff
$(PKG)_SUBDIR   := jpeg-$($(PKG)_VERSION)
$(PKG)_FILE     := jpegsrc.v$($(PKG)_VERSION).tar.gz
$(PKG)_URL      := http://www.ijg.org/files/$($(PKG)_FILE)
$(PKG)_DEPS     := 

define $(PKG)_UPDATE
    curl -s -L  'http://www.ijg.org/' | \
    $(SED) -n 's,.*jpegsrc\.v\([0-9][^>]*\)\.tar.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef


define $(PKG)_BUILD_ARCH
    cd '$(1)' && ./configure \
        $(CONFIGURE_HOST) \
        --prefix='$(PREFIX)' \
        --disable-shared \
	    CC='gcc $(BASE_FLAGS)' CXX='g++ $(BASE_FLAGS)' \
        CPP='gcc -E $(BASE_FLAGS)' CXXCPP='g++ -E $(BASE_FLAGS)' \
        CFLAGS='$(BASE_CFLAGS)' LDFLAGS='$(BASE_LDFLAGS)' 
    $(MAKE) -C '$(1)' -j '$(JOBS)' install bin_PROGRAMS= sbin_PROGRAMS= noinst_PROGRAMS= man_MANS=
endef

