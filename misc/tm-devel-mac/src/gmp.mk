# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# GMP
PKG             := gmp
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 5.0.2
$(PKG)_CHECKSUM := 2968220e1988eabb61f921d11e5d2db5431e0a35
$(PKG)_SUBDIR   := gmp-$($(PKG)_VERSION)
$(PKG)_FILE     := gmp-$($(PKG)_VERSION).tar.bz2
$(PKG)_WEBSITE  := http://www.gmplib.org/
$(PKG)_URL      := https://gmplib.org/download/gmp/$($(PKG)_FILE)
$(PKG)_URL_2    := ftp://ftp.cs.tu-berlin.de/pub/gnu/$(PKG)/$($(PKG)_FILE)
$(PKG)_DEPS     :=

GMP_ABI_i386    := 32
GMP_ABI_ppc     := 32
GMP_ABI_x86_64  := 64

define $(PKG)_UPDATE
    curl -s -L 'http://www.gmplib.org/' | \
    grep '<a href="' | \
    $(SED) -n 's,.*gmp-\([0-9][^>]*\)\.tar.*,\1,p' | \
    grep -v '^4\.' | \
    head -1
endef


define $(PKG)_BUILD
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef


define $(PKG)_BUILD_ARCH
    cd '$(1)' && '$(1)'/configure \
        $(CONFIGURE_HOST) \
        --prefix='$(PREFIX)' \
        --disable-shared \
        --without-readline \
	    CC='gcc $(BASE_FLAGS)' CXX='g++ $(BASE_FLAGS)' \
        CPP='gcc -E $(BASE_FLAGS)' CXXCPP='g++ -E $(BASE_FLAGS)' \
        CFLAGS='$(BASE_CFLAGS)' LDFLAGS='$(BASE_LDFLAGS)' \
        NM='$(DEVTOOLS)/usr/bin/nm -p' \
        ABI='$(GMP_ABI_$(3))'
    $(MAKE) -C '$(1)' -j '$(JOBS)'
    $(MAKE) -C '$(1)' -j 1 install
endef
