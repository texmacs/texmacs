# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# GS
PKG             := gs
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 9.06
$(PKG)_CHECKSUM := a3de8ccb877ee9b7437a598196eb6afa11bf31dc
$(PKG)_SUBDIR   := ghostscript-$($(PKG)_VERSION)
$(PKG)_FILE     := ghostscript-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://www.ghostscript.com/
$(PKG)_URL      := http://downloads.ghostscript.com/public/$($(PKG)_FILE)
$(PKG)_URL_2    := http://downloads.ghostscript.com/public/$($(PKG)_FILE)
$(PKG)_DEPS     := 

GMP_ABI_i386    := 32
GMP_ABI_ppc     := 32
GMP_ABI_x86_64  := 64

define $(PKG)_UPDATE
    curl -s -L 'http://www.ghostscript.com/download/gsdnld.html' | \
    grep '<a href="' | \
    $(SED) -n 's,.*ghostscript-\([0-9][^>]*\)\.tar\.gz.*,\1,p' | \
    grep -v '^4\.' | \
    head -1
endef

define $(PKG)_BUILD
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef


define $(PKG)_BUILD_ARCH
    cd '$(1)' && '$(1)'/configure\
        --host='$(TARGET_$(3))' \
        NM='$(DEVTOOLS)/usr/bin/nm -p' \
        --prefix='$(PREFIX)' \
        --disable-shared \
        --without-x \
	    CC='gcc $(BASE_FLAGS)' CXX='g++ $(BASE_FLAGS)' \
        CPP='gcc -E $(BASE_FLAGS)' CXXCPP='g++ -E $(BASE_FLAGS)' \
        CFLAGS='$(BASE_CFLAGS)' LDFLAGS='$(BASE_LDFLAGS)' \
        ABI='$(GMP_ABI_$(3))' 
    $(MAKE) -C '$(1)' -j '$(JOBS)'
    $(MAKE) -C '$(1)' -j 1 install
endef
