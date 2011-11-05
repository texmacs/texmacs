# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# freetype
PKG             := freetype
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 2.4.7
$(PKG)_CHECKSUM := e1b2356ebbc6d39d813797572b1e5d8a2635e969
$(PKG)_SUBDIR   := freetype-$($(PKG)_VERSION)
$(PKG)_FILE     := freetype-$($(PKG)_VERSION).tar.bz2
$(PKG)_WEBSITE  := http://freetype.sourceforge.net/
$(PKG)_URL      := http://download.savannah.gnu.org/releases/freetype/$($(PKG)_FILE)
$(PKG)_DEPS     := 

define $(PKG)_UPDATE
    curl -s -L 'http://sourceforge.net/projects/freetype/files/freetype2/' | \
    $(SED) -n 's,.*/\([0-9][^"]*\)/".*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
   $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef


define $(PKG)_BUILD_ARCH
    cd '$(1)' && GNUMAKE=$(MAKE) '$(1)'/configure \
        --host='$(TARGET_$(3))' \
        --disable-shared \
        --prefix='$(PREFIX)' \
	    CC='gcc $(BASE_FLAGS)' CXX='g++ $(BASE_FLAGS)' \
        CPP='gcc -E $(BASE_FLAGS)' CXXCPP='g++ -E $(BASE_FLAGS)' \
        CFLAGS='$(BASE_CFLAGS)' LDFLAGS='$(BASE_LDFLAGS)' 
    $(MAKE) -C '$(1)' -j '$(JOBS)' 
    $(MAKE) -C '$(1)' -j 1 install
endef
