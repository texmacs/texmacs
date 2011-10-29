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
    wget -q -O- 'http://sourceforge.net/projects/freetype/files/freetype2/' | \
    $(SED) -n 's,.*/\([0-9][^"]*\)/".*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
   $(foreach BUILD_ARCH,$(BUILD_ARCHS),
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH)))
endef


define $(PKG)_BUILD_ARCH
    [ -d '$(1)/../$(3)' ] || mkdir -p '$(1)/../$(3)'
    cd '$(1)/../$(3)' && GNUMAKE=$(MAKE) '$(1)'/configure \
        --host='$(TARGET_$(3))' \
        --disable-shared \
        --prefix='$(PREFIX)/$(3)' \
        CC="gcc-4.2 -arch $(3) -mmacosx-version-min=10.4 "\
        CXX="g++-4.2 -arch $(3) -mmacosx-version-min=10.4 "\
        CPP="cpp-4.2"\
        CXXCPP="cpp-4.2" \
        CPPFLAGS=" -I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        CFLAGS=" -I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        CXXFLAGS="-I$(PREFIX)/$(3)/include -I$(PREFIX)/include"\
        LDFLAGS="-L$(PREFIX)/$(3)/lib -L$(PREFIX)/lib" 
    $(MAKE) -C '$(1)/../$(3)' -j '$(JOBS)' install
endef
