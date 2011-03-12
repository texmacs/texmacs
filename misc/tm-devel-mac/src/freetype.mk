# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# freetype
PKG             := freetype
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 2.4.4
$(PKG)_CHECKSUM := 1d136cbc51c67b212c91ba04dc5db797f35e64e6
$(PKG)_SUBDIR   := freetype-$($(PKG)_VERSION)
$(PKG)_FILE     := freetype-$($(PKG)_VERSION).tar.bz2
$(PKG)_WEBSITE  := http://freetype.sourceforge.net/
$(PKG)_URL      := http://$(SOURCEFORGE_MIRROR)/project/freetype/freetype2/$($(PKG)_VERSION)/$($(PKG)_FILE)
$(PKG)_DEPS     := 

define $(PKG)_UPDATE
    wget -q -O- 'http://sourceforge.net/projects/freetype/files/freetype2/' | \
    $(SED) -n 's,.*/\([0-9][^"]*\)/".*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
    [ -d '$(1)/../$(3)' ] || mkdir -p '$(1)/../$(3)'
    cd '$(1)/../$(3)' && GNUMAKE=$(MAKE) '$(1)'/configure \
        --host='$(TARGET_$(3))' \
        --disable-shared \
        --prefix='$(PREFIX)/$(3)' \
        CPPFLAGS="-arch $(3) -mmacosx-version-min=10.4  -I$PREFIX/include"\
        CFLAGS="-arch $(3) -mmacosx-version-min=10.4  -I$PREFIX/include"\
        CXXFLAGS="-arch $(3) -mmacosx-version-min=10.4  -I$PREFIX/include"\
        LDFLAGS="-arch $(3) -mmacosx-version-min=10.4  -L$PREFIX/lib" 
    $(MAKE) -C '$(1)/../$(3)' -j '$(JOBS)' install
endef
