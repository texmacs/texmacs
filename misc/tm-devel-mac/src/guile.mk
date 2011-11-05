# This file is part of tm-devel-mac.
# Adapted from the mingw-cross-env project.
# See doc/index.html for further information.

# GNU Guile
PKG             := guile
$(PKG)_IGNORE   :=
$(PKG)_VERSION  := 1.8.8
$(PKG)_CHECKSUM := 548d6927aeda332b117f8fc5e4e82c39a05704f9
#$(PKG)_VERSION  := 1.8.7
#$(PKG)_CHECKSUM := 24cd2f06439c76d41d982a7384fe8a0fe5313b54
$(PKG)_SUBDIR   := $(PKG)-$($(PKG)_VERSION)
$(PKG)_FILE     := $(PKG)-$($(PKG)_VERSION).tar.gz
$(PKG)_WEBSITE  := http://www.gnu.org/software/$(PKG)/
$(PKG)_URL      := http://ftp.gnu.org/gnu/$(PKG)/$($(PKG)_FILE)
$(PKG)_DEPS     := gmp gettext libtool libiconv libffi readline
#gcc libtool gmp libiconv gettext libunistring gc libffi readline

# these options prevents some misbehaviour in configure due to cross-compilation to ppc from i386
MORE_OPT_ppc := ac_cv_type_getgroups=gid_t ac_cv_type_setgroups=gid_t

define $(PKG)_UPDATE
    curl -s -L 'http://git.savannah.gnu.org/gitweb/?p=$(PKG).git;a=tags' | \
    grep '<a class="list subject"' | \
    $(SED) -n 's,.*<a[^>]*>[^0-9>]*\([0-9][^< ]*\)\.<.*,\1,p' | \
    head -1
endef

define $(PKG)_BUILD
     $(call $(3)_BUILD_ARCH,$(1),$(2),$(BUILD_ARCH))
endef

define $(PKG)_BUILD_ARCH
    # The setting "scm_cv_struct_timespec=no" ensures that Guile
    # won't try to use the "struct timespec" from <pthreads.h>,
    # which would fail because we tell Guile not to use Pthreads.
#        scm_cv_struct_timespec=no \
#        --build='$(HOST)' \

    cd '$(1)' && \
        PKG_CONFIG_PATH='$(PREFIX)/lib/pkgconfig/' \
        ./configure \
        --host='$(TARGET_$(3))' \
        --prefix='$(PREFIX)' \
        --disable-shared \
        --without-threads \
	    --with-sysroot='$(MACOS_SDK)'  \
	    CC='gcc $(BASE_FLAGS)' CXX='g++ $(BASE_FLAGS)' \
        CPP='gcc -E $(BASE_FLAGS)' CXXCPP='g++ -E $(BASE_FLAGS)' \
        CFLAGS='$(BASE_CFLAGS)' LDFLAGS='$(BASE_LDFLAGS)' \
        $(MORE_OPT_$(3))

#    $(MAKE) -C '$(1)' -j '$(JOBS)' schemelib_DATA=
#    $(MAKE) -C '$(1)' -j 1 install schemelib_DATA=
    $(MAKE) -C '$(1)' -j '$(JOBS)' 
    $(MAKE) -C '$(1)' -j 1 install 

#         LIBS='-lunistring -lintl -liconv' \

#    '$(TARGET)-gcc' \
#        -W -Wall -Werror -ansi -pedantic \
#        '$(2).c' -o '$(PREFIX)/$(TARGET)/bin/test-guile.exe' \
#        `'$(TARGET)-pkg-config' guile-1.8 --cflags --libs`
endef
