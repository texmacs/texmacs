# syntax=docker/dockerfile:1

FROM ubuntu:noble AS deps

RUN apt-get update && \
	DEBIAN_FRONTEND=noninteractive apt-get install -y \
	build-essential \
	ccache \
	autoconf \
	automake \
	libtool \
	pkg-config \
	cmake \
	ninja-build \
	git \
	rsync \
	vim \
	gettext \
	texinfo \
	libx11-dev \
	libxpm-dev \
	libxext-dev \
	libpng-dev \
	libfreetype6-dev \
	libgmp-dev \
	libnettle8 \
	libhogweed6 \
	libgnutls28-dev \
	libxcb1-dev \
	libxml2-dev \
	libxkbcommon-dev \
	libfontconfig1-dev \
	ghostscript \
	meson \
	zlib1g \
	qt6-base-dev \
	qt6-base-dev-tools \
	qt6-tools-dev \
	qt6-tools-dev-tools \
	qt6-svg-dev \
	libqt6core6 \
	libqt6gui6 \
	libqt6widgets6 \
	libqt6svg6 \
	libqt6printsupport6 \
	libqt6network6 \
	&& \
	apt-get clean && rm -rf /var/lib/apt/lists/*

# ---------------------------------------------------------------------------
# Builder stage
# ---------------------------------------------------------------------------
FROM deps AS builder

ARG UID=1000
ARG GID=1000

RUN install --verbose --directory --owner ubuntu --group ubuntu --mode 755 /opt/texmacs

USER ubuntu

# Copy TeXmacs source from the build context (current directory = src/).
# Exclude build artefacts so rebuilds do not drag in stale objects.
COPY --chown=ubuntu:ubuntu \
	--exclude=*.o \
	--exclude=*.so \
	--exclude=*.a \
	--exclude=tm-guile188 \
	. /opt/texmacs/src/

# Copy guile-texmacs from the named build context.
# Provide it with: --build-context guile-src=../guile-texmacs
COPY --chown=ubuntu:ubuntu --from=guile-src . /opt/texmacs/src/tm-guile188/

ENV QMAKE=/usr/lib/qt6/bin/qmake6

WORKDIR /opt/texmacs/src

RUN autoreconf -fi \
	&& ./configure --with-gnutls=yes --with-guile=embedded18 \
	&& make clean \
	&& make CXX='ccache g++' CC='ccache gcc' -j$(nproc)

# ---------------------------------------------------------------------------
# Runtime stage
# ---------------------------------------------------------------------------
FROM ubuntu:noble

RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
		iproute2 \
		netcat-openbsd \
		gnupg \
		mailutils \
	; \
	rm -rf /var/lib/apt/lists/*

RUN set -eux; \
	groupadd -r -g 999 texmacs-server; \
	useradd -r -g texmacs-server -u 999 \
		--home-dir=/srv/texmacs-server \
		--shell=/bin/bash \
		texmacs-server; \
	install --verbose --directory --owner texmacs-server --group texmacs-server --mode 1777 /srv/texmacs-server

ENV GOSU_VERSION=1.17
RUN set -eux; \
	savedAptMark="$(apt-mark showmanual)"; \
	apt-get update; \
	apt-get install -y --no-install-recommends ca-certificates wget; \
	rm -rf /var/lib/apt/lists/*; \
	dpkgArch="$(dpkg --print-architecture | awk -F- '{ print $NF }')"; \
	wget -O /usr/local/bin/gosu "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch"; \
	wget -O /usr/local/bin/gosu.asc "https://github.com/tianon/gosu/releases/download/$GOSU_VERSION/gosu-$dpkgArch.asc"; \
	export GNUPGHOME="$(mktemp -d)"; \
	gpg --batch --keyserver hkps://keys.openpgp.org --recv-keys B42F6819007F00F88E364FD4036A9C25BF357DD4; \
	gpg --batch --verify /usr/local/bin/gosu.asc /usr/local/bin/gosu; \
	gpgconf --kill all; \
	rm -rf "$GNUPGHOME" /usr/local/bin/gosu.asc; \
	apt-mark auto '.*' > /dev/null; \
	[ -z "$savedAptMark" ] || apt-mark manual $savedAptMark > /dev/null; \
	apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false; \
	chmod +x /usr/local/bin/gosu; \
	gosu --version; \
	gosu nobody true

RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
		libqt6core6 \
		libqt6gui6 \
		libqt6widgets6 \
		libqt6svg6 \
		libqt6printsupport6 \
		libqt6network6 \
		libltdl7 \
		libfreetype6 \
		libc6 \
		libcrypt1 \
		libgcc-s1 \
		libgmp10 \
		libpng16-16 \
		libstdc++6 \
		zlib1g \
		ghostscript \
		findutils \
		fonts-stix \
		fonts-texgyre \
		libgnutls30 \
	; \
	rm -rf /var/lib/apt/lists/*

RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends vim; \
	rm -rf /var/lib/apt/lists/*

RUN set -eux; \
	mkdir -p \
		/usr/local/share/TeXmacs \
		/usr/local/libexec/TeXmacs/bin \
		/usr/local/libexec/TeXmacs/lib \
		/srv/texmacs-server/.TeXmacs/system \
		/srv/texmacs-server/.TeXmacs/server

COPY --from=builder /opt/texmacs/src/TeXmacs/progs          /usr/local/share/TeXmacs/progs/
COPY --from=builder /opt/texmacs/src/tm-guile188/ice-9      /usr/local/share/TeXmacs/progs/ice-9/
COPY --from=builder /opt/texmacs/src/TeXmacs/fonts          /usr/local/share/TeXmacs/fonts/
COPY --from=builder /opt/texmacs/src/TeXmacs/plugins        /usr/local/share/TeXmacs/plugins
COPY --from=builder /opt/texmacs/src/TeXmacs/styles         /usr/local/share/TeXmacs/styles/
COPY --from=builder /opt/texmacs/src/TeXmacs/texts          /usr/local/share/TeXmacs/texts/
COPY --from=builder /opt/texmacs/src/TeXmacs/langs          /usr/local/share/TeXmacs/langs/
COPY --from=builder /opt/texmacs/src/TeXmacs/packages       /usr/local/share/TeXmacs/packages/
COPY --from=builder /opt/texmacs/src/TeXmacs/bin/texmacs.bin /usr/local/libexec/TeXmacs/bin/

COPY --from=builder /opt/texmacs/src/misc/scripts/texmacs          /usr/local/bin/
COPY --from=builder /opt/texmacs/src/LICENSE                        /usr/local/share/TeXmacs/LICENSE
COPY --from=builder /opt/texmacs/src/misc/docker/docker-entrypoint.sh /usr/local/bin/
COPY --from=builder /opt/texmacs/src/misc/docker/server-preferences.scm /srv/texmacs-server/.TeXmacs/system/preferences.scm
COPY --from=builder /opt/texmacs/src/misc/docker/mail.rc            /etc/mail.rc
COPY --from=builder /opt/texmacs/src/misc/docker/license.tm         /srv/texmacs-server/.TeXmacs/server/license.tm

RUN set -eux; \
	chmod +x /usr/local/bin/texmacs; \
	chmod +x /usr/local/bin/docker-entrypoint.sh

RUN set -eux; \
	chown -R texmacs-server:texmacs-server /srv/texmacs-server/

ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]

EXPOSE 6561
CMD ["texmacs"]
