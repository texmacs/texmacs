# Deploying server

## Install texmacs

Use the `.deb` package to install on an Ubuntu server.

## Server Architecture

A dedicated user and group `texmacs-server:texmacs-server` is used, with
`/srv/texmacs-server` as home directory.

It can be created like this:
```bash
	groupadd -r texmacs-server; \
	useradd -r -g texmacs-server \
		--home-dir=/srv/texmacs-server \
		--shell=/bin/bash \
		texmacs-server; \
	install --verbose --directory --owner texmacs-server --group texmacs-server --mode 0755 /srv/texmacs-server
```

The texmacs server binary is executed by systemd, with the port as unit
parameter:

`/usr/lib/systemd/system/texmacs-server@.service`:
```ini
[Unit]
Description=TeXmacs server on specific port
After=network.target

[Service]
User=texmacs-server
Group=texmacs-server
ExecStart=texmacs --server --headless --port %i
Restart=on-failure
Type=exec

[Install]
WantedBy=multi-user.target
```

### Enable the service (started at boot)
Default port is 6561, but you can specify one with `@port`: `systemctl enable texmacs-server@6562.service`

HAProxy can listen on 6561 and forward to the actual texmacs-server, with some DoS protection (`/etc/haproxy/haproxy.cfg`):
```ini
listen texmacs
        timeout connect <D1>s
        timeout client  <D2>m
        timeout server  <D3>m
        maxconn <N>
        bind   *:6561
        server app_backend 127.0.0.1:6562 maxconn <M> check
```

### Certificates

Renewal is automated with certbot, but the certificate files need to be owned by
`texmacs-server:texmacs-server`. This can be done with a deploy renewal hook:

`/etc/letsencrypt/renewal-hooks/deploy/texmacs-server.sh`:
```bash
#! /bin/bash

DOMAIN=$(basename "$RENEWED_LINEAGE")
echo "Executing deploy hooks for $DOMAIN"

if [ "$DOMAIN" = "cloud.texmacs.org" ]; then
        chown texmacs-server:texmacs-server $RENEWED_LINEAGE/privkey*.pem $RENEWED_LINEAGE/cert*.pem
        systemctl restart texmacs-server@6562.service
fi
```

## Deploy new versions with bind mount

Deploying new versions of the TeXmacs server with bind mounts has these advantages:
- A full install every time is not needed
- Older versions can be kept, and swapped with a simple symbolic link

New uploads are bind-mounted over the `/usr/local` TeXmacs installation, to "override" the default installation with the current one


### Systemd

The bind mounts are done with systemd as well, with these two units:

`/etc/systemd/system/usr-local-libexec-TeXmacs-bin-texmacs.bin.mount`
```ini
[Unit]
Conflicts=umount.target

[Mount]
Where=/usr/local/libexec/TeXmacs/bin/texmacs.bin
What=/srv/texmacs-server/current/bin/texmacs.bin
Type=none
Options=bind,uid=111,gid=116

[Install]
WantedBy=default.target
```

`/etc/systemd/system/usr-local-share-TeXmacs.mount`
```ini
[Unit]
Conflicts=umount.target

[Mount]
Where=/usr/local/share/TeXmacs/
What=/srv/texmacs-server/current
Type=none
Options=bind,uid=111,gid=116

[Install]
WantedBy=default.target
```

**Make sure to replace the uid/gid with the correct ones for texmacs-server:texmacs-server**

Enable the mount units (you will need a `/srv/texmacs-server/current` deployment or it might not work, see [Build and upload](#build-and-upload) below):
```bash
$ sudo systemctl enable usr-local-share-TeXmacs.mount usr-local-libexec-TeXmacs-bin-texmacs.bin.mount
```

### Build and upload

The `misc/scripts/deploy-server.sh` script helps build, check and upload the needed binaries to the server.

Artifacts can be built by the script, or given to it with `-a/--artifacts-dir`. They should either
be the result of a docker build (see DOCKER.md and Dockerfile) or of a CI build.

The script takes the following files and directories from a standard TeXmacs build (the `TeXmacs` directory in the main texmacs `src` dir, which is usually a dev's TEXMACS_PATH):
- `$TEXMACS_PATH/progs`
- `$TEXMACS_PATH/fonts`
- `$TEXMACS_PATH/plugins`
- `$TEXMACS_PATH/styles`
- `$TEXMACS_PATH/texts`
- `$TEXMACS_PATH/langs`
- `$TEXMACS_PATH/packages`
- `$TEXMACS_PATH/bin`
- `LICENSE`
- `tm-guile188/ice-9` (will be put in `progs`)

They are put in the `<artifacts-dir>/build-<timestamp>` dir during the script's build phase (`--no-build` to disable)

These artifacts can also be built with docker, in order to have the same environment as the Ubuntu server. To do this, pass
`-b/--use-docker` to the script. You will need docker installed for this (a rootless install is highly recommended)

The `-d` and `-a` options are easy to confuse:
- `-a` the result of the script's build phase, with the above dir structure, directly usable for upload
- `-d` the directory of a TeXmacs build output, not usable as is, but the script will transform it into a usable artifact

#### EXAMPLE

```bash
# build using docker and upload to the default texmacs-server@cloud.texmacs.org remote
$ misc/scripts/deploy-server.sh -b

# build using docker and upload to the remote of your choosing
$ misc/scripts/deploy-server.sh -b --remote-user alice --remote-host cloud.example.com

# skip build and use previous docker build as an artifact and upload to the default remote
$ misc/scripts/deploy-server.sh --no-build -a build-202607291006

# build artifacts from the CI (which should have a TeXmacs dir in it with progs/fonts/etc...) and upload to the default remote
$ misc/scripts/deploy-server.sh -d $CIDIR
```

### Deploy

The `texmacs-server` user does not have the right to restart services, so the following should be done with a user that has root privileges:

1. Stop the server `sudo systemctl stop texmacs-server@6562.service`
2. Change the current symlink to point to the new uploaded artifacts (as texmacs-server or prefixed by `sudo -u texmacs-server`):
```bash
$ ln -sfn $NEW_DIRECTORY /srv/texmacs-server/current
```
3. Restart mount binds (you can enable the services at boot now if you did not already)
```bash
$ sudo systemctl restart usr-local-share-TeXmacs.mount usr-local-libexec-TeXmacs-bin-texmacs.bin.mount
$ sudo systemctl restart texmacs-server@6562.service
```
