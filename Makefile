VERSION=2.0
PACKAGE=pve-ha-manager
SIMPACKAGE=pve-ha-simulator
PKGREL=8

GITVERSION:=$(shell git rev-parse HEAD)

ARCH:=$(shell dpkg-architecture -qDEB_BUILD_ARCH)

DEB=${PACKAGE}_${VERSION}-${PKGREL}_${ARCH}.deb
SIMDEB=${SIMPACKAGE}_${VERSION}-${PKGREL}_all.deb


all: deb

.PHONY: dinstall
dinstall: $(DEB) $(SIMDEB)
	dpkg -i ${DEB} ${SIMDEB}

.PHONY: deb
deb: ${DEB} ${SIMDEB}
${DEB}:
	rm -rf build
	mkdir build
	rsync -a src/ build
	rsync -a debian/ build/debian
	echo "git clone git://git.proxmox.com/git/pve-ha-manager.git\\ngit checkout ${GITVERSION}" > build/debian/SOURCE
	cd build; dpkg-buildpackage -b -us -uc
	lintian ${DEB}
	lintian ${SIMDEB}

.PHONY: clean
clean:
	make -C src clean
	rm -rf build *.deb ${PACKAGE}-*.tar.gz *.changes *.buildinfo
	find . -name '*~' -exec rm {} ';'

.PHONY: distclean
distclean: clean

.PHONY: upload
upload: ${DEB} ${SIMDEB}
	tar cf - ${DEB} ${SIMDEB}|ssh repoman@repo.proxmox.com -- upload --product pve --dist stretch --arch ${ARCH}

