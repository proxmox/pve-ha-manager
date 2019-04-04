VERSION=2.0
PACKAGE=pve-ha-manager
SIMPACKAGE=pve-ha-simulator
PKGREL=9

GITVERSION:=$(shell git rev-parse HEAD)
BUILDDIR ?= ${PACKAGE}-${VERSION}
ARCH:=$(shell dpkg-architecture -qDEB_BUILD_ARCH)

DEB=${PACKAGE}_${VERSION}-${PKGREL}_${ARCH}.deb
DSC=${PACKAGE}_${VERSION}-${PKGREL}.dsc
SIMDEB=${SIMPACKAGE}_${VERSION}-${PKGREL}_all.deb
SIMDSC=${PACKAGE}_${VERSION}-${PKGREL}_${ARCH}.deb

all: deb

${BUILDDIR}:
	rm -rf ${BUILDDIR}
	rsync -a src/ debian ${BUILDDIR}
	echo "git clone git://git.proxmox.com/git/pve-ha-manager.git\\ngit checkout ${GITVERSION}" > ${BUILDDIR}/debian/SOURCE

.PHONY: dinstall
dinstall: $(DEB) $(SIMDEB)
	dpkg -i ${DEB} ${SIMDEB}

.PHONY: deb
deb: ${DEB} ${SIMDEB}
${DEB}: ${BUILDDIR}
	cd ${BUILDDIR}; dpkg-buildpackage -b -us -uc
	lintian ${DEB}
	lintian ${SIMDEB}

.PHONY: dsc
dsc: ${DSC}
${DSC}: ${BUILDDIR}
	cd ${BUILDDIR}; dpkg-buildpackage -S -us -uc -d -nc
	lintian ${DSC}

.PHONY: clean
clean:
	make -C src clean
	rm -rf ${BUILDDIR} *.tar.gz *.dsc *.deb ${PACKAGE}-*.tar.gz *.changes *.buildinfo
	find . -name '*~' -exec rm {} ';'

.PHONY: distclean
distclean: clean

.PHONY: upload
upload: ${DEB} ${SIMDEB}
	tar cf - ${DEB} ${SIMDEB}|ssh repoman@repo.proxmox.com -- upload --product pve --dist stretch --arch ${ARCH}

