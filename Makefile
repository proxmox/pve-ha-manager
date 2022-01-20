include /usr/share/dpkg/pkg-info.mk
include /usr/share/dpkg/architecture.mk

PACKAGE=pve-ha-manager
SIMPACKAGE=pve-ha-simulator

GITVERSION:=$(shell git rev-parse HEAD)
BUILDDIR ?= ${PACKAGE}-${DEB_VERSION_UPSTREAM}

DEB=${PACKAGE}_${DEB_VERSION_UPSTREAM_REVISION}_${DEB_BUILD_ARCH}.deb
DBG_DEB=${PACKAGE}-dbgsym_${DEB_VERSION_UPSTREAM_REVISION}_${DEB_BUILD_ARCH}.deb

DSC=${PACKAGE}_${DEB_VERSION_UPSTREAM_REVISION}.dsc
SIMDEB=${SIMPACKAGE}_${DEB_VERSION_UPSTREAM_REVISION}_all.deb
SIMDSC=${SIMPACKAGE}_${DEB_VERSION_UPSTREAM_REVISION}.dsc

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
${DEB} ${DBG_DEB}: ${BUILDDIR}
	cd ${BUILDDIR}; dpkg-buildpackage -b -us -uc
	lintian ${DEB}
	lintian ${SIMDEB}

.PHONY: dsc
dsc: ${DSC}
${DSC}: ${BUILDDIR}
	cd ${BUILDDIR}; dpkg-buildpackage -S -us -uc -d
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
	tar cf - ${DEB} ${DBG_DEB} ${SIMDEB}|ssh repoman@repo.proxmox.com -- upload --product pve --dist bullseye --arch ${DEB_BUILD_ARCH}

