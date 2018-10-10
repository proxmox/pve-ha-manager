SOURCES=VirtVM.pm VirtCT.pm VirtFail.pm

.PHONY: install
install:
	install -d -m 0755 ${DESTDIR}${PERLDIR}/PVE/HA/Sim/Resources
	for i in ${SOURCES}; do install -D -m 0644 $$i ${DESTDIR}${PERLDIR}/PVE/HA/Sim/Resources/$$i; done
