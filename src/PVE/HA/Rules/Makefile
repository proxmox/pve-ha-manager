SOURCES=NodeAffinity.pm ResourceAffinity.pm

.PHONY: install
install:
	install -d -m 0755 ${DESTDIR}${PERLDIR}/PVE/HA/Rules
	for i in ${SOURCES}; do install -D -m 0644 $$i ${DESTDIR}${PERLDIR}/PVE/HA/Rules/$$i; done
