SIM_SOURCES=Helpers.pm NodeAffinity.pm ResourceAffinity.pm
SOURCES=${SIM_SOURCES}

.PHONY: install
install:
	install -d -m 0755 ${DESTDIR}${PERLDIR}/PVE/HA/Rules
	for i in ${SOURCES}; do install -D -m 0644 $$i ${DESTDIR}${PERLDIR}/PVE/HA/Rules/$$i; done

.PHONY: installsim
installsim:
	install -d -m 0755 ${DESTDIR}${PERLDIR}/PVE/HA/Rules
	for i in ${SIM_SOURCES}; do install -D -m 0644 $$i ${DESTDIR}${PERLDIR}/PVE/HA/Rules/$$i; done
