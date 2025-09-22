SOURCES=Env.pm Hardware.pm TestEnv.pm TestHardware.pm RTEnv.pm RTHardware.pm Resources.pm
STUBS=Cluster.pm

.PHONY: install
install:
	install -d -m 0755 ${DESTDIR}${PERLDIR}/PVE/HA/Sim
	for i in ${SOURCES}; do install -D -m 0644 $$i ${DESTDIR}${PERLDIR}/PVE/HA/Sim/$$i; done
	for i in ${STUBS}; do install -D -m 0644 $$i ${DESTDIR}${PERLDIR}/PVE/$$i; done
	make -C Resources install
