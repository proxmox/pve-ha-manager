PACKAGE=pve-ha-manager
SIMPACKAGE=pve-ha-simulator

PREFIX=/usr
BINDIR=$(PREFIX)/bin
SBINDIR=$(PREFIX)/sbin
MANDIR=$(PREFIX)/share/man
DOCDIR=$(PREFIX)/share/doc/$(PACKAGE)
SIMDOCDIR=$(PREFIX)/share/doc/$(SIMPACKAGE)
MAN1DIR=$(MANDIR)/man1/
MAN8DIR=$(MANDIR)/man8/
BASHCOMPLDIR=$(PREFIX)/share/bash-completion/completions/
ZSHCOMPLDIR=$(PREFIX)/share/zsh/vendor-completions/
export PERLDIR=$(PREFIX)/share/perl5

-include /usr/share/pve-doc-generator/pve-doc-generator.mk

all: watchdog-mux

pve-ha-crm.bash-completion:
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::Service::pve_ha_crm; PVE::Service::pve_ha_crm->generate_bash_completions();" >$@.tmp
	mv $@.tmp $@

pve-ha-lrm.bash-completion:
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::Service::pve_ha_lrm; PVE::Service::pve_ha_lrm->generate_bash_completions();" >$@.tmp
	mv $@.tmp $@

ha-manager.bash-completion:
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::CLI::ha_manager; PVE::CLI::ha_manager->generate_bash_completions();" >$@.tmp
	mv $@.tmp $@

pve-ha-crm.zsh-completion:
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::Service::pve_ha_crm; PVE::Service::pve_ha_crm->generate_zsh_completions();" >$@.tmp
	mv $@.tmp $@

pve-ha-lrm.zsh-completion:
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::Service::pve_ha_lrm; PVE::Service::pve_ha_lrm->generate_zsh_completions();" >$@.tmp
	mv $@.tmp $@

ha-manager.zsh-completion:
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::CLI::ha_manager; PVE::CLI::ha_manager->generate_zsh_completions();" >$@.tmp
	mv $@.tmp $@

watchdog-mux: watchdog-mux.c
	gcc watchdog-mux.c -o watchdog-mux -Wall -Wl,-z,relro

.PHONY: install
install: watchdog-mux pve-ha-crm pve-ha-lrm ha-manager.1 pve-ha-crm.8 pve-ha-lrm.8 ha-manager.bash-completion pve-ha-lrm.bash-completion \
	    pve-ha-crm.bash-completion ha-manager.zsh-completion pve-ha-lrm.zsh-completion pve-ha-crm.zsh-completion pve-ha-simulator
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::Service::pve_ha_crm; PVE::Service::pve_ha_crm->verify_api();"
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::Service::pve_ha_lrm; PVE::Service::pve_ha_lrm->verify_api();"
	PVE_GENERATING_DOCS=1 perl -I. -T -e "use PVE::CLI::ha_manager; PVE::CLI::ha_manager->verify_api();"
	install -d $(DESTDIR)$(SBINDIR)
	install -m 0755 pve-ha-crm $(DESTDIR)$(SBINDIR)
	install -m 0755 pve-ha-lrm $(DESTDIR)$(SBINDIR)
	install -m 0755 ha-manager $(DESTDIR)$(SBINDIR)
	install -m 0755 watchdog-mux $(DESTDIR)$(SBINDIR)
	install -d $(DESTDIR)$(BINDIR)
	install -m 0755 pve-ha-simulator $(DESTDIR)$(BINDIR)
	install -m 0644 -D pve-ha-crm.bash-completion $(DESTDIR)$(BASHCOMPLDIR)/pve-ha-crm
	install -m 0644 -D pve-ha-lrm.bash-completion $(DESTDIR)$(BASHCOMPLDIR)/pve-ha-lrm
	install -m 0644 -D ha-manager.bash-completion $(DESTDIR)$(BASHCOMPLDIR)/ha-manager
	install -m 0644 -D pve-ha-crm.zsh-completion $(DESTDIR)$(ZSHCOMPLDIR)/_pve-ha-crm
	install -m 0644 -D pve-ha-lrm.zsh-completion $(DESTDIR)$(ZSHCOMPLDIR)/_pve-ha-lrm
	install -m 0644 -D ha-manager.zsh-completion $(DESTDIR)$(ZSHCOMPLDIR)/_ha-manager
	make -C PVE install
	make -C PVE PERLDIR=$(PREFIX)/share/$(SIMPACKAGE) installsim
	install -d $(DESTDIR)/$(MAN8DIR)
	install -m 0644 pve-ha-crm.8 $(DESTDIR)/$(MAN8DIR)
	gzip -9 $(DESTDIR)/$(MAN8DIR)/pve-ha-crm.8
	install -m 0644 pve-ha-lrm.8 $(DESTDIR)/$(MAN8DIR)
	gzip -9 $(DESTDIR)/$(MAN8DIR)/pve-ha-lrm.8
	install -d $(DESTDIR)/$(MAN1DIR)
	install -m 0644 ha-manager.1 $(DESTDIR)/$(MAN1DIR)
	gzip -9 $(DESTDIR)/$(MAN1DIR)/ha-manager.1
	$(MAKE) -C templates $@

.PHONY: test
test: 
	make -C test test

.PHONY: clean
clean:
	rm -f *.xml.tmp *.1 *.5 *.8 *{synopsis,opts}.adoc docinfo.xml
	make -C test clean
	rm -rf watchdog-mux *.1.pod *.1.gz *.bash-completion *.zsh-completion *.tmp

.PHONY: distclean
distclean: clean

