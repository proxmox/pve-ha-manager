NOTIFICATION_TEMPLATES=							\
	default/fencing-subject.txt.hbs				\
	default/fencing-body.txt.hbs				\
	default/fencing-body.html.hbs				\

.PHONY: install
install:
	install -dm 0755 $(DESTDIR)/usr/share/pve-manager/templates/default
	$(foreach i,$(NOTIFICATION_TEMPLATES), \
	    install -m644 $(i) $(DESTDIR)/usr/share/pve-manager/templates/$(i) ;)
