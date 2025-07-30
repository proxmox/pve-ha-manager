all:


.PHONY: test
test:
	@echo "-- start regression tests --"
	./test_failover1.pl
	./test_rules_config.pl
	./ha-tester.pl
	./test_fence_config.pl
	@echo "-- end regression tests (success) --"

.PHONY: clean
clean:
	rm -rf *~ test-*/log  test-*/*~ test-*/status \
	fence_cfgs/*.cfg.commands fence_cfgs/*.write \
	rules_cfgs/*.cfg.output
