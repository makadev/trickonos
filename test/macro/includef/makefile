
TESTS = $(sort $(wildcard *.tpl))

##

TESTLOG = $(foreach t,$(TESTS),$(t).log)

.PHONY: clean test

test: $(TESTLOG)

%.log:
	$(VALGRIND) ../../../$(TCBIN) $(TC_RUN_TEST) --failtest -log$*.log -f $*

clean:
	-$(RM) -f $(TESTLOG)
	-$(RM) -f $(wildcard *.pct)
	-$(RM) -f $(wildcard *.pcu)
