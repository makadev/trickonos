
TESTS = $(sort $(wildcard *.tpl))

##

TESTLOG = $(foreach t,$(TESTS),$(t).log)

.PHONY: clean test

test: $(TESTLOG)

%.log:
	$(VALGRIND) ../../../$(TCBIN) $(TC_CC_TEST) --failtest -log$*.log -f $*

clean:
	-$(RM) -f $(TESTLOG)
	-$(RM) -f $(foreach t,$(TESTS),$(t).pct)
	-$(RM) -f $(foreach t,$(TESTS),$(t).pcu)
