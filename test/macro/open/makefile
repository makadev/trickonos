
TESTS = $(sort $(wildcard *.tpl))

##

TESTLOG = $(foreach t,$(TESTS),$(t).log)

.PHONY: clean test

test: $(TESTLOG)

%.log:
	$(VALGRIND) ../../../$(TCBIN) $(TC_RUN_TEST) -log$*.log -f $*

clean:
	-$(RM) -f $(TESTLOG)
	-$(RM) -f $(foreach t,$(TESTS),$(t).pct)
	-$(RM) -f $(foreach t,$(TESTS),$(t).pcu)
	-$(RM) -f $(foreach t,$(TESTS),$(t).out)
	-$(RM) -f $(wildcard somedir/*.out)

