TESTDIR = string

##

TESTS = $(foreach testdir,$(TESTDIR),$(testdir).test)
TESTCLEAN = $(foreach testdir,$(TESTDIR),$(testdir).clean)

.PHONY: clean test

test: $(TESTS)

%.test:
	$(MAKE) -C $*

clean: $(TESTCLEAN)

%.clean:
	$(MAKE) -C $* clean
