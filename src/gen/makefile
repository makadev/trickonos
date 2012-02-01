
INCLGEN = ctab.tpl nulltypes.tpl

INCCLEAN = $(wildcard *.inc)

##

.PHONY: clean cleaninc regen check_tc $(INCLGEN)

cleaninc:
	-$(RM) $(INCCLEAN)

*.tpl:
	$(TCCI) -vc -f $@

regen: check_tc cleaninc $(INCLGEN)

check_tc:
	$(TCCI) -vs -oc nulltypes.tpl
