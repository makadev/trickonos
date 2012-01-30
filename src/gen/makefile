
INCLGEN = ctab_imp.tpl ctab_int.tpl nulltypes.tpl

##

INCL = $(INCLGEN:.tpl=.inc)

.SUFFIXES: .tpl .inc

.PHONY: clean regen check_tc $(INCLGEN)

.tpl.inc:
	$(TCCI) -vc -f $*.tpl > $*.inc

regen: check_tc $(INCL)

check_tc:
	$(TCCI) -vs -oc ctab.use
