
INCLGEN = ctab_imp.tpl ctab_int.tpl

##

INCL = $(INCLGEN:.tpl=.inc)

.SUFFIXES: .tpl .inc

.PHONY: clean regen check_tc $(INCLGEN)

.tpl.inc:
	../../$(TCBIN) -vc -f $*.tpl > $*.inc

regenindex: check_tc $(INCL)

check_tc:
	../../$(TCBIN) -vs -oc ctab.use
