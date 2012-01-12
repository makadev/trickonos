## procs used - except for defaults
FPC = fpc
PYACC = pyacc
PASDOC = pasdoc
MKDIR = mkdir
RM = rm
VALGRIND = valgrind

## file
PROJECTFILE = tcci.lpr

## pathes
UNIT_PATH = util compiler machine sobject
INC_PATH = 
DOC_PATH = apidoc
OBJ_PATH = lib
BIN_PATH = bin
SRC_PATH = src

## defs
DEFAULT_DEFS =
RELEASE_DEFS = RELEASE
DEBUG_DEFS = DEBUG
MEMDEBUG_DEFS = DEBUG CLEANSHUTDOWN
VGDEBUG_DEFS = DEBUG CLEANSHUTDOWN
VGRELEASE_DEFS = RELEASE

## flags
DEFAULT_FLAGS = -Sg- -Sh -Sm- -Mobjfpc -Sc- -Ss-
RELEASE_FLAGS = -B -Si -Sa- -Ci- -Co- -Cr- -Ct- -CR- -O2 -CX -XX -gs -gl
DEBUG_FLAGS = -Si- -Sa -Cior -O- -CR -g -gl -gh-
MEMDEBUG_FLAGS = -Si- -Sa -Cior -O- -CR -g -gl -gh
VGDEBUG_FLAGS = -Si- -Sa -Cior -O- -CR -g -gl -gh- -gv -gp
VGRELEASE_FLAGS = -B -Si -Sa- -Ci- -Co- -Cr- -Ct- -CR- -O2 -CX -Xs -XX -gh- -gl -gp -gv

## default test vars for tc

TCBIN=$(BIN_PATH)/tcci
TC_CC_TEST=-oc --scan-debug --pct-debug -vd
TC_RUN_TEST=--pct-debug -vd

## pasdoc settings
PASDOCSETTINGS = -v 1 --auto-abstract --marker 'DOC>> ' \
  -L en -E $(DOC_PATH) \
  -O html $(foreach unit,$(UNIT_PATH),$(SRC_PATH)/$(unit)/*.pas)

########## internal ############
FILEFLAGS= -FE../$(BIN_PATH) -FU../$(OBJ_PATH) \
       $(foreach unit,$(UNIT_PATH),-Fu$(unit)) \
       $(foreach inc,$(INC_PATH),-Fi$(inc))

FPCFLAGS = $(DEFAULT_FLAGS) \
 $(foreach def,$(DEFAULT_DEFS),-d$(def))

RELEASE_FLAGS += $(foreach def,$(RELEASE_DEFS),-d$(def))
DEBUG_FLAGS += $(foreach def,$(DEBUG_DEFS),-d$(def))
MEMDEBUG_FLAGS += $(foreach def,$(MEMDEBUG_DEFS),-d$(def))
VGDEBUG_FLAGS += $(foreach def,$(VGDEBUG_DEFS),-d$(def))
VGRELEASE_FLAGS += $(foreach def,$(VGRELEASE_DEFS),-d$(def))

PASDOCFLAGS = $(PASDOCSETTINGS) \
 $(foreach unit,$(UNIT_PATH),-I$(SRC_PATH)/$(unit)) \
 $(foreach inc,$(INC_PATH),-I$(SRC_PATH)/$(inc)) \
 $(foreach def,$(DEFAULT_DEFS),-D$(def))

PASDOCRELEASE_FLAGS = $(foreach def,$(RELEASE_DEFS),-D$(def)) \
  --visible-members protected,public,published,automated

PASDOCDEBUG_FLAGS = $(foreach def,$(DEBUG_DEFS),-D$(def)) \
 --visible-members private,protected,public,published,automated

## bin build
export

build: debug

release: makepathes
	$(MAKE) -C $(SRC_PATH) release

debug: makepathes
	$(MAKE) -C $(SRC_PATH) debug

memdebug: makepathes
	$(MAKE) -C $(SRC_PATH) memdebug

vgdebug: makepathes
	$(MAKE) -C $(SRC_PATH) vgdebug

vgrelease: makepathes
	$(MAKE) -C $(SRC_PATH) vgrelease

makepathes:
	-$(MKDIR) $(OBJ_PATH) $(BIN_PATH)

## core test
test: clean debug
	$(MAKE) -C test clean
	$(MAKE) -C test VALGRIND=

releasetest: clean release
	$(MAKE) -C test clean
	$(MAKE) -C test VALGRIND=

memtest: clean memdebug
	$(MAKE) -C test clean
	$(MAKE) -C test VALGRIND=

vgtest: clean vgdebug
	$(MAKE) -C test clean
	$(MAKE) -C test

## extension test
testext: clean debug
	$(MAKE) -C testext clean
	$(MAKE) -C testext VALGRIND=

releasetestext: clean release
	$(MAKE) -C testext clean
	$(MAKE) -C testext VALGRIND=

memtestext: clean memdebug
	$(MAKE) -C testext clean
	$(MAKE) -C testext VALGRIND=

vgtestext: clean vgdebug
	$(MAKE) -C testext clean
	$(MAKE) -C testext

## all tests

testall: clean debug
	$(MAKE) -C test clean
	$(MAKE) -C test VALGRIND=
	$(MAKE) -C testext clean
	$(MAKE) -C testext VALGRIND=

releasetestall: clean release
	$(MAKE) -C test clean
	$(MAKE) -C test VALGRIND=
	$(MAKE) -C testext clean
	$(MAKE) -C testext VALGRIND=

memtestall: clean memdebug
	$(MAKE) -C test clean
	$(MAKE) -C test VALGRIND=
	$(MAKE) -C testext clean
	$(MAKE) -C testext VALGRIND=

vgtestall: clean vgdebug
	$(MAKE) -C test clean
	$(MAKE) -C test
	$(MAKE) -C testext clean
	$(MAKE) -C testext

## stuff

.PHONY: clean cleanall pasdoc doc

## create docs
pasdoc: docrelease

#doc:
#	$(MAKE) -C doc

docrelease: makedocpathes
	$(PASDOC) $(PASDOCFLAGS) $(PASDOCRELEASE_FLAGS)

docdebug: makedocpathes
	$(PASDOC) $(PASDOCFLAGS) $(PASDOCDEBUG_FLAGS)

makedocpathes:
	-$(MKDIR) $(DOC_PATH)

## cleaning

clean:
	-$(RM) -rf $(OBJ_PATH) $(BIN_PATH)

cleanall: clean
	-$(RM) -rf $(DOC_PATH)
#	$(MAKE) -C doc cleanall
	$(MAKE) -C test clean
	$(MAKE) -C testext clean
