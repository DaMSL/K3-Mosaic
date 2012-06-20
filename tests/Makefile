# Makefile for the K3 programming language lexer/parser/driver.
# -- Based off DBToaster Makefiles. (Yanif) 

include ../Makefile.inc

FILES=\
	../util/Util \
	../util/ListAsSet \
	../util/Symbols \
	../util/Tree \
	../stages/k3/K3 \
	../stages/k3/K3Util \
	../stages/k3/K3Helpers \
	../stages/k3_dist/ProgInfo \
	../stages/k3_dist/GenDist \

TOPLEVEL_FILES=\
	DistTest \

DIRS=\
	../util\
	../stages/k3 \
	../stages/k3_dist \

#################################################

BASE_FILES     := $(FILES)

BC_FILES    =$(patsubst %,%.cmo,$(FILES))
BC_INCLUDES =$(patsubst %,%.cmi,$(FILES))
NC_FILES    =$(patsubst %,%.cmx,$(FILES))
NC_INCLUDES =$(patsubst %,%.cmxi,$(FILES))

EXTRA_FILES        := $(TOPLEVEL_FILES)
BC_EXTRA_FILES      =$(patsubst %,%.cmo, $(EXTRA_FILES))
BC_EXTRA_INCLUDES   =$(patsubst %,%.cmi, $(EXTRA_FILES))
NC_EXTRA_FILES      =$(patsubst %,%.cmx, $(EXTRA_FILES))
NC_EXTRA_INCLUDES   =$(patsubst %,%.cmxi,$(EXTRA_FILES))

OCAML_FLAGS +=\
	$(patsubst %, -I %,$(DIRS))

OCAMLOPT_FLAGS +=\
	$(patsubst %, -I %,$(DIRS))

#################################################

all: Makefile.local versioncheck dist_test

versioncheck:
	@if [ $(shell ocaml -version | sed 's/.*version \(.*\)$$/\1/' | \
	                  awk -F. '{print ($$1+1000) ($$2+1000) ($$3+1000)}')\
	     -lt 100310121001 ] ; then \
	  echo "Your OCaml version is too low.  OCaml 3.12.1 is required, you have"\
	       $(shell ocaml -version); exit -1; fi

dist_test: $(BC_FILES) $(BC_EXTRA_FILES)
	@echo "Building dist_test"
	@if [ ! -d bin ] ; then \
		mkdir bin;\
	fi
	@$(OCAMLCC) $(OCAML_FLAGS) -o bin/$@ $(BC_FILES) $(BC_EXTRA_FILES)

#################################################

$(BC_FILES) $(BC_EXTRA_FILES) : %.cmo : %.ml
	@if [ -f $(*).mli ] ; then \
		echo Compiling Header $(*);\
		$(OCAMLCC) $(OCAML_FLAGS) -c $(*).mli;\
	fi	
	@echo Compiling $(*)
	@$(OCAMLCC) $(OCAML_FLAGS) -c $<

$(NC_FILES) $(NC_EXTRA_FILES) : %.cmx : %.ml
	@if [ -f $(*).mli ] ; then \
		echo Compiling Optimized Header $(*);\
		$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $(*).mli;\
	fi	
	@echo Compiling Optimized $(*)
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $<

# Ignore generated CMI dependencies.  They get autocompiled along with the
# object files
$(patsubst %,%.cmi,$(FILES)) : 
$(patsubst %,%.cmxi,$(FILES)) : 

#################################################

Makefile.deps: Makefile $(patsubst %,%.ml,$(BASE_FILES))
	@echo Computing Dependency Graph
	@$(OCAMLDEP) $(patsubst %, -I %,$(DIRS)) \
			$(patsubst %,%.ml,$(BASE_FILES)) > $@

Makefile.local:
	@echo Initializing local configuration file
	@cp ../config/Makefile.local.default Makefile.local

include Makefile.deps
include Makefile.local

#################################################

clean:
	rm -f $(patsubst %,%.ml,$(GENERATED_FILES))
	rm -f $(patsubst %,%.mli,$(PARSERS))
	rm -f $(patsubst %,%.output,$(PARSERS))
	rm -f $(BC_FILES) $(BC_INCLUDES) $(BC_EXTRA_FILES) $(BC_EXTRA_INCLUDES)
	rm -f $(NC_FILES) $(NC_INCLUDES) $(NC_EXTRA_FILES) $(NC_EXTRA_INCLUDES)
	rm -f $(patsubst %,%.o,$(FILES)) $(patsubst %,%.o,$(EXTRA_FILES))
	rm -f $(patsubst %,%.annot,$(FILES))
	rm -f bin/dist_test

#################################################

.PHONY: all versioncheck
	
