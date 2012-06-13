# Makefile for the K3 programming language lexer/parser/driver.
# -- Based off DBToaster Makefiles. (Yanif) 

include Makefile.inc

FILES=\
	util/ListAsSet \
	util/Symbols \
	util/Tree \
	stages/k3/K3 \
	stages/k3/K3Util \
	#stages/k3/K3Typechecker \
	#stages/k3/K3Interpreter \

TOPLEVEL_FILES=\
	Driver \

LEXERS=\
	stages/k3/K3Lexer \

PARSERS=\
	stages/k3/K3Parser \

DIRS=\
	util\
	stages/k3 \

#################################################

BASE_FILES     := $(FILES)
GENERATED_FILES = $(PARSERS) $(LEXERS) 
FILES += $(GENERATED_FILES)

BC_FILES    =$(patsubst %,%.cmo,$(FILES))
BC_INCLUDES =$(patsubst %,%.cmi,$(FILES))
NC_FILES    =$(patsubst %,%.cmx,$(FILES))
NC_INCLUDES =$(patsubst %,%.cmxi,$(FILES))

OCAML_FLAGS +=\
	$(patsubst %, -I %,$(DIRS))

OCAMLOPT_FLAGS +=\
	$(patsubst %, -I %,$(DIRS))

#################################################

all: Makefile.local versioncheck bin/k3

versioncheck:
	@if [ $(shell ocaml -version | sed 's/.*version \(.*\)$$/\1/' | \
	                  awk -F. '{print ($$1+1000) ($$2+1000) ($$3+1000)}')\
	     -lt 100310121001 ] ; then \
	  echo "Your OCaml version is too low.  OCaml 3.12.1 is required, you have"\
	       $(shell ocaml -version); exit -1; fi

k3c: Tree.cmo K3.cmo K3Util.cmo K3Parser.cmo K3Lexer.cmo driver.cmo
	ocamlc -annot -o driver Tree.cmo K3.cmo K3Util.cmo K3Parser.cmo K3Lexer.cmo driver.cmo

bin/k3: $(NC_FILES) Driver.ml
	@echo "Linking K3 (Optimized)"
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -o $@ $(NC_FILES) Driver.ml

#################################################

$(BC_FILES) : %.cmo : %.ml
	@if [ -f $(*).mli ] ; then \
		echo Compiling Header $(*);\
		$(OCAMLCC) $(OCAML_FLAGS) -c $(*).mli;\
	fi	
	@echo Compiling $(*)
	@$(OCAMLCC) $(OCAML_FLAGS) -c $<

$(NC_FILES) : %.cmx : %.ml
	@if [ -f $(*).mli ] ; then \
		echo Compiling Optimized Header $(*);\
		$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $(*).mli;\
	fi	
	@echo Compiling Optimized $(*)
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $<

$(patsubst %,%.ml,$(LEXERS)) : %.ml : %.mll
	@echo Building Lexer $(*)
	@$(OCAMLLEX) $< 2>&1 | sed 's/^/  /'

$(patsubst %,%.ml,$(PARSERS)) : %.ml : %.mly
	@echo Building Parser $(*)
	@$(OCAMLYACC) $< 2>&1 | sed 's/^/  /'

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
	@cp config/Makefile.local.default Makefile.local

include Makefile.deps
include Makefile.local

#################################################

clean:
	rm -f $(patsubst %,%.ml,$(GENERATED_FILES))
	rm -f $(patsubst %,%.mli,$(PARSERS))
	rm -f $(patsubst %,%.output,$(PARSERS))
	rm -f $(BC_FILES) $(BC_INCLUDES)
	rm -f $(NC_FILES) $(NC_INCLUDES)
	rm -f $(patsubst %,%.o,$(FILES))
	rm -f $(patsubst %,%.annot,$(FILES))
	rm -f bin/k3

#################################################

.PHONY: all versioncheck
	
