# Makefile for the K3 programming language.

include config/Makefile.inc

FILES=\
	src/Constants \
	src/util/Util \
	src/util/ListAsSet \
	src/util/Symbols \
	src/util/Printing \
	src/util/Tree \
	src/stages/k3/core/K3AST \
	src/stages/k3/core/K3Annotations \
	src/stages/k3/K3 \
	src/stages/k3/utils/K3Util \
	src/stages/k3/utils/K3Printing \
	src/stages/k3/utils/K3Helpers \
	src/stages/k3/utils/PrintSyntax \
	src/stages/k3/K3Global \
	src/stages/k3/core/K3Streams \
	src/stages/k3/K3Typechecker \
	src/stages/k3/interpreter/K3Values \
	src/stages/k3/interpreter/K3Runtime \
	src/stages/k3/interpreter/K3Consumption \
	src/stages/k3/interpreter/K3Interpreter \
	src/stages/k3_dist/ProgInfo \
	src/stages/k3_dist/K3Ring \
	src/stages/k3_dist/K3Route \
	src/stages/k3_dist/K3Shuffle \
	src/stages/k3_dist/ModifyAst \
	src/stages/k3_dist/GenDist \
	src/stages/imperative/ReifiedK3 \
	src/stages/imperative/Imperative \
	src/stages/imperative/ImperativeUtil \
	src/stages/imperative/Runtime \
	src/stages/imperative/Remoting \
	src/stages/imperative/RK3ToImperative \
	src/stages/imperative/CPP \
	src/stages/imperative/CPPTyping \
	src/stages/imperative/ImperativeToCPP \
	src/stages/m3/Debug \
	src/stages/m3/ListExtras \
	src/stages/m3/ListAsFunction \
	src/stages/m3/FreshVariable \
	src/stages/m3/M3Type \
	src/stages/m3/M3Constants \
	src/stages/m3/M3Functions \
	src/stages/m3/Schema \
	src/stages/m3/Ring \
	src/stages/m3/Arithmetic \
	src/stages/m3/Calculus \
	src/stages/m3/CalculusPrinter \
	src/stages/m3/Plan \
	src/stages/m3/M3 \
	src/stages/m3/M3ToK3 \
	src/stages/m3/M3ProgInfo \

TOPLEVEL_FILES=\
	tests/Testing \
	src/Driver \

LEXERS=\
	src/stages/k3/parser/K3Lexer \
	src/stages/m3/Calculuslexer \

PARSERS=\
	src/stages/k3/parser/K3Parser \
	src/stages/m3/Calculusparser \

DIRS=\
	lib/bolt \
	src \
	src/util \
	src/stages/k3 \
	src/stages/k3/core \
	src/stages/k3/utils \
	src/stages/k3/parser \
	src/stages/k3/interpreter \
	src/stages/k3_dist \
	src/stages/m3 \
	src/stages/imperative \
	tests \

INCLUDE_OBJ=\
        str.cma \
        unix.cma \
        dynlink.cma \
        bolt.cma


#################################################

BASE_FILES     := $(FILES)
GENERATED_FILES = $(PARSERS) $(LEXERS) 
FILES += $(GENERATED_FILES)

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
        $(patsubst %, -I %,$(DIRS)) \
        $(INCLUDE_OBJ)

OCAMLOPT_FLAGS +=\
        $(patsubst %, -I %,$(DIRS))\
        $(patsubst %.cma,%.cmxa,$(INCLUDE_OBJ))


COMMON_GARBAGE := $(patsubst %,%.o,$(FILES)) $(patsubst %,%.o,$(EXTRA_FILES)) \
		  $(patsubst %,%.annot,$(FILES)) $(patsubst %,%.annot,$(EXTRA_FILES))

BC_GARBAGE     := $(BC_FILES) $(BC_INCLUDES) $(BC_EXTRA_FILES) $(BC_EXTRA_INCLUDES)
NC_GARBAGE     := $(NC_FILES) $(NC_INCLUDES) $(NC_EXTRA_FILES) $(NC_EXTRA_INCLUDES)

#################################################

TEST_BASE=\
	tests/Testing \

TESTS=\
	tests/unit/TypecheckerTest \
	tests/unit/InterpreterTest \
	tests/unit/M3ToK3Test \

TEST_BASE_FILES    := $(patsubst %,%.cmo, $(TEST_BASE))
TEST_BASE_INCLUDES := $(patsubst %,%.cmi, $(TEST_BASE))
TEST_FILES      := $(patsubst %,%.cmo, $(TESTS))
TEST_INCLUDES   := $(patsubst %,%.cmi, $(TESTS))

TEST_BINARIES   := $(patsubst %, bin/%, $(TESTS))

TEST_GARBAGE    := $(TEST_BASE_FILES) $(TEST_BASE_INCLUDES) $(TEST_FILES) $(TEST_INCLUDES) \
                   $(patsubst %,%.o,$(TEST_BASE)) $(patsubst %,%.o,$(TESTS)) \
                   $(patsubst %,%.annot,$(TEST_BASE)) $(patsubst %,%.annot,$(TESTS))

#################################################

all: config/Makefile.local versioncheck deps k3 tests

opt: config/Makefile.local versioncheck deps k3_opt

#################################################

# Compiler binaries

k3_opt: $(NC_FILES) $(NC_EXTRA_FILES)
	@echo "Linking K3 (Optimized)"
	@if [ ! -d bin ] ; then \
		mkdir bin;\
	fi
	@$(OCAMLOPT) $(OCAMLOPT_FLAGS) -o bin/$@ $(NC_FILES) $(NC_EXTRA_FILES)

k3: $(BC_FILES) $(BC_EXTRA_FILES)
	@echo "Linking K3"
	@if [ ! -d bin ] ; then \
		mkdir bin;\
	fi
	@$(OCAMLCC) $(OCAML_FLAGS) -o bin/$@ $(BC_FILES) $(BC_EXTRA_FILES)

#################################################

# Utility targets

versioncheck:
	@if [ $(shell ocaml -version | sed 's/.*version \(.*\)$$/\1/' | \
	                  awk -F. '{print ($$1+1000) ($$2+1000) ($$3+1000)}')\
	     -lt 100310121001 ] ; then \
	  echo "Your OCaml version is too low.  OCaml 3.12.1 is required, you have"\
	       $(shell ocaml -version); exit -1; fi

#################################################

# Library dependencies

deps: lib/bolt

BASE_DIR=$(shell pwd)
BOLT_SRC_DIR = deps/bolt-1.4
BOLT_BUILD_DIR = deps/bolt-1.4/_build
BOLT_INSTALL_DIR = lib/bolt/

lib/bolt:
	@if [ ! \( -f $(BOLT_INSTALL_DIR)/bolt.cma -a -f $(BOLT_INSTALL_DIR)/bolt.cmxa \) ]; then \
		echo "Bolt Logging library" \
		&& cd $(BOLT_SRC_DIR) && /bin/bash ./configure -ocaml-prefix `which ocaml | sed 's/\/bin\/ocaml//'` \
		&& make all \
		&& cd $(BASE_DIR) \
		&& (if [ ! -d $(BOLT_INSTALL_DIR) ]; then mkdir -p $(BOLT_INSTALL_DIR); fi) \
		&& (cp $(BOLT_BUILD_DIR)/src/syntax/bolt_pp.cmo $(BOLT_INSTALL_DIR); \
	  		for ext in cmi cmo cmx o cmj jo; do \
	    		test -f $(BOLT_BUILD_DIR)/src/threads/boltThread.$$ext \
	    		&& cp $(BOLT_BUILD_DIR)/src/threads/boltThread.$$ext $(BOLT_INSTALL_DIR) || true; \
	  		done; \
	  		for ext in a cma cmi cmo cmxa cmja ja; do \
	    		test -f $(BOLT_BUILD_DIR)/bolt.$$ext \
	    		&& cp $(BOLT_BUILD_DIR)/bolt.$$ext $(BOLT_INSTALL_DIR) || true; \
	  		done; \
	  		cd $(BOLT_SRC_DIR) && make veryclean) \
	fi



#################################################

# Test Suites

tests: k3 $(TEST_FILES) $(TEST_BINARIES)
	@echo "Built tests"

#################################################

$(BC_FILES) $(BC_EXTRA_FILES) $(TEST_FILES): %.cmo : %.ml
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

$(patsubst %,%.ml,$(LEXERS)) : %.ml : %.mll
	@echo Building Lexer $(*)
	@$(OCAMLLEX) $< 2>&1 | sed 's/^/  /'

$(patsubst %,%.ml,$(PARSERS)) : %.ml : %.mly
	@echo Building Parser $(*)
	@$(OCAMLYACC) $< 2>&1 | sed 's/^/  /'

$(TEST_BINARIES) : bin/% : %.ml
	@if [ ! -d bin/tests/unit ] ; then \
		mkdir -p bin/tests/unit;\
	fi
	@echo Building $(*) test
	@$(OCAMLCC) $(OCAML_FLAGS) -o bin/$(*) $(BC_FILES) $(TEST_BASE_FILES) $<

# Ignore generated CMI dependencies.  They get autocompiled along with the
# object files
$(BC_INCLUDES) :
$(NC_INCLUDES) : 
$(TEST_INCLUDES) :

#################################################

config/Makefile.deps: Makefile $(patsubst %,%.ml,$(BASE_FILES))
	@echo Computing Dependency Graph
	@$(OCAMLDEP) $(patsubst %, -I %,$(DIRS)) \
			$(patsubst %,%.ml,$(BASE_FILES)) > $@

config/Makefile.local:
	@echo Initializing local configuration file
	@cp $@.default $@

include config/Makefile.deps
include config/Makefile.parserdeps
include config/Makefile.local

#################################################

clean:
	rm -f $(patsubst %,%.ml,$(GENERATED_FILES))
	rm -f $(patsubst %,%.mli,$(PARSERS))
	rm -f $(patsubst %,%.output,$(PARSERS))
	rm -f $(COMMON_GARBAGE) $(BC_GARBAGE) $(NC_GARBAGE) $(TEST_GARBAGE)
	rm -f bin/k3 $(TEST_BINARIES)

#################################################

.PHONY: all versioncheck
