# Makefile for the K3 programming language lexer/parser/driver.

include Makefile.inc

FILES=\
	util/Util \
	util/ListAsSet \
	util/Symbols \
	util/Printing \
	util/Tree \
	stages/k3/K3AST \
	stages/k3/K3Annotations \
	stages/k3/K3 \
	stages/k3/K3Util \
	stages/k3/K3Printing \
	stages/k3/K3Helpers \
	stages/k3/K3Typechecker \
	stages/k3/K3Values \
	stages/k3/K3Streams \
	stages/k3/interpreter/K3Consumption \
	stages/k3/interpreter/K3Runtime \
	stages/k3/interpreter/K3Interpreter \
	stages/k3_dist/ProgInfo \
	stages/k3_dist/GenDist \
	stages/imperative/ReifiedK3 \
	stages/imperative/Imperative \
	stages/imperative/ImperativeUtil \
	stages/imperative/RK3ToImperative \
	stages/imperative/CPP \
	stages/imperative/ImperativeToCPP \
	stages/imperative/CPPTyping

TOPLEVEL_FILES=\
	tests/Testing \
	Driver \

LEXERS=\
	stages/k3/K3Lexer \

PARSERS=\
	stages/k3/K3Parser \

DIRS=\
	util\
	stages/k3 \
	stages/k3/interpreter \
	stages/k3_dist \
	stages/imperative \
	tests \

INCLUDE_OBJ=\
        str.cma\
        unix.cma


#################################################

#ML_FILES     := $(foreach f,$(FILES),$(wildcard $(f).ml))
#MLI_FILES    := $(foreach f,$(FILES),$(wildcard $(f).mli))
#BASE_FILES   := $(patsubst %.ml,%,$(ML_FILES))

BASE_FILES     := $(FILES)
GENERATED_FILES = $(PARSERS) $(LEXERS) 
FILES += $(GENERATED_FILES)

#ML_FILES  += $(patsubst %,%.ml,$(GENERATED_FILES))
#MLI_FILES += $(patsubst %,%.mli,$(GENERATED_FILES))

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
	tests/TypecheckerTest \
	tests/InterpreterTest \

TEST_BASE_FILES    := $(patsubst %,%.cmo, $(TEST_BASE))
TEST_BASE_INCLUDES := $(patsubst %,%.cmi, $(TEST_BASE))
TEST_FILES      := $(patsubst %,%.cmo, $(TESTS))
TEST_INCLUDES   := $(patsubst %,%.cmi, $(TESTS))

TEST_BINARIES   := $(patsubst %, bin/%, $(TESTS))

TEST_GARBAGE    := $(TEST_BASE_FILES) $(TEST_BASE_INCLUDES) $(TEST_FILES) $(TEST_INCLUDES) \
                   $(patsubst %,%.o,$(TEST_BASE)) $(patsubst %,%.o,$(TESTS)) \
                   $(patsubst %,%.annot,$(TEST_BASE)) $(patsubst %,%.annot,$(TESTS))

#################################################

all: Makefile.local versioncheck k3 tests

opt: Makefile.local versioncheck k3_opt

versioncheck:
	@if [ $(shell ocaml -version | sed 's/.*version \(.*\)$$/\1/' | \
	                  awk -F. '{print ($$1+1000) ($$2+1000) ($$3+1000)}')\
	     -lt 100310121001 ] ; then \
	  echo "Your OCaml version is too low.  OCaml 3.12.1 is required, you have"\
	       $(shell ocaml -version); exit -1; fi

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
	@if [ ! -d bin/tests ] ; then \
		mkdir -p bin/tests;\
	fi
	@echo Building $(*) test
	@$(OCAMLCC) $(OCAML_FLAGS) -o bin/$(*) $(BC_FILES) $(TEST_BASE_FILES) $<

# Ignore generated CMI dependencies.  They get autocompiled along with the
# object files
$(BC_INCLUDES) :
$(NC_INCLUDES) : 
$(TEST_INCLUDES) :

#################################################

Makefile.deps: Makefile $(patsubst %,%.ml,$(BASE_FILES))
	@echo Computing Dependency Graph
	@$(OCAMLDEP) $(patsubst %, -I %,$(DIRS)) \
			$(patsubst %,%.ml,$(BASE_FILES)) > $@

Makefile.local:
	@echo Initializing local configuration file
	@cp config/Makefile.local.default Makefile.local

include Makefile.deps
include Makefile.parserdeps
include Makefile.local

#################################################

clean:
	rm -f $(patsubst %,%.ml,$(GENERATED_FILES))
	rm -f $(patsubst %,%.mli,$(PARSERS))
	rm -f $(patsubst %,%.output,$(PARSERS))
	rm -f $(COMMON_GARBAGE) $(BC_GARBAGE) $(NC_GARBAGE) $(TEST_GARBAGE)
	rm -f bin/k3 $(TEST_BINARIES)

#################################################

.PHONY: all versioncheck
