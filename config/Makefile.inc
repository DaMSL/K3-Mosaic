PP_FLAGS    = -pp 'camlp4o lib/bolt/bolt_pp.cmo'
OPT_FLAGS   = -ccopt -O3 -nodynlink -unsafe -noassert

OCAML_FLAGS = -g -dtypes $(PP_FLAGS)

OCAMLCC   = ocamlc
OCAMLOPT  = ocamlopt
OCAMLMKTOP= ocamlmktop
OCAMLDEP  = ocamldep
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex