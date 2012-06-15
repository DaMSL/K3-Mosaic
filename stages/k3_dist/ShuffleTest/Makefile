# Makefile for the shuffle test suite

all: ShuffleTest

Shuffle.cmi: Shuffle.mli
	ocamlc -c Shuffle.mli

Shuffle.cmo: Shuffle.ml Shuffle.cmi
	ocamlc -c Shuffle.ml

ShuffleTest.cmo: ShuffleTest.ml Shuffle.cmo
	ocamlc -c ShuffleTest.ml
	
ShuffleTest: ShuffleTest.cmo Shuffle.cmo
	ocamlc -o ShuffleTest Shuffle.cmo ShuffleTest.cmo

clean:
	-rm *.cm*
