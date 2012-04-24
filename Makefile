# Makefile for the K3 programming language lexer/parser/driver.

all: driver

Tree.cmi: Tree.mli
	ocamlc -annot -c Tree.mli

Tree.cmo: Tree.ml Tree.cmi
	ocamlc -annot -c Tree.ml

K3.cmi: K3.mli Tree.cmi
	ocamlc -annot -c K3.mli

K3.cmo: K3.ml K3.cmi Tree.cmi
	ocamlc -annot -c K3.ml

K3Typechecker.cmi: K3Typechecker.mli K3.cmi Tree.cmi
	ocamlc -annot -c K3Typechecker.mli

K3Typechecker.cmo: K3Typechecker.ml K3Typechecker.cmi K3.cmi Tree.cmi
	ocamlc -annot -c K3Typechecker.ml

K3Interpreter.cmi: K3Interpreter.mli K3.cmi Tree.cmi
	ocamlc -annot -c K3Interpreter.mli

K3Interpreter.cmo: K3Interpreter.ml K3Interpreter.cmi K3.cmi Tree.cmi
	ocamlc -annot -c K3Interpreter.ml

K3Parser.mli K3Parser.ml: K3Parser.mly
	ocamlyacc -v K3Parser.mly

K3Parser.cmi: K3Parser.mli K3.cmo Tree.cmo
	ocamlc -annot -c K3Parser.mli

K3Lexer.ml: K3Lexer.mll
	ocamllex K3Lexer.mll

K3Lexer.cmi K3Lexer.cmo: K3Lexer.ml K3Parser.cmi
	ocamlc -annot -c K3Lexer.ml

K3Parser.cmo: K3Lexer.cmo K3Parser.ml
	ocamlc -annot -c K3Parser.ml

driver.cmo: driver.ml K3Parser.cmo 
	ocamlc -annot -c driver.ml

driver: Tree.cmo K3.cmo K3Parser.cmo K3Lexer.cmo driver.cmo
	ocamlc -annot -o driver Tree.cmo K3.cmo K3Parser.cmo K3Lexer.cmo driver.cmo

clean:
	-rm *.cm*
	-rm *.annot
	-rm K3Lexer.ml K3Parser.ml
	-rm K3Parser.mli K3Parser.output
	-rm driver
