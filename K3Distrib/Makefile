# Makefile for the K3 distributed programming language.

k3_dist =./K3Distrib

all: rst.cmi rst.cmo

rst.cmi rst.cmo: rst.ml K3Helpers.cmi K3.cmi Tree.cmi K3Helpers.cmo 
	ocamlc -annot -c rst.ml

Tree.cmi: ../Tree.mli
	cd ..; ocamlc -annot -c Tree.mli; cp Tree.cmi $(k3_dist); cd $(k3_dist)

Tree.cmo: ../Tree.ml Tree.cmi
	cd ..; ocamlc -annot -c Tree.ml; cp Tree.cmo $(k3_dist); cd $(k3_dist)

K3.cmi: ../K3.mli Tree.cmi
	cd ..; ocamlc -annot -c K3.mli; cp K3.cmi $(k3_dist); cd $(k3_dist)

K3.cmo: ../K3.ml K3.cmi Tree.cmi
	cd ..; ocamlc -annot -c K3.ml; cp K3.cmo $(k3_dist); cd $(k3_dist)

K3Helpers.cmi: ../K3Helpers.mli K3.cmi Tree.cmi
	cd ..; ocamlc -annot -c K3Helpers.mli; cp K3Helpers.cmi $(k3_dist); cd $(k3_dist)

K3Helpers.cmo: ../K3Helpers.ml K3Helpers.cmi K3.cmi Tree.cmi
	cd ..; ocamlc -annot -c K3Helpers.ml; cp K3Helpers.cmo $(k3_dist); cd $(k3_dist)

clean:
	-rm *.cm*
	-rm *.annot
