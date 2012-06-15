# Makefile for the K3 distributed programming language.

k3_dist =./K3Distrib

all: GenDist.cmi GenDist.cmo rst.cmi rst.cmo 

rst.cmi rst.cmo: rst.ml K3Helpers.cmi K3.cmi 
	ocamlc -g -annot -c rst.ml

Tree.cmi: ../Tree.mli
	cd ..; ocamlc -g -annot -c Tree.mli; cp Tree.cmi $(k3_dist); cd $(k3_dist)

Tree.cmo: ../Tree.ml Tree.cmi
	cd ..; ocamlc -g -annot -c Tree.ml; cp Tree.cmo $(k3_dist); cd $(k3_dist)

K3.cmi: ../K3.mli Tree.cmi
	cd ..; ocamlc -g -annot -c K3.mli; cp K3.cmi $(k3_dist); cd $(k3_dist)

K3.cmo: ../K3.ml K3.cmi Tree.cmi
	cd ..; ocamlc -g -annot -c K3.ml; cp K3.cmo $(k3_dist); cd $(k3_dist)

K3Helpers.cmi: ../K3Helpers.mli K3.cmi Tree.cmi
	cd ..; ocamlc -g -annot -c K3Helpers.mli; cp K3Helpers.cmi $(k3_dist); cd $(k3_dist)

K3Helpers.cmo: ../K3Helpers.ml K3Helpers.cmi K3.cmi Tree.cmi
	cd ..; ocamlc -g -annot -c K3Helpers.ml; cp K3Helpers.cmo $(k3_dist); cd $(k3_dist)

ProgInfo.cmi: ProgInfo.mli K3Helpers.cmi K3.cmi Utils.cmi
	ocamlc -annot -g -c ProgInfo.mli

ProgInfo.cmo: ProgInfo.ml K3Helpers.cmo K3.cmo Utils.cmo
	ocamlc -annot -g -c ProgInfo.ml

Utils.cmi Utils.cmo: Utils.ml
	ocamlc -annot -g -c Utils.ml

GenDist.cmo: GenDist.ml ProgInfo.cmo K3Helpers.cmo K3.cmo Utils.cmo
	ocamlc -annot -g -c GenDist.ml

GenDist.cmi: GenDist.mli ProgInfo.cmi K3Helpers.cmi K3.cmi Utils.cmi
	ocamlc -annot -g -c GenDist.mli

Demo.cmo: Demo.ml GenDist.cmi GenDist.cmo K3.cmi K3.cmo Tree.cmo Tree.cmi ProgInfo.cmi ProgInfo.cmo Utils.cmi Utils.cmo K3Helpers.cmi K3Helpers.cmo
	ocamlc -annot -c -g Demo.ml

demo: Demo.cmo
	ocamlc -g -annot -o demo Tree.cmo K3.cmo Utils.cmo K3Helpers.cmo ProgInfo.cmo GenDist.cmo Demo.cmo

clean:
	-rm *.cm*
	-rm *.annot
