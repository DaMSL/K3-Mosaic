
echo OCaml version
ocaml -version

export OPAMYES=1
export OCAMLRUNPARAM=b

opam init
eval `opam config env`
opam install ocamlfind

./build_opt.sh
./build_utils.sh
tests/auto_test.py -l tests/passed_local_tests.txt
tests/auto_test.py -d -l tests/passed_dist_tests.txt
