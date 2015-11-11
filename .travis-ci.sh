echo OCaml version
ocaml -version

export OPAMYES=1
export OCAMLRUNPARAM=b

opam init
eval `opam config env`
opam install ocamlfind
opam install yojson

./build_opt.sh
./build_utils.sh
echo Running local tests...
tests/auto_test.py -l tests/passed_local_tests.txt --no-log
echo Done with local tests
echo Running distributed tests...
tests/auto_test.py -d -l tests/passed_dist_tests.txt --no-log -n 4 --no-opt-route
echo Done with distributed tests
echo Running no-corrective tests...
tests/auto_test.py -d -l tests/passed_dist_tests_no_corr.txt --no-log --no-correctives -n 4 --no-opt-route
echo Done with no-corrective tests
echo 'Creating TPCH files (no interpretation)'
tests/auto_test.py -d -l tests/tpch_tests.txt --no-interp --no-opt-route
echo 'Done with TPCH files (no interpretation)'

