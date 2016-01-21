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
echo Running distributed/corrective/no-isobatch tests...
tests/auto_test.py -d -l tests/passed_dist_tests.txt --no-log -s 2 -n 4 --no-opt-route --no-run-isobatch --run-correctives
echo Done with distributed/corrective/no-isobatch tests
echo Running distributed/no-corrective/isobatch tests...
tests/auto_test.py -d -l tests/passed_dist_tests_no_corr.txt --no-log -s 2 -n 4 --no-opt-route
echo Done with distributed/no-corrective/isobatch tests
echo 'Creating TPCH files (no interpretation)'
tests/auto_test.py -d -l tests/tpch_tests.txt --no-interp --no-opt-route
echo 'Done with TPCH files (no interpretation)'


