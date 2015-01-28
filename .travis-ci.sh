
echo "yes" | sudo add-apt-repository ppa:avsm
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time
echo OCaml version
ocaml -version
sudo pip install six

export OPAMYES=1
export OCAMLRUNPARAM=b

opam init
eval `opam config env`
opam install ocamlfind

./build_opt.sh
./build_utils.sh
tests/auto_test.py -l tests/passed_local_tests.txt
tests/auto_test.py -d -l tests/passed_dist_tests.txt
