#!/bin/bash

#get directory of script
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

cd $SCRIPTPATH

if [ ! -d "./bin" ]; then mkdir bin; fi

# driver
ocamlbuild -use-ocamlfind Driver.native -build-dir ./bin $@
if [ -f "./bin/src/Driver.native" ]
then echo "#!/bin/bash" > ./bin/k3
     echo "$SCRIPTPATH/bin/src/Driver.native \$@" >> ./bin/k3
     chmod +x ./bin/k3
fi
cd -
