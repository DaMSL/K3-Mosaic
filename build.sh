#!/bin/bash

#get directory of script
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

cd $SCRIPTPATH
if [ ! -d "./bin" ]; then mkdir bin; fi
ocamlbuild Driver.byte -build-dir ./bin -tag debug $@
if [ -f "./bin/src/Driver.byte" ]
then echo "#!/bin/bash" > ./bin/k3
     echo "export BOLT_CONFIG=$SCRIPTPATH/bolt.cfg" >> ./bin/k3
     echo "ocamlrun -b $SCRIPTPATH/bin/src/Driver.byte \$@" >> ./bin/k3
     chmod +x ./bin/k3
fi
cd -
