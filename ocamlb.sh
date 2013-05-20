#!/bin/bash

if [ ! -d "./bin" ]; then mkdir bin; fi
ocamlbuild Driver.byte -build-dir ./bin $@
if [ -f "./bin/src/Driver.byte" ]
then CURDIR=`pwd` 
     echo "#!/bin/bash" > ./bin/k3
     echo "export BOLT_CONFIG=$CURDIR/bolt.cfg" >> ./bin/k3
     echo "$CURDIR/bin/src/Driver.byte \$@" >> ./bin/k3
     chmod +x ./bin/k3
fi
