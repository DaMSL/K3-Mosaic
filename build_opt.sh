#!/bin/bash

#get directory of script
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

cd $SCRIPTPATH

# check for dbtoaster
if [ ! -f "./external/dbtoaster/bin/dbtoaster_release" ] 
then 
  if [ ! -d "./external/dbtoaster/bin" ]
  then
      mkdir "./external/dbtoaster/bin"
  fi
  cd external/dbtoaster_src
  echo "Building DBToaster..."
  make bin/dbtoaster
  cp bin/dbtoaster* ../dbtoaster/bin
  cd $SCRIPTPATH
fi

if [ ! -d "./bin" ]; then mkdir bin; fi

# driver
ocamlbuild Driver.native -build-dir ./bin $@
if [ -f "./bin/src/Driver.native" ]
then echo "#!/bin/bash" > ./bin/k3
     echo "export BOLT_CONFIG=$SCRIPTPATH/bolt.cfg" >> ./bin/k3
     echo "$SCRIPTPATH/bin/src/Driver.native \$@" >> ./bin/k3
     chmod +x ./bin/k3
fi
cd -
