#get directory of script
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null

# check for dbtoaster
if [ ! -f "./external/dbtoaster_src/bin/dbtoaster_release" ] 
then 
  cd external/dbtoaster_src
  make all
  cp bin/dbtoaster* ../dbtoaster/bin
  cd $SCRIPTPATH
fi

# check for bolt
if [ ! -f "./lib/bolt/bolt.cmo" ] 
then 
  cd deps/bolt-1.4
  ./configure
  make all
  if [ ! -d "$SCRIPTPATH/lib/bolt" ]; then mkdir $SCRIPTPATH/lib/bolt; fi
  cp _build/bolt.* $SCRIPTPATH/lib/bolt
  cp _build/src/threads/boltThread.* $SCRIPTPATH/lib/bolt
  cp _build/src/syntax/bolt_pp.cm* $SCRIPTPATH/lib/bolt
  cd $SCRIPTPATH
fi

cd $SCRIPTPATH
if [ ! -d "./bin" ]; then mkdir bin; fi
# partition map tool
ocamlbuild PartMapTool.byte -build-dir ./bin -tag debug $@
if [ -f "./bin/src/PartMapTool.native" ]
then echo "#!/bin/bash" > ./bin/partmap_tool
     echo "ocamlrun -b $SCRIPTPATH/bin/src/PartMapTool.native \$@" >> ./bin/partmap_tool
     chmod +x ./bin/partmap_tool
fi

# driver
ocamlbuild Driver.byte -build-dir ./bin -tag debug $@
if [ -f "./bin/src/Driver.native" ]
then echo "#!/bin/bash" > ./bin/k3
     echo "export BOLT_CONFIG=$SCRIPTPATH/bolt.cfg" >> ./bin/k3
     echo "ocamlrun -b $SCRIPTPATH/bin/src/Driver.native \$@" >> ./bin/k3
     chmod +x ./bin/k3
fi
cd -
