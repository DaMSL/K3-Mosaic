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

# build utils
if [ "$1" != '-clean' ]
then
  targets='PartMapTool,partmap_tool SanitizeLog,sanitize_log CombineData,combine_data'
  ext=native
  for i in $targets
  do
    target=${i%,*}
    lower=${i#*,}
    if [ ! -f "./bin/src/$target.$ext" ]
    then
      ocamlbuild -use-ocamlfind $target.$ext -build-dir ./bin $@
      if [ -f "./bin/src/$target.$ext" ]
      then
        echo "#!/bin/bash" > ./bin/$lower
        echo "$SCRIPTPATH/bin/src/$target.$ext \$@" >> ./bin/$lower
        chmod +x ./bin/$lower
      fi
    fi
  done
fi

cd -
