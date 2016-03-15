#!/bin/bash
# Build all the ocaml utilities

#get directory of script
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null
cd $SCRIPTPATH

if [ ! -d "./bin" ]; then mkdir bin; fi

targets='PartMapTool,partmap_tool SanitizeLog,sanitize_log CombineData,combine_data'
ext=native
for i in $targets
do
  target=${i%,*}
  lower=${i#*,}
  ocamlbuild -use-ocamlfind $target.$ext -build-dir ./bin $@
  if [ -f "./bin/src/$target.$ext" ]
  then
    echo "#!/bin/bash" > ./bin/$lower
    echo "$SCRIPTPATH/bin/src/$target.$ext \$@" >> ./bin/$lower
    chmod +x ./bin/$lower
  fi
done

cd -
