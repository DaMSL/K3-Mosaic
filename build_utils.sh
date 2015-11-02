# Build all the ocaml utilities

#get directory of script
pushd `dirname $0` > /dev/null
SCRIPTPATH=`pwd -P`
popd > /dev/null
cd $SCRIPTPATH

if [ ! -d "./bin" ]; then mkdir bin; fi

# partition map tool
ocamlbuild -use-ocamlfind PartMapTool.d.byte -build-dir ./bin $@
if [ -f "./bin/src/PartMapTool.d.byte" ]
then echo "#!/bin/bash" > ./bin/partmap_tool
     echo "$SCRIPTPATH/bin/src/PartMapTool.d.byte \$@" >> ./bin/partmap_tool
     chmod +x ./bin/partmap_tool
fi

# log sanitization tool
ocamlbuild -use-ocamlfind SanitizeLog.native -build-dir ./bin $@
if [ -f "./bin/src/SanitizeLog.native" ]
then echo "#!/bin/bash" > ./bin/sanitize_log
     echo "$SCRIPTPATH/bin/src/SanitizeLog.native \$@" >> ./bin/sanitize_log
     chmod +x ./bin/sanitize_log
fi

# data combining tool
ocamlbuild -use-ocamlfind CombineData.native -build-dir ./bin $@
if [ -f "./bin/src/CombineData.native" ]
then echo "#!/bin/bash" > ./bin/combine_data
     echo "$SCRIPTPATH/bin/src/CombineData.native \$@" >> ./bin/combine_data
     chmod +x ./bin/combine_data
fi

