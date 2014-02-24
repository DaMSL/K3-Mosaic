# Build all the ocaml utilities

if [ ! -d "./bin" ]; then mkdir bin; fi

# partition map tool
ocamlbuild PartMapTool.native -build-dir ./bin $@
if [ -f "./bin/src/PartMapTool.native" ]
then echo "#!/bin/bash" > ./bin/partmap_tool
     echo "$SCRIPTPATH/bin/src/PartMapTool.native \$@" >> ./bin/partmap_tool
     chmod +x ./bin/partmap_tool
fi

# log sanitization tool
ocamlbuild SanitizeLog.native -build-dir ./bin $@
if [ -f "./bin/src/SanitizeLog.native" ]
then echo "#!/bin/bash" > ./bin/sanitize_log
     echo "$SCRIPTPATH/bin/src/SanitizeLog.native \$@" >> ./bin/sanitize_log
     chmod +x ./bin/sanitize_log
fi

# data combining tool
ocamlbuild CombineData.native -build-dir ./bin $@
if [ -f "./bin/src/CombineData.native" ]
then echo "#!/bin/bash" > ./bin/combine_data
     echo "$SCRIPTPATH/bin/src/CombineData.native \$@" >> ./bin/combine_data
     chmod +x ./bin/combine_data
fi

