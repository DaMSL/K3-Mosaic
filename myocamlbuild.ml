open Ocamlbuild_plugin
open Ocamlbuild_pack
let () =
  dispatch begin function
    | After_rules ->
        flag ["bolt"; "compile"]
          (S [A"-I"; A"../lib/bolt"]);
        flag ["bolt"; "link"; "byte"]
          (S [A"-I"; A"../lib/bolt"; A"bolt.cma"]);
        flag ["bolt"; "link"; "native"]
          (S [A"-I"; A"../lib/bolt"; A"bolt.cmxa"]);
    | _ -> ()
end
