open Util

let file = "./log.out"
let logging = ref false
let channel = ref None

let set b = logging := b

let log ?name s _ = match !logging, !channel with
| true, None   -> let c = open_out file in
                  channel := Some c;
                  output_string c s
| true, Some c -> output_string c s
| _ -> ()

let close () = match !channel with
  | Some c -> close_out c; channel := None
  | _      -> ()

let () = at_exit close


