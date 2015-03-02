open Util

let file = "./log.out"
let logging = ref true
let channel = ref None
let log_flush_cnt = 100
let log_cnt = ref 0

let set b = logging := b

let log ?name s _ = match !logging, !channel with
| true, None   -> let c = open_out file in
                  channel := Some c;
                  incr log_cnt;
                  output_string c s
| true, Some c ->
    incr log_cnt;
    output_string c s;
    if !log_cnt >= log_flush_cnt then begin
      log_cnt := 0;
      flush c
    end

| _ -> ()

let close () = match !channel with
  | Some c -> close_out c; channel := None
  | _      -> ()

let () = at_exit close


