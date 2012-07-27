open Format
open Lazy

(* Pretty printing helpers *)
type cut_type = NoCut | Hint | Line
 
let ob () = pp_open_hovbox str_formatter 2
let cb () = pp_close_box str_formatter ()
let pc () = pp_print_cut str_formatter ()
let ps s = pp_print_string str_formatter s
let psp () = pp_print_space str_formatter ()
let fnl () = pp_force_newline str_formatter ()

let cut c = match c with
  | NoCut -> ()
  | Hint -> pc ()
  | Line -> fnl ()

let ps_list ?(sep=", ") cut_t f l =
  let n = List.length l in
  ignore(List.fold_left
    (fun cnt e ->
      f e;
      (if cnt < n then ps sep);
      cut cut_t;
      cnt+1)
    1 l)

let pretty_tag_term_str t = ob(); ps t; cb()

let pretty_tag_str ?(lb="(") ?(rb=")") ?(sep=", ") cut_t extra t ch_lazy_t =
  begin
    ob();
    ps (t ^ lb); cut cut_t;
    if extra = "" then () else (ps (extra ^ sep); cut cut_t);
    ps_list ~sep:sep cut_t force ch_lazy_t;
    ps rb;
    cb()
  end

let wrap_formatter print_fn =
  pp_set_margin str_formatter 120;
  print_fn ();
  flush_str_formatter ()
