open Format
open Lazy

(* Pretty printing helpers *)
type cut_type = NoCut | CutHint | CutLine

let formatter = ref (stdbuf, str_formatter);;
 
let ob () = pp_open_hovbox (snd !formatter) 2
let cb () = pp_close_box (snd !formatter) ()
let pc () = pp_print_cut (snd !formatter) ()
let pb i = pp_print_break (snd !formatter) 0 i
let ps s = pp_print_string (snd !formatter) s
let psp () = pp_print_space (snd !formatter) ()
let fnl () = pp_force_newline (snd !formatter) ()

let cut c = match c with
  | NoCut -> ()
  | CutHint -> pc ()
  | CutLine -> fnl ()

let ps_list ?(sep=", ") cut_t f l =
  let n = List.length l in
  ignore(List.fold_left
    (fun cnt e ->
      f e;
      (if cnt < n then ps sep);
      (if cnt < n then cut cut_t);
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

let wrap_formatter ?(fresh=true) print_fn =
  let print () =
	  pp_set_margin (snd !formatter) 300;
	  print_fn ();
	  pp_print_flush (snd !formatter) ();
    let r = Buffer.contents (fst !formatter)
    in Buffer.clear (fst !formatter); r
  in
  if fresh then
    let buffer = Buffer.create 300 in
    let saved_formatter = !formatter in
    formatter := (buffer, formatter_of_buffer buffer);
    let r = print ()
    in formatter := saved_formatter; r
  
  else print()
