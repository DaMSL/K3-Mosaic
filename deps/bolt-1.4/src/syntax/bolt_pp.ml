(*
 * This file is part of Bolt.
 * Copyright (C) 2009-2012 Xavier Clerc.
 *
 * Bolt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Bolt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


module Args = struct
  let level = ref 5

  let logger = ref ""

  let for_pack = ref ""
end

let logger_name modname =
  if !Args.logger <> "" then
    !Args.logger
  else if !Args.for_pack <> "" then
    !Args.for_pack ^ "." ^ modname
  else
    modname

let module_of_file file =
  let basename = Filename.basename file in
  String.capitalize (try Filename.chop_extension basename with _ -> basename)

module Make (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  open Camlp4.Sig
  include Syntax

  let level_of_string x = (* re-defined here to avoid dependency *)
    let _loc = Loc.ghost in
    match String.uppercase x with
    | "FATAL" -> <:expr< Bolt.Level.FATAL >>, 0
    | "ERROR" -> <:expr< Bolt.Level.ERROR >>, 1
    | "WARN" -> <:expr< Bolt.Level.WARN >>, 2
    | "INFO" -> <:expr< Bolt.Level.INFO >>, 3
    | "DEBUG" -> <:expr< Bolt.Level.DEBUG >>, 4
    | "TRACE" -> <:expr< Bolt.Level.TRACE >>, 5
    | _ -> failwith (Printf.sprintf "invalid logging level %S" x)

  type attributes = {
      error : Ast.expr option;
      properties : Ast.expr option;
      name : string option;
    }

  let no_attribute = { error = None; properties = None; name = None; }

  let merge x y =
    let m n x y = match x, y with
    | Some _, Some _ -> failwith (n ^ " attribute defined twice")
    | Some _, _ -> x
    | _, Some _ -> y
    | _ -> None in
    { error = m "'exception'" x.error y.error;
      properties = m "'properties'" x.properties y.properties;
      name = m "'name'" x.name y.name; }

  let location loc attrs =
    let file = Loc.file_name loc in
    let logger = match attrs.name with
    | Some s -> s
    | None -> logger_name (module_of_file file) in
    let line = string_of_int (Loc.start_line loc) in
    let column = string_of_int ((Loc.start_off loc) - (Loc.start_bol loc)) in
    logger, file, line, column

  let open_bolt _loc e =
    <:expr< (let open Bolt in $e$) >>

  let printf_expr _loc e =
    let error () =
      raise (Stream.Error "constant string or module item expected after \"LOG\"") in
    match e with
    | Ast.ExApp _ ->
        let rec insert x = function
          | Ast.ExApp (loc, e1, e2) ->
              Ast.ExApp (loc, (insert x e1), e2)
          | v -> Ast.ExApp (Ast.loc_of_expr v, x, v) in
        insert (<:expr< Printf.sprintf >>) e
    | Ast.ExStr _ -> e
    | Ast.ExId (_, id) ->
        let rec check_id last = function
          | Ast.IdAcc (_, e1, e2) ->
              check_id false e1;
              check_id true e2
          | Ast.IdLid _ ->
              if not last then error ()
          | Ast.IdUid _ ->
              if last then error ()
          | _ -> error () in
        check_id true id;
        open_bolt _loc e
    | Ast.ExAcc (_, _, _) -> open_bolt _loc e
    | _ -> error ()

  EXTEND Gram
    GLOBAL: expr;
    attr: [[ ["EXCEPTION" | "EXN"]; e = expr ->
               { no_attribute with error = Some e }
           | "NAME"; s = STRING ->
               { no_attribute with name = Some s }
           | ["PROPERTIES" | "WITH"]; e = expr ->
               { no_attribute with properties = Some e } ]];
    expr: LEVEL "simple" [[ "LOG"; e = expr; l = LIST0 attr; "LEVEL"; lvl = UIDENT ->
          let attrs = List.fold_left merge no_attribute l in
          let level_code, level_value = level_of_string lvl in
          if level_value <= !Args.level then begin
            let logger, file, line, column = location _loc attrs in
            let properties = match attrs.properties with
            | Some x -> open_bolt _loc x
            | None -> <:expr< [] >> in
            let error = match attrs.error with
            | Some x -> <:expr< Some $x$ >>
            | None -> <:expr< None >> in
            let msg = printf_expr _loc e in
            <:expr< Bolt.Logger.log
              $str:logger$
              $level_code$
              ~file:$str:file$
              ~line:$int:line$
              ~column:$int:column$
              ~properties:$properties$
              ~error:$error$
              ($msg$) >>
          end else
            <:expr< () >>
      ]];
  END
end

let add_prepare si =
  if !Args.level >= 0 then begin
    let open Camlp4.PreCast in
    let loc = Ast.loc_of_str_item si in
    let file = Loc.file_name loc in
    let logger = logger_name (module_of_file file) in
    let _loc = Loc.ghost in
    let s = <:str_item< let () = Bolt.Logger.prepare $str:logger$ >> in
    Ast.StSem (Loc.ghost, s, si)
  end else
    si

let () =
  let open Camlp4 in
  let module Id = struct let name = "Bolt" let version = "1.1" end in
  let module M = Register.OCamlSyntaxExtension (Id) (Make) in
  PreCast.AstFilters.register_str_item_filter add_prepare;
  let levels = [
    "NONE", -1;
    "FATAL", 0;
    "ERROR", 1;
    "WARN", 2;
    "INFO", 3;
    "DEBUG", 4;
    "TRACE", 5
  ] in
  Options.add
    "-level"
    (Arg.Symbol ((List.map fst levels), (fun s -> Args.level := List.assoc s levels)))
    "<level>  Set the logging level";
  Options.add "-logger" (Arg.Set_string Args.logger) "<name>  Set the logger name";
  Options.add "-for-pack" (Arg.Set_string Args.for_pack) "<prefix>  Set the prefix for logger names"
